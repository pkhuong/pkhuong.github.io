---
layout: post
title: "Appending to a log: an introduction to the Linux dark arts"
date: 2021-01-22 22:47:33 -0500
draft: true
hidden: true
comments: true
categories: 
---

Appending to a log file can be a surprisingly complex affair on Linux,
especially at high volumes.  I'm not an I/O person, it's always just
been part of my yak shaving stack; over the years, I nevertheless
managed to learn a few things and find some hidden gems on LKML.
Every time I have to really write to a log, I end up re-googling for
this stuff. No more. I'll write it all down here; I hope it's helpful,
if not for you, then for me.

Again, I'm not that interested in I/O, and only aim for decent
performance in a good state, and something that doesn't spiral awfully
out of control in extreme situations.  For peak I/O performance, I'd
read what the people at scylla and vectorized have to say, or maybe
https://github.com/glommer and https://github.com/axboe.

TL;DR: If you want to know the best way to write to a ton of data
really fast, ask someone else.  I can only help you avoid a couple
mistakes with [a bit of simple code](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h).

Yes, `O_APPEND` is atomic
-------------------------

Let's start by assuming 
[log records are encoded in a self-synchronising format](/Blog/2021/01/11/stuff-your-logs/),
with readers that detect corruption and handle duplicate data.

Given that, we only need our writes to a log file to be atomic on
success.  When a write fails, all that matters is that we can detect
the failure (e.g., short write), and try again.  We get that with
POSIX `O_APPEND` semantics,[^NFS-go-home] even when appending to a
log file (inode) from multiple threads or processes, via any number
of file objects.

[^NFS-go-home]: Not on NFS. If you have to use NFS, I can't help you.

[The spec is clear](https://pubs.opengroup.org/onlinepubs/9699919799/functions/write.html):
"If the O_APPEND flag of the file status flags is set, the file offset
shall be set to the end of the file prior to each write and no
intervening file modification operation shall occur between changing
the file offset and the write operation."

And, of course, a write is a modification operation.

You will however find plenty of FUD from people who test with buffered
files in scripting languages, or in the shell.  Obviously, the
kernel's atomicity guarantees are irrelevant when a runtime system in
userspace buffers writes and flushes them whenever it wants.  The
situation with the shell is more subtle: the shell doesn't buffer, but
does use pipes, so large writes are split whenever the intermediate
pipe fills up.

It turns out that there is a size limit when writing to regular files
on Linux, but it's ridiculously high: 0x7ffff000 bytes (slightly less
than 2GB).  Larger writes aren't split either, they just fail with
a short write.

If you need atomicity guarantees, which you do for concurrent logging
to the same file, use `O_APPEND` *and raw write syscalls*; this
lets the kernel handle locking for you, as long as you don't try to
write 2GB of data in one syscall.

It definitely makes sense to buffer to coalesce syscalls and improve
write throughput, but you probably can't rely on your runtime system's
generic buffered I/O: the flushing policy must not split a record in
half.

You might also wonder about retrying whenever a write is "short."  In
practice, short writes to local filesystems only happen in "sticky"
situations like writing 2GB at once, or running out of disk.  It makes
sense to quickly error out of a function that's supposed to write to
disk when that disk is full.

Finally, you can share the same file descriptor across threads (or
`dup(2)`ed descriptors for the same file object across multiple
processes), but only if you don't want to know the file offset of your
newly-written data.  There's unfortunately no Linux syscall to
atomically append data and return the offset where the data was
written.  If you have exclusive ownership *over the file description*
(not the file descriptor, but the underlying file description which
refer to an actual file), you can `lseek(2)` after the write, and know
that no one touched the offset in your private file object.

See [`write_and_retry` in this gist](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L129-L164)
for an example.

Pace yourself
-------------

Writes have the fun property of not needing to wait for I/O devices
before returning: unless you explicitly ask for synchronous I/O, the
kernel can simply copy the write to an internal buffer and immediately
return success.

This makes for great microbenchmark performance, but also means that
the machine will start acting completely differently once a program's
sustained write rate exceeds the hardware's write bandwidth.  When
that happens, buffers fill up, and the kernel eventually triggers
emergency flushes to disk, which leads to a storm of randomly
scheduled I/O requests and an unresponsive machine.

Typically, this means that your system works fine under expected load,
but craps out catastrophically as soon the write load exceeds a
certain threshold.

The fix is, unsurprisingly, to introduce backpressure by capping the
amount of buffering, i.e., by bounding the size of the write queue to
much less than the available RAM.  The implementation is however not
obvious.

We'll use
[sync_file_range(2)](https://man7.org/linux/man-pages/man2/sync_file_range.2.html),
a system call that allegedly "is extremely dangerous and should not be
used in portable programs."

The syscall isn't dangerous, it's merely misnamed.  Linus Torvalds
shows how the syscall
[makes sense in a blocked write loop](https://lore.kernel.org/lkml/CA+55aFxHt8q8+jQDuoaK=hObX+73iSBTa4bBWodCX3s-y4Q1GQ@mail.gmail.com/).

The idea is to first explicitly define a queue size per file; just
enough to saturate the storage devices, without flooding them with a
ton of tiny writes.  Back in the mid 2010s, I found that 16-32MB was
more than enough to saturate a pair of RAID-0 SSDs... and, the more
files you have, or the lower the I/O devices' bandwidth-delay product,
the less buffering each file needs.

Linus's code regularly schedule newly written data for flushing with
`sync_file_range(fd, flush_begin, flush_size, SYNC_FILE_RANGE_WRITE);`.
This first call should *usually* return immediately, and merely marks the
`flush_size` bytes starting at `flush_offset` as waiting to be written out.

The next call, `sync_file_range` with
`SYNC_FILE_RANGE_WAIT_BEFORE|SYNC_FILE_RANGE_WRITE|SYNC_FILE_RANGE_WAIT_AFTER`,
*will* sleep and slow the caller down, but only if the `flush_size`
bytes starting at `flush_begin - queue_size` are still buffered and
waiting to be written out.  When the call returns, we know that only
the `queue_size` bytes between `flush_begin + flush_size - queue_size`
and `flush_begin + queue_size` may still be buffered.

In a very roundabout way, `sync_file_range` lets us ask the kernel for
backpressure on each file's queued up writes!

How does that work when logging records of arbitrary size?

First, it's easy to kill your write throughput when flushing right up
to EOF, when the file's size isn't round: `sync_file_range` doesn't know
if it wants to be a durability or a buffer management syscall, so it
will trigger a writeout for the last partial page, and then overwrite
it again when we append more data and trigger another flush.

This didn't used to be so bad on spinning rust, but contemporary
storage media (SSDs, NVMes, SMR drives... anything but persistent
memory) really don't like partial overwrites.[^bad-ssd]

[^bad-ssd]: A couple years ago, this effect was particularly visible on cheaper "enterprise" SSDs, which did offer high read and write throughput, but did not overprovision enough for the background garbage collection to keep up and remain unobstrusive.  We'd observe decent sustained throughput for a while, and eventually an abrupt drop to the overwrite throughput actually sustainable once data has to be erased.

We'll want to make sure we round the end of the flush range *down* to
some generous alignment, e.g., 2 MB.

You can picture this process as moving three cursor close to the end
of the file

    ... data bytes .... bytes ......
       ^                 ^         ^
       |                 |         |
    flushed           flushing    write

The `write` head is the regular `O_APPEND` write cursor, which is
always repositioned to the end of the file.  Everything to the left of
the `flushing` cursor has been marked as ready for write-out; we don't
need explicit synchronisation between writers because marking for
flushing is idempotent (if the bytes are already marked for write-out,
it's obviously a no-op, but it's also a no-op if the bytes have
already been written out).  Finally, when writers execute the flushing
logic, they wait until all data to the left of the `flushed` cursor
has been written to disk.

The distance between the `write` and the `flushed` cursors is the
amount of buffering we allow per file.  In our implementation, we
instead maintain a tight bound on the distance between the `flushed`
and `flushing` cursors, and allow the distance between `flushing` and
`write` to grow for performance reasons (e.g., to make sure the
`flushing` head is always aligned).

Again, [there's an implementation in this gist](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L166-L244).

When to perform regular maintenance
-----------------------------------

When do we invoke this logic to mark new data as ready for write-out
and wait for older data to have been written out?

At first sight, it makes sense to just wait until the file's write
pointer crosses, e.g., a 2 MB boundary.  However, this complicates the
retry loop in `append_to_log_file`, and increases the likelihood that
flush calls for multiple files will synchronise.  For example, if we
shard perfectly to 100 log files, we'll expect the files to grow at
the same rate, and thus cross 2 MB boundaries at the same time.  The
result will be periodic bursts of flush requests, when we'd prefer a
steady rate of flush requests.

My solution is to avoid shared per-file state with probabilistic
counting.  Let's say we want to keep the buffer for each file to
approximately 32 MB.  We could have each writer thread generate a
"bytes written until flush" countdown value by sampling from an
Exponential distribution with mean 128 KB, and decrement that by the
bytes actually written after each call to `append_to_log_file`.
Whenever the countdown reaches 0 or negative, we call the flush
routine for the file we just wrote to.

This probabilistic scheme is equivalent to triggering a flush with
probability \\(2^{-17}\\) for each byte written.  The probability is
memoryless, independent of all other writes, so it doesn't matter that
we apply the countdown per writer thread; it's the same probability
for every byte written to a file.  The distribution of bytes written
between flushes has a long tail, but it's not *that* bad: the
probability of failing to flush after 4 MB is 1.3e-14.

The probabilistic trigger trivially scales to multiple writers without
any coordination, and jitters away potential synchronised behaviour
between files that happent to grow at the same rate.

In fact, since the exponential is memoryless, we can adjust the sample
rate dynamically, and maintain a "countdown" that's sampled from the
standard Exponential with mean 1.  Rather than pre-scaling that to the
expected flush period, we can divide the number of bytes written by
that period, and subtract that scaled value from the countdown.

The sampling logic, including a PRNG, [takes less than 100 LOC](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L373-L466),
and the code to 
[count down and trigger maintenance is nothing special](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L589-L608).

Dropping old data
-----------------

Rotating log files is a classic solution to dropping old data.
However, in a multithreaded (or multiprocess!) setting, this rare
operations introduces complexity to every log call, if one doesn't
want data to show up grossly out of order, or even disappear.  It also
becomes very complex when the rotation logic must live outside the
producer, e.g., when we want to make sure a consumer reads all the log
data before deleting it.

There's a simpler option nowadays.  Unix has long supported sparse
files, file with empty byte ranges created by seeking around before
writing actual data.  We can now also sparsify files after the fact 
[by "punching holes"](https://man7.org/linux/man-pages/man2/fallocate.2.html#DESCRIPTION:~:text=Deallocating%20file%20space)
over preexisting data.

When the hole is suitably aligned, the operation only involves
metadata, and doesn't have to overwrite any data; if the hole isn't
aligned, everything still works fine, but the filesystem has to write
actual zeros around for the misaligned slop, and that's not ideal for
performance or disk utilisation.

That's nice because we'll reclaim useless storage without affecting
any file offset or renaming files.  This means we can apply retention
rules from any writer, without any coordination... in fact, we could
even defer to readers and let them apply more complex retention rules,
e.g., deleting after a transaction commit, or removing records based
on real time.

There's a really good match between our use case and the `fallocate`
flags, so 
[the code mostly has to figure out what range of file offsets we want to erase](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L246-L309).

Shrinking large sparse files
----------------------------

Removing old data by punching holes works nicely on filesystems with
reasonable (\\(2^{63}\\) bytes) limits on file sizes, like XFS or ZFS.

Unfortunately, ext4 isn't part of that club, and craps out at
\\(2^{44}\\) bytes.  That's not the limit of a file's footprint, but
on its size in metadata... and that's a problem for us, because we
want to keep appending to log files, while regularly punching away old
data to keep their physical footprint in check.

In 2014, Linux gained another fallocate mode,
`FALLOC_FL_COLLAPSE_RANGE`, targeted at DVR and video editing devices.
From a filesystem's point of view, these devices constantly write to
log files, and sometimes want to shrink logs.  That seems similar to
our problem.

It's sad that we have to shrink because, until now, all our
maintenance operations were idempotent on a range of file bytes: once
we punch a hole over a given range, we can punch the resulting empty
range again, and the result is still an empty range.

Collapsing doesn't work like that.  If we collapse away the first 1MB
of a 10 MB file, we're left with a 9 MB file that has a range of bytes
in its first 1 MB... so two consecutive calls to collapse away the
first 1 MB of a 10 MB file are *not* idempotent, and yield an 8 MB file.

Thankfully, Dave Chinner gave us an out when 
[he argued that out-of-bounds collapse calls should be rejected](https://lwn.net/Articles/589309/)
rather than converted to `ftruncate(fd, 0)`.

We do not rely on file collapse to control our logs' physical
footprint: that's what hole punching is for.  We instead only need to
collapse ranges to avoid running into ext4's 16 TB limit on the file
size.  That gives us a lot of freedom.

Let's collapse at coarse 1 TB granularity, and essentially treat that
terabyte range like a 40 bit counter that will wraparound, but is
large enough to avoid ABA races in practice (because it takes a lot of
real time to write 1 TB to a file).

Concretely, we make collapse calls *idempotent enough* by only issuing
them when the file is large enough that the collapsed range is at
least 1 TB larger than the size of the resulting shrunk file.

For example, we could wait until the file size is 4 TB (and completely
sparse except for the tail end).  We can collapse away the first 3 TB
chunks, since we know there's no data in there.  If the collapse goes
through, the result will be a 1 TB file, maybe a bit longer if some
data was logged.  A second collapse call to collapse away the first 3
TB will now fail, since the file doesn't have 3 TBs to collapse!

Remember that we also tried to keep the range of
`FALLOC_FL_PUNCH_HOLE` calls within the last TB of data in the file
(and maybe a bit more for alignment).  We'll always issue collapse
calls that remove at least 2 TB, i.e., the end of the file moves by at
least 2 TB.  If a hole-punching call were to lose a race with a
collapse (for example, at the end of a 4 TB file), it would end up
erasing data past the end of the (now less than 2 TB) file, a no-op
that certainly will not erase fresh data.

Collapsing at such a coarse granularity means it only happens rarely;
that's important for writers, because collapse can be slow, but also
for readers.

Whenever we shrink a log file, readers will detect that they're past
EOF, and will have to relocate their read cursor.  It's actually not
hard to do this correctly, since we know the read cursor's new
positions hasn't changed modulo 1 TB, but it is a slow error-handling
path.  Similarly, we can fix up pointers into a file with modular
arithmetic: while we can't know for sure where it lies in the new
file, we can figure out the only matching offset that has not yet been
erased... and regular error handling will take care of the case where
we did not find the data we were looking for.

We can also make sure shrinking happens rarely after startup by
shrinking more aggressively when we open the log file (e.g., whenever
its size is greater than 4 TB), and only shrink log files that have
reached 5 or 6 TB when at steady state.

Of course, this collapse approach doesn't work if you actually want to
keep 1 TB of log data.  One could increase the chunk size... However,
given that requirement, I would probably require a filesystem that
supports 64-bit file sizes (e.g., XFS or ZFS), and disable file
shrinking.

Again, there's a pretty good fit between our use case and the one the
flags were defined for, so the bulk of the work is
[figuring out what, if anything, we want to collapse away](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L311-L371).

That's all I got!
-----------------

You now know all the tricks I've learned about logging persistent data
on Linux.  The result a simple logging scheme that pushes all locking
and other complexity to the kernel, but still supports important use
cases like multi-threaded and multi-process writes, log rotation, and
even consumer-triggered rotation.

If you need high throughput with a lot of small writes, you definitely
need to add a buffering layer on top.  However, I've done pretty well
with raw `write(2)` per record, e.g., for a write-ahead log of large
and business-critical POST requests.

This [`log_file_append.h` gist](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h)
is my first time combining all the tricks presener earlier in a
coherent interface. I hope it's useful.

<p><hr style="width: 50%"></p>
