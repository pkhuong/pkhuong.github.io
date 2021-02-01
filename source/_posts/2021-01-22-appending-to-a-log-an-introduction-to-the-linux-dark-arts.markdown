---
layout: post
title: "Appending to a log: an introduction to the Linux dark arts"
date: 2021-01-22 22:48:33 -0500
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
mistakes with [a bit of simple code](/images/2021-01-22-appending-to-a-log-an-introduction-to-the-linux-dark-arts/log_file_append.html).[^howto-generate]

[^howto-generate]: This HTML file was generated from [the C header file](/images/2021-01-22-appending-to-a-log-an-introduction-to-the-linux-dark-arts/log_file_append.h) with [an awk script](/images/2021-01-22-appending-to-a-log-an-introduction-to-the-linux-dark-arts/code2md.awk) driven by [this script](/images/2021-01-22-appending-to-a-log-an-introduction-to-the-linux-dark-arts/generate_html_doc.sh) and with [a mangled version of modest.css](/images/2021-01-22-appending-to-a-log-an-introduction-to-the-linux-dark-arts/modest.css).

Here's an overview of the baseline I'll present:

1. Let the kernel order writes with `O_APPEND`, and handle any (rare)
   issue on the read side, by making sure the data is self-synchronising.
2. Use `fdatasync` to keep a tight leash on the kernel's write buffering,
   while making sure to align write-out requests to avoid partial blocks.
3. Trigger periodic maintenance, including `fdatasync`, with
   memoryless probabilistic counting.
4. Erase old data by punching holes, again, with alignment to avoid
   zero-ing out partial blocks.

That's enough for wait-free writes and maintenance, and lock-free
reads; moreover, readers only have to spin when they fell behind the
periodic deletion of old data, and that's usually a bigger problem
than the lack of progress guarantees.

Unfortunately, some filesystems and many standard unix utilities do
not deal well with large sparse files.  For example, `ext4` caps the
logical size of each file to 16 TB, even for files that contain much
fewer physical bytes.  On the utility front, `rsync` uses regular
reads for all those zeros instead of `lseek`ing over holes.

That's why I also describe "backward" compatible mode, which adds a
fifth component: use file locks around maintenance (including hole
punching), and periodically collapse the file in fixed (e.g., 1 GB)
increments.

Maintenance is usually optional, so writes and periodic maintenance
are still wait-free: if the maintenance file lock is already taken, we
can assume another thread or process will eventually clean things up
for us.  Only when writes fail (potentially because of `ENOSPC` or
`EFBIG`) do we block on mandatory maintenance.

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

See [the `write_and_retry` function in this gist](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L458-L493)
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
memory) really don't like partial overwrites.[^bad-ssd] Arguably, CoW
filesystems don't have that problem: they instead copy partially
overwritten blocks in software.  TL;DR: no one likes to flush partial
blocks.

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

Again, [there's an implementation in this gist](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L495-L631).

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

The sampling logic, including a PRNG, [takes less than 100 LOC](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L330-L443),
and the code to 
[count down and trigger maintenance is nothing special](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L1044-L1065).

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
[the code mostly has to figure out what range of file offsets we want to erase](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L652-L722).

Shrinking large sparse files
----------------------------

Removing old data by punching holes works nicely on filesystems with
reasonable (\\(2^{63}\\) bytes) limits on file sizes, like XFS or ZFS.

It also has the nice property of being idempotent, and that the file
offset always grows monotonically.  This monotonicity makes is easy
for readers to just keep pumping more bytes (until they fall behind),
and also means that concurrent maintenance isn't an issue.

Unfortunately, ext4 isn't part of that club, and craps out at
\\(2^{44}\\) bytes.  That's not the limit of a file's footprint, but
on its size in metadata... and that's a problem for us, because we
want to keep appending to log files, while regularly punching away old
data to keep their physical footprint in check.

Moreover, a lot of standard Unix utilities (e.g., rsync) don't handle
large sparse files very well: they'll just keep reading terabytes of
zeros without attempting to skip holes.

In 2014, Linux gained another fallocate mode,
`FALLOC_FL_COLLAPSE_RANGE`, targeted at DVR and video editing devices.
From a filesystem's point of view, these devices constantly write to
log files, and sometimes want to shrink logs.  That seems similar to
our problem.

That's a niche operation: as of January 2021, only ext4 and XFS
support `FL_COLLAPSE_RANGE`.  However, of all the mainstream
filesystems on Linux (btrfs, ext4, xfs, and arguably zfs), ext4 is the
only one that needs shrinking: all other filesystems handle logical
file sizes in the exabytes (\\(2^{60}\\) bytes).  So it's really not a
problem that these filesystems can't shrink files, because they never
*need* to shrink either.

It's sad that we have to shrink because, until now, all our
maintenance operations were idempotent on a range of file bytes: once
we punch a hole over a given range, we can punch the resulting empty
range again, and the result is still an empty range.

Collapsing doesn't work like that.  If we collapse away the first 1MB
of a 10 MB file, we're left with a 9 MB file that has a range of bytes
in its first 1 MB... so two consecutive calls to collapse away the
first 1 MB of a 10 MB file are *not* idempotent, and yield an 8 MB file.

There is a way to make collapse work without locks, but, for that
trick to be safe, we need to collapse in huge increments (e.g.,
multiples of 1 TB, and more than 2TB).  That doesn't play well with
standard utilities.

We'll instead use [file locks](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L214-L328)...
but only with non-blocking `trylock` calls during steady state.

When we do decide to shrink a log file, we'll do so in multiples
of a fixed granularity (e.g., 1 GB).

This known granularity let readers relocate their read cursor when
they detect that they're past EOF.  It's actually not hard to do this
correctly, since we know the read cursor's new positions hasn't
changed modulo 1 GB (for example), but it is a slow error-handling
path.  When that happens, we can fix up pointers into a file with
modular arithmetic: while we can't know for sure where it lies in the
new file, we can figure out the only matching offset that has not yet
been erased... and regular error handling will take care of the case
where we did not find the data we were looking for.

Again, there's a pretty good fit between our use case and the one the
flags were defined for, so, after acquiring a lock on the log file,
the bulk of the work is 
[figuring out what, if anything, we want to collapse away](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L724-L809).

On the read side
----------------

Let's start by assuming the reader keeps up with the writer: there's
always going to be fun error handling code when a reader tries to read
data that's been rotated away.

At some point, the reader will want to block until there is more data
to read.  Reads from regular files don't block, so they will have to
use a filesystem watch facility like `inotify`... but at least there's
only one log file to watch.

Once new data comes in, the reader can simply issue `read(2)` calls
normally.  Very rarely, the reader will notice that its `read` call
returns 0 bytes: the file offset is at *or after* EOF.  The reader
should then `lseek` or `fstat` to figure out the file's current
logical size.  If the current offset `cur` is indeed after EOF, the
file was shrunk by collapsing a couple 1 TB chunks.  We don't know how
many, but that's fine because we also assume that any data lives in
the last TB of the file.  We can use a bit of modular arithmetic to
find the last offset in the file that is equivalent to `cur` modulo 1
TB.

Once we have that offset, the reader should also `lseek` with
`SEEK_DATA` to make sure there is actual data to read: if there isn't,
the reader definitely fell behind.

When a reader falls behind, it can't do much except seek to the first
chunk of data in the file (`SEEK_DATA` from offset 0), and start
reading from that point.  This will probably yield one or more partial
records, but that's why we use a self-synchronising format with error
detection.

That's a lot of complexity compared to log segment files (?)
------------------------------------------------------------

On the read side, it's actually not clear that the logic is actually
more complicated than for segments files.

For one, the `inotify` story to block until more data is written
becomes much more complex with segments: readers must monitor the
parent directory, and not just a single specific file.  That's a
problem when we want to store multiple log files in the same
directory, and some logs are more active than others...  I suppose one
could fix that with a subdirectory per log.

When the reader is up to date, they still need to know when it's time
to switch to a new segment; merely reaching the end of the current
segment doesn't suffice, since more data could still be written.
Maybe we can rely on writers to leave a special "end of segment"
marker; that sounds like the best case for readers.

When a reader falls behind, catching up now requires listing a
directory's contents to figure out where to resume reading.

This can be fixed with a dedicated metadata file that contains two
counters (trailing and leading segment ids), similar to what
[jlog](https://github.com/omniti-labs/jlog) does.

I think the difference on the write-side is more striking.

The write / retention policy protocol I described earlier is wait-free
for regular writes, from the perspective of userspace... and even the
locking component mostly uses non-blocking trylock. Obviously, there
is still coordination overhead and the syscalls are internally
implemented with locks, but the kernel can hopefully be trusted to
avoid preemption while holding locks.  What wait-freedom gives us is a
guarantee that unlucky scheduling of a few userspace threads will
never block other log writers.  This is a similar situation to atomic
instructions: internally, chips implement these instructions with a
locking protocol, but we assume that hardware critical sections don't
run forever.

When we split our data across multiple files, we would have to build
on top of the only lock-free primitive exposed by Linux filesystems:
sticky bits in the form of `link`ing / `renameat`ing files in a
directory.

In order to maintain the wait-free (or even mere lock-free) property
of our write protocol, we would have to keep monotonic state in the
form of a tombstone for every single segment file that ever existed in
our log (or, at least, enough that races are unlikely).

Alternatively, we could `mmap` a metadata file read-write and use atomic
operations.  However, that writable metadata memory map becomes a new
single point of catastrophic corruption, and that's not necessarily
what you want in a disaster recovery mechanism.

Either way, writers must also agree when it's time to switch to a new
segment.  The most robust way is probably to detect when writes went
over the segment size limit after the fact.  Unfortunately, that
approach can result in a burst of wasted I/O whenever we switch to a
new segment.  If we want to reliably direct writes on the first go, we
need to lock in userspace around writes... an easy way to wreck our
write throughput under high load.

That's all I got!
-----------------

You now know all the tricks (if you remember to use 
[double checked locking when it makes senses](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h-L822-L893))
I've learned about logging persistent data on Linux.  The result a
simple logging scheme that pushes most of the coordination to
the kernel, but still supports important use cases like multi-threaded
and multi-process writes, log rotation, and even consumer-triggered
rotation.

If you need high throughput with a lot of small writes, you definitely
need to add a buffering layer on top.  However, I've done pretty well
with raw `write(2)` per record, e.g., for a write-ahead log of large
and business-critical POST requests.

If you really want peak write performance, you just read the wrong
document.  This is more about seeing how much we can wring out of
standard-ish POSIX interfaces, without trying to replace the kernel.

I find this is a recurring theme in my work: in order to achieve peak
$whatever, we have to relinquish all of our tools and rebuild a
kernel, compiler, etc. from scratch.  In many cases, it's more
interesting to ask how far we can go without doing so, than to figure
out what we need to give up in order to close the last couple
percentage points.  There's a reason Lent is only 40 days out of the year.

This [`log_file_append.h` gist](https://gist.github.com/pkhuong/7a1aeb5ad0ef24299c5117f5f1310a38#file-log_file_append-h)
is my first time combining all the tricks I presented here in a
coherent interface. I hope it's useful.

<p><hr style="width: 50%"></p>
