---
layout: post
title: "Stuff your logs!"
date: 2021-01-08 13:11:23 -0500
hidden: true
draft: true
comments: true
categories: 
---

Having more than satisfied the [rule of three](https://wiki.c2.com/?RuleOfThree),
I feel comfortable claiming that one's representation of first resort
for binary logs (write-ahead, recovery, replay, or anything else that
may be consumed by a program) should use a
[self-synchronising code](https://en.wikipedia.org/wiki/Self-synchronizing_code).

At [Backtrace](https://www.backtrace.io), we've settled on a variant of 
[consistent overhead byte stuffing (COBS)](http://www.stuartcheshire.org/papers/COBSforToN.pdf)
that improves encoding and decoding speed at the expense of increased
buffering requirements (up to 64KB instead of 254 bytes).  We target
servers and always access full records at a time, so this downside is
a non-issue for us.

I'll describe the format later in this post.  But first, why do I
make that claim?

Why is self-synchronisation important?
--------------------------------------

Logs are never more useful than when things go wrong, so it's crucial
that they be robust and simple to write.  Of course, the storage layer
should detect and correct errors, but things will sometimes fall
through, especially for on-premises software, where no one fully
controls deployments.  When that happens, we should aim for graceful
partial failure, rather than losing all data in a file whenever one of
its pages goes to the great bit bucket in the sky.

[Self-synchronising encodings](https://en.wikipedia.org/wiki/Self-synchronizing_code)
like [COBS](https://en.wikipedia.org/wiki/Consistent_Overhead_Byte_Stuffing)
can not only recover from overwritten data (e.g., when a network
switch flips a bit, or weak error handling in a filesystem results
in a silently zero-filled page), but also from garbage inserted in
the middle of a stream, and even from vanishing bytes (e.g., a
couple bytes lost to buggy ad hoc backup logic).

A COBS encoding for log records achieves all that by unambiguously
separating records with a reserved byte (e.g., 0), and encoding each
record to avoid that separator byte.  A reader can thus assume that
potential records starts and end at a log file's first and last bytes,
and otherwise look for separator bytes to determine where to cut
records.  When bytes are lost, inserted, or overwritten, the only
records affected are those that immediately surround the corruption:
readers resynchronise as soon as they find a valid separator byte in
the encoded stream.

This encoding does not reliably detect invalid records; it only
guarantees that readers can always identify where valid records begin
and end, for records that aren't corrupt nor next to corrupt
(missing, inserted, or overwritten) data.  Corruption (e.g., a
duplicated page) may lead readers to hallucinate correctly framed
data; higher-level code should compare checksums once potential
records are identified, and the log processing logic itself should
handle duplicate valid records.

On the write side, the encoding logic is simple (a couple dozen lines
of C code), and uses a predictable amount of space, as expected from
an algorithm suitable for microcontrollers.

Actually writing encoded data is also easy:
on POSIX filesystems, we can make sure each record is delimited (e.g.,
prefixed with the delimiter byte), and issue a
[regular `O_APPEND` write(2)](https://pubs.opengroup.org/onlinepubs/007904875/functions/write.html).
Vectored writes can even insert delimiters without copying in
userspace.

When a write errors out, we can blindly (maybe once or twice) try
again: the encoding is independent of the output file's state.  When a
write is cut short, we can still issue the same[^suffix] write call,
without trying to "fix" the short write: the encoding and the
read-side logic already protect against that kind of corruption.

[^suffix]: If you append with the delimiter, it probably makes sense to special-case short writes and also prepend with the delimiter after failures, in order to help readers resynchronise.

What if multiple threads or processes write to the same log file?
When we [open with `O_APPEND`](https://pubs.opengroup.org/onlinepubs/007904875/functions/open.html),
the operating system can handle the rest.  This doesn't make
contention disappear, but at least we're not adding a bottleneck in
userspace on top of what is necessary to append to the same file.

This simplicity also plays well  with
[high-throughput I/O primitives like `io_uring`](https://kernel.dk/io_uring.pdf), and with 
[blob stores that support appends](https://docs.microsoft.com/en-us/rest/api/storageservices/append-block):
independent workers can concurrently queue up blind append requests and
retry on failure.
There's no need for application-level mutual exclusion or rollback.

Fun tricks with robust readers
------------------------------

Our log encoding lets readers recover from bad bytes; we also assume
that readers reject invalid records, and that the log processing logic
handles duplicate records.  These are table stakes for a reliable log
consumer.

What I find interesting is that we can turn this robustness into
new capabilities.

It's often useful to process the tail of a log at a regular cadence.
For example, I once maintained a system that regularly tailed hourly
logs to update approximate views. One could support that use case with
length footers... however, with COBS framing, we can also start
scanning for a valid record from an arbitrary byte location, and read
the rest of the data normally.

When logs grow large enough, we want to process them in parallel.  The
standard solution is to shard log streams, but that unfortunately
couples the parallelisation and storage strategies, and
adds complexity to the write side.

With COBS framing, we can partition a data file arbitrarily (e.g., in
fixed size chunks) for independent workers.  A worker will scan for
the first valid record starting inside its assigned chunk, and handle
every record that *starts* in its chunk. Filtering on the start byte
means that a worker may read past the logical end of its chunk, in order
to fully read the last record that starts in the chunk: that's how we
unambiguously assign a worker to every record, including records that
straddle chunk boundaries.

Random access even lets us implement a form of binary or interpolation
search on raw unindexed logs, when we know the records are (k-)sorted
on the search key!

Eventually, we might also want to truncate our logs.

Contemporary filesystems like XFS (and even Ext4) support large sparse
files.  For example, sparse files can reach \\(2^{63} - 1\\) bytes on
XFS with a minimal metadata-only footprint: the on-disk data for such
sparse files is only allocated when we issue actual writes.  Nowadays,
we can [sparsify files after the fact](https://lwn.net/Articles/415889/),
and convert ranges of non-zero data into zero-filled "holes" in order
to release storage without messing with file offsets
(or even atomically
[collapse old data away](https://lwn.net/Articles/589260/)).
Filesystems can only execute these operations at coarse granularities,
but that's not an issue for our readers: they must merely remember to
skip sparse holes, and the decoding loop will naturally handle any
garbage partial record left behind.

[Backtrace](https://www.backtrace.io)'s word stuffing variant
-------------------------------------------------------------

[Cheshire and Baker's original byte stuffing scheme](http://www.stuartcheshire.org/papers/COBSforToN.pdf)
targets small machines and slow transports (amateur radio and
phone lines).  That's why it bounds the amount of buffering needed to
254 bytes for writers and 9 bits of state for readers, and attempts to
minimise space overhead, beyond its worst-case bound of 0.4%.

The algorithm is also reasonable. A writer buffers data until it
encounters a forbidden 0 "stuff" byte (a delimiter byte), or there are 254
bytes of buffered data.  Whenever a writer stops buffering, it outputs a
block, whose contents are described by its first byte.  If the
writer stopped buffering because it found a "stuff" byte, it emits one
byte with `buffer_size + 1` before writing and clearing the
buffer.  Otherwise, it outputs 255 (one more than the buffer size),
followed by the buffer's contents.  On the read side, we know that the
first byte we read describes the current block of data (255 means
254 bytes of literal data, any other value is one more than the number
of literal bytes to copy, followed by a "stuff" byte).  We denote the
end of a record with an implicit delimiter: when we run out of data to
decode, we should have just decoded an extra delimiter byte that's not
really part of the data.

With framing, an encoded record surrounded by delimiters thus looks like the following

```
|0   |blen|(blen - 1) literal data bytes....|blen|literal data bytes ...|0    |
```

The delimiting "0" bytes are optional at the beginning and end of a
file, and each `blen` size prefix is one byte with value in
\\([1, 255]\\).  A value \\(\mathtt{blen} \in [1, 254]\\) represents
a block \\(\mathtt{blen} - 1\\) literal bytes, followed by an implicit
"stuff" 0 byte.  If we instead have \\(\mathtt{blen} = 255\\), we
have a block of \\(254\\) bytes, without any implicit byte.  Readers
only need to remember how many bytes remain until the end of the
current block (one byte for a counter), and whether they should insert
an implicit stuff byte before decoding the next block (one binary flag).

We have different goals for the software we write at [Backtrace](https://www.backtrace.io).
For our logging use cases, we pass around fully constructed records,
and we want to issue a single atomic write syscall per record.
Buffering is baked in, so there's no point in making sure we can work
with a small write buffer.  We also don't care as much about the space
overhead (the worst-case bound is already pretty good) as much as we
do about encoding and decoding speed.

These different design goals lead us to an updated hybrid word/byte
stuffing scheme.

In the past, I've seen
["word" stuffing schemes](https://issues.apache.org/jira/browse/AVRO-27)
aim to reduce the runtime overhead of COBS codecs by scaling up
the COBS loops to work on two or four bytes at a time.  However,
a byte search is trivial to vectorise, and there is no guarantee that
frameshift corruption will be aligned to word boundaries (for example,
POSIX allows short writes of an arbitrary number of bytes).

Our hybrid word-stuffing instead looks for a forbidden two-byte
delimiter sequence at arbitrary byte offsets.  We must still
conceptually process bytes one at a time, but we can now encode
wider spans of literal data (nearly up to \\(2^{16} = 64\textrm{KB}\\)),
and that's a win when encoding larger records: short variable-length
`memcpy` aren't great.

Delimiting with a pair of bytes instead of with a single byte also makes it
easier to carefully select a delimiter that's unlikely to appear in our data.
Cheshire and Baker do the opposite, and use a frequent byte (0) to
eliminate the space overhead in the common case.  We care a lot more
about encoding and decoding speed, so an unlikely delimiter makes more
sense for us.  We picked `0xfe 0xfd` because that sequence doesn't
appear in small integers (unsigned, two's complement, varint, single
or double float) regardless of endianness, nor in valid UTF-8 data.

With a 2-byte forbidden sequence, we can encode the size of each block
in radix 253 (`0xfd`), which goes up to \\(253^2 - 1 = 64008\\).
That's a reasonable size for `memcpy`.  The radix conversion replaces
the off-by-one weirdness in COBS: that part of the original algorithm
actually encodes values in \\([0, 254]\\) into one byte while avoiding
the 0 stuff byte.

A two-byte size prefix is a bit ridiculous for small records (our
records tend to be on the order of 30-50 bytes). We thus encode the
first block specially, with a single radix-253 byte for the size
prefix.  Since the stuff sequence `0xfe 0xfd` is unlikely to appear in
our data, the encoding for short record often boils down to adding a
byte-sized length prefix.

A framed encoded record now looks like
```
|0xfe|0xfd|blen|blen literal bytes...|blen_1|blen_2|literal bytes...|0xfe|0xfd|
```

The first `blen` is in \\([0, 252]\\) and tells us how many literal
bytes follow in the initial block.  If the initial \\(\mathtt{blen} =
252\\), the literal bytes are immediately followed by the next block's
decoded contents.  Otherwise, we must first append an implicit `0xfe
0xfd` stuff sequence... which may be the artificial stuff sequence we
virtually append at the end of every record before encoding.

Every subsequent block comes with a two-byte size prefix, in little-endian
radix-253.  In other words, `|blen_1|blen_2|` represents the
block size \\(\mathtt{blen}\sb{1} + 253 \cdot \mathtt{blen}\sb{2}\\), where
\\(\mathtt{blen}_{\{1, 2\}} \in [0, 252]\\).  Again, if the block
size is the maximum encodable value, \\(253^2 - 1 = 64008\\), we
have literal data followed by the next block; otherwise, we must
append a `0xfe 0xfd` stuff sequence to the decoded output before
moving on to the next block.

The encoding algorithm is only slightly more complex than for the
original COBS scheme.

Assume the data to encode is suffixed with an artificial 2-byte stuff
sequence `0xfe 0xfd`.

For the first block, look for the stuff sequence in the first 252
bytes.  If we find it, emit its position (must be less than 251) in
one byte, then all the data bytes up to but not including the stuff
sequenceg, and resume regular encoding after the stuff sequence.  If
the sequence isn't in the first block, emit `252`, followed
by 252 bytes of data, and resume regular encoding after those bytes.

For regular (all but the first) blocks, look for the stuff sequence in
the next 64008 bytes.  If we find it, emit the sequence's byte offset
(must be less than 64008) in little-endian radix 253, followed by the
data up to but not including the stuff sequence, and resume encoding
after the sequence.  If we don't find the stuff sequence, emit 64008
in radix 253 (`0xfc 0xfc`), copy the next 64008 bytes of data, and
resume encoding immediately after the data.

Remember that we conceptually padded the data with a stuff sequence at
the end.  This means we'll always observe that we fully consumed the
input data at a block boundary.  When we encode the block for the
final stuff sequence, we stop (and append a stuff sequence to
delimit the end of the record).

You can find our [MIT-licensed implementation in this gist](https://gist.github.com/pkhuong/79054e80d672a8c23d8005ee0aa352c9#file-word_stuff-c).

When encoding short records, we already noted that the encoding step
is often equivalent to adding a one-byte size prefix.  In fact, we can
encode and decode all records of size up to \\(252 + 64008 = 64260\\)
bytes in place, and only ever have to slide the initial 252-byte
block: whenever a block is shorter than the maximum length (252 bytes
for the first block, 64008 for subsequent ones), that's
because we found a stuff sequence in the decoded data.  We can replace
that stuff sequence with a size header when encoding, and undo the
operation when decoding.

Our code does not implement these optimisations because encoding and
decoding stuffed bytes aren't bottlenecks for our use cases, but it's
good to know that we're nowhere near the performance ceiling.

A resilient record stream on top of word stuffing
-------------------------------------------------

The stuffing scheme only provides resilient framing.  That's
essential, but not enough for a record abstraction.  At the very
least, we need checksums in order to detect invalid records that
happen to be correctly encoded (e.g., when a non-stuff literal byte is
overwritten).

Our pre-stuffed records start with the little-endian header

```
struct record_header {
        uint32_t crc;
        uint32_t generation;
};
```

where `crc` is the `crc32c` of whole record, including the
header,[^initialize-your-crc] and the `generation` is a yet-unused
arbitrary 32-bit payload that we added for forward compatibility.
There is no size field: the framing already handles that.

[^initialize-your-crc]: We overwrite the `crc` field with `UINT32_MAX` before computing a checksum for the header and its trailing data.  It's important to avoid zero prefixes because the result of crc-ing a 0 byte into a 0 state is... 0.

The remaining bytes in a record are an arbitrary payload.  We use
protobuf messages to help with schema evolution (and keep messages small
and flat for decoding performance), but there's no special coupling
between the stream of word-stuffed records and the payload's 
format.

[Our MIT-licensed implementation](https://gist.github.com/pkhuong/79054e80d672a8c23d8005ee0aa352c9#file-record_stream-c)
let writers output to buffered `FILE` streams, or directly to file descriptors.

Buffered streams offer higher write throughput, but are only safe
when the caller handles synchronisation and flushing; we use them
as part of a commit protocol that
[fsync](https://pubs.opengroup.org/onlinepubs/009695399/functions/fsync.html)s
and publishes files with
[atomic `rename` syscalls](https://pubs.opengroup.org/onlinepubs/009695399/functions/rename.html).
During normal operations, we instead write to file descriptors opened with
`O_APPEND` and a background fsync worker: in practice, the operating
system is more stable than our software, so it's more important that
encoded records immediately make it to the kernel than all the way to
persistent storage.

For readers, we can either read from a buffer, or `mmap` in a file,
and read from the resulting buffer.  While we expose a linear iterator
interface, we can also override the start and stop byte offset of an
iterator; we use that capability to replay logs in parallel.  Finally,
when readers advance an iterator, they can choose to receive a raw data
buffer, or have it decoded with a protobuf message descriptor.

What's next?
------------

We have happily been using this log format for more than nine months
to store metadata records that we replay every time the
[Backtrace](https://www.backtrace.io) server restarts.

Decoupling writes from the parallel read strategy let us improve our
startup time incrementally, without any hard migration.  Serialising
with a real schema language (protocol buffers) also made it easier to
start small and slowly add optional metadata, before forcing a hard
switch-over.

This piece-meal approach let us transition from a length-prefixed data
format to one where all important metadata lives in a resilient record
stream, without breaking backward compatibility.  We slowly added more
metadata to records and eventually parallelised loading from the
metadata record stream, all while preserving backward and forward
compatibility.  Finally, six months after the initial rollout, we
flipped the switch and made the new, more robust, format mandatory;
the old length-prefixed data files still exist, but are now treated as
bags of arbitrary checksummed data bytes, with all the control
metadata in record streams.

By now, we've had a respectable amount of pleasant operational
experience with the format, and, while performance is more than good
enough for us (the parallel loading phase is currently dominated by
disk I/O and parsing in `protobuf-c`), we also know there's plenty of
headroom: our records are short enough that they can usually be
decoded without any write, and always in place, by replacing every
2-byte block length prefix with the stuff sequence.

We're now starting laying the groundwork to distribute our single-node
embedded database and making it interact more fluently with other data
stores.  The first step will be generating a
[change data capture stream](https://materialize.com/change-data-capture-part-1/),
and re-using the word-stuffed record format was an obvious choice.

Word stuffing is simple, efficient, and robust.  If you can't just
defer to a real database (maybe you're trying to write one yourself)
for your data stream, give it a shot!  Feel free to
[try our MIT-licensed code](https://gist.github.com/pkhuong/79054e80d672a8c23d8005ee0aa352c9#file-license)
if you don't want to write your own.

<p><hr style="width: 50%"></p>
