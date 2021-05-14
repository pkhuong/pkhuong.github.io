---
layout: post
title: "Sqlite replication without logs on logs"
date: 2021-04-30 22:07:54 -0400
hidden: true
draft: true
comments: true
categories: 
---

When we use sqlite on a log-structured file system like ZFS, BTRFS, or
F2FS that's backed by a solid state drive, what actually happens is:
sqlite emulates atomic file updates on top of POSIX file operations
with a journal (undo or write-ahead log, for sqlite), the filesystem
implements these in-places updates by appending to a log with a bunch
of random-access writes that happen to be mostly sequential (in order
to minimise write amplifications), and the SSD itself relies on a
translation layer to implement that theoretically arbitrary write
patterns as batched writes to the actual Flash physical medium.

This [log stacking antipattern](https://www.usenix.org/conference/inflow14/workshop-program/presentation/yang)
increases the write pressure on the final Flash chips, and
generally wastes performance potential.

Obviously, this is an interface design problem: both the filesystem
and the storage device expose interfaces that simultaneously do not
match the best way to work with the underlying implementation, and
does not expose useful functionality afforded by that
implementation.[^expressive-power] This happens a lot with storage,
and it's not a bad approach: storage moves slowly because losing data
can be catastrophic and both filesystems and devices stick around for
a long time, so it's better to make a new filesystem or storage medium
present the same interface as older implementations, and only think
about extending or replacing that legacy interface once the new
implementations have proven themselves in production.  There's also a
history of exciting interfaces that end up overfitting on contemporary
designs, and become obsolete after a couple years.  The storage
community has learned to be conservative with interfaces across layers
(application, filesystem, device), and I think that's a reasonable
approach.

[^expressive-power]: Random access to arbitrary sectors is a minimal interface to implement everything else, but it doesn't let the filesystem inform the device of its goal with all these writes, so the end result is less than ideal.

In 2017, the F2FS (flash-friendly) filesystem introduced such 
[an extension for applications that need atomic batch updates](http://esos.hanyang.ac.kr/files/publication/conferences/international/HPCCT2016-108_final.pdf),
e.g., Android apps that store data with sqlite.  Using this extension in
sqlite's [classic rollback journal mode](https://sqlite.org/atomiccommit.html)
resulted in *higher performance than even the WAL mode*, as
long as the rollback journal remained in memory.  That was good enough
for upstream sqlite to widen its VFS interface with markers
that bracket [atomic batches of writes](https://www.sqlite.org/c3ref/c_fcntl_begin_atomic_write.html#sqlitefcntlbeginatomicwrite).

When the VFS promises it can commit *batches* of writes atomically,
sqlite's rollback-based (non-WAL) commit logic will not persist
a rollback journal to disk, and instead commit its buffered updates
as a single atomic batch.

One way to implement that functionality is to actually use a
filesystem that exposes transaction fcntls.  Another option would be
to emulate transactions with a temporary (unlinked) file and
[copy-on-write reflinks](https://lwn.net/Kernel/Index/#reflink).

However, there's an approach that extends the range of applications
that might use sqlite: with asynchronous streaming replication, we can
provide durability for write batches by sending them over the network.
Building on top of sqlite's native support for VFS write transactions
makes our lives simpler (just write a shim VFS), and means sqlite
won't waste time maintaining its own journal.  Asynchronous replication
gets us the ability to fail-over from one designated active node to
another without blocking (most) writes.

In fact, we could probably even support
[cross-data base transactions](https://sqlite.org/lockingv3.html),
unlike sqlite's native WAL mode, by waiting for the writer to
release locks to find quiescent points.

There are downsides to this approach, compared to [shipping sqlite's WAL](https://litestream.io/).
There's an obvious trade-off regarding reliability and invasiveness:
reading the WAL file from the outside is much less likely to break the
main DB, but parsing the WAL to find new frames might result in broken
recovery logs, and relying on sqlite's database open logic to apply
WAL segments makes it harder to implement live read replication.
Apart from that, I can think of three definite downsides:

1. Writing to a db without using our replicating VFS will break
   everything
2. Write transactions must fit in RAM
3. The classic rollback mode does not support concurrent readers
   while a write is in flight

We can fix #1 by using a special on-disk format that will break the
regular OS VFS, so that's not too much of an issue.  I'm not sure what
I think of #2.  In practice, most sqlite *databases* comfortably fit
in RAM...

The last point is more subtle than it looks, because the WAL mode's
support for reads while a write is in flight is not great: sqlite's
[WAL file can grow without bounds as long as a read transaction is open](https://www.sqlite.org/cgi/src/doc/wal2/doc/wal2.md).
This behaviour can be surprising in production, and is particularly
annoying when one independently ships a journal to persistent remote
storage.

Concurrent reads, at least on the write leader, are also irrelevant for
the use case I'm interested in.  Backtrace's backend uses sqlite to
store configuration data, and caches the database's contents as
heap-allocated objects.  Concurrent readers access the latter
in-memory object, which are synchronised more loosely and efficiently
than sqlite.  Our primary reason for journaling sqlite writes is
fail-over; secondarily, we would like remote read-only replicas, but
these concurrent readers wouldn't be visible to the writer's sqlite.

In fact, if I did need concurrent reads, I'd probably consider a
dedicated write connection with concurrent reads service from
read-only replicas backed by the replicating VFS's journal.

Allowing multiple threads to try and write to sqlite is also a bad
idea: while multiple write transactions may be open at the same time,
only the fastest one can actually commit writes, and all others will
fail (fast) to upgrade, due to the potential write conflict. We can
avoid that by opening all write transactions in `IMMEDIATE` or
`EXCLUSIVE` mode, but I think it makes more sense to handle write
ownership in the application, with a dedicated write connection, if
not a dedicated write thread.  Handling write exclusion at the
connection level also makes it easier to eventually support replicated
writes from multiple machines (only one connection can hold the
networked write lock at any one time).

If we service read-only connections from read-only snapshots of the
database, the data will eventually grow stale.  However, opening and
closing a connection to a sqlite database is *fast* (just some local
filesystem operations, except for potential hiccups in WAL mode), so
connection pools aren't that useful.  This makes it practical to
regularly a new connection with a fresh replica, e.g., whenever a
worker services a read-only request.  That gives us the same guarantee
as WAL readers that open a transaction for each request.

Implementation plan
-------------------

Let's start with the simple case of single-threaded access to one
database (no cross-database transaction) with streaming replication:
that suffices for a high availability backend for a low-traffic
configuration sqlite database.

We can create a shim VFS that delegates everything to the regular OS
VFS.  We know we only need to replicate the main database file, since
the rollback journal is replaced by VFS-level transactions.  However,
we still have to handle regular writes to the main database file that
aren't bracketed by batch begin/commit/abort markers.

We'll handle these regular writes as trivial atomic batches of one,
and inform sqlite that individual writes of any size are atomic.

When committing an atomic batch (trivial or otherwise), we can either
first perform the writes to the database file, and then log them
asynchronously, or log the writes before actually updating the local
snapshot.

We're using streaming replication because we're OK with losing recent
data in the event of a crash, as long as we can find a valid snapshot,
so I think it's fine to write first.  If we have to roll back or
there's a crash in the middle of a write transaction, we'll just have
to replay the replication stream.  These should be exceedingly rare
events, so it makes sense to optimise for the case when the commit
succeeds.

Since we write before logging, we can use the local database file as a
buffer for the replication stream, and only send byte to the
replication code.  However, this also means that we mustn't overwrite
pages that are still being used as buffers.  We can start by blocking
when in `BEGIN_ATOMIC_WRITE` until all data has been read from the database
file into the replication stream.  Later, maybe we'll want to be smarter
about write sets and block in individual writes.

TL;DR: we can implement plain physical replication in a VFS, and have
sqlite inform the VFS of transaction boundaries instead of wasting IO
on journaling.

Simplifications: "All writes to the database file are an integer number of pages and are page-aligned." https://sqlite.org/lpc2019/doc/trunk/briefing.md

Ref https://www.sqlite.org/src/doc/trunk/src/test_demovfs.c

Locking: I think/hope we can avoid the complexity of sqlite's file locks
with open file description (OFD) locks.

Test plan
---------

We'll add our VFS file(s) to the amalgamated source, by:

1. copying the file(s) to the build directory's `tsrc/` subdir
2. `sed -e 's|   stmt\.c|   stmt.c replvfs.c|' -i ../tool/mksqlite3c.tcl`
3. `make OPTS="-DSQLITE_EXTRA_INIT=sqlite3_replvfs_init" test`.
4. Configure without WAL or mmap.

See main.c for the meaning of `SQLITE_EXTRA_INIT`.  We'll use that to
register our VFS as the new default.  We can also name it "unix" to
shadow the regular system "unix" VFS (some tests refer to it by name).

Representation of streamed updates
----------------------------------

We'll track changes with aligned 64KB pages.  The replication data
will live in two buckets:

1. a content-addressed bucket, where all writes are blind overwrites.
2. a location-addressed bucket, with names that correspond to hosts & dbs

The bulk of the data will live in the content-addressed bucket, which
should probably have a retention policy setup.  The name of each blob
in this content-addressed bucket is the hex representation of the 128-bit
umash fingerprint of the blob's uncompressed contents.  The contents
stored in S3 are zstd-compressed.

The location-addressed bucket will contain little data, and should
probably be versioned.  Each location-addressed blob is named after a
machine and database, and only contains references to blobs in the
content-addressed bucket.  Location-addressed blobs will be
overwritten in-place, so the bucket should probably be versioned (with
a retention policy on the versions).

Our 64 KB tracking granularity is independent from the DB's page size
(B-tree node size).  However, it would be ideal to set the page size
to 64KB or more.  For journalled (non-WAL) DBs, the page size can be
overridden with [a pragma](https://www.sqlite.org/pragma.html#pragma_page_size)
followed by a vacuum.

Defining `SQLITE_FAST_SECURE_DELETE` at build-time should also improve
compressibility without affecting the size of changes sent to s3.

Hooking into sqlite
-------------------

We want to make sure the streaming replication doesn't make things
worse, and we currently don't have any problem with write latency.
Our databases also top out at < 1MB.

When a file acquires the write lock, we will execute writes directly.
However, we will also log these writes to the replication queue as
they come.  Before releasing the write lock, we will signal a publish
barrier to the queue: only then is it safe to update the replicated
file.

When the database's page size is smaller than our 64 KB tracking
granularity, we will have to backfill surrounding bytes.  We'll do
so by waiting until the end of the transaction to read from the
actual file.  Of course, it's preferable to avoid this extra I/O,
so setting the DB's page size to 64 KB is best.

We'll also want to make sqlite use a growth "chunk" size of ~1MB.
That's roughly the size of our prod DBs, results in a negligible
16 extra pages of metadata, and should help FSes do something smart.

We will also want to publish full snapshots from time to time...  but
we only want to do so once any hot journal has been rolled back.
That's why we must wait for a write lock to be downgraded, or a shared
lock to be released: sqlite always checks for hot journals when
opening a shared lock, so once the shared lock is released, any hot
journal must have been applied (or there was nothing to apply).  If
there is a hot journal, it will be applied with the write lock taken,
so everything must be good to go once the write lock is downgraded.

We also don't want to do this too frequently.  I think it makes sense
to try and publish a full snapshot at most once per connection, and
only if the db hasn't been snapshotted in more than, e.g., one hour
(track that with an xattr).  We should also add a free snapshot per
boot, in order to catch OS crashes (again, xattr, this time on linux
bootid / mtime of pid1, salted + hashed).

Note that we will also want to build an internal snapshot around our
first write to the DB.  This snapshot doesn't have to be published to
S3, but it is needed for the replication subsystem to construct a 
correct map of the file.  We can do that with the reader lock held.
It's OK, if not ideal, if multiple threads feed snapshots to the
replication subsystem: since they hold the reader lock, the snapshots
must be identical, so we don't have to worry about ordering.

When the file does change, the writer will first acquire the write lock,
so no snapshot will be in progress.

Replication subsystem
---------------------

Each writer is responsible for sending its write transaction as it
goes, and for signaling the end of the transaction.  The data in the
replication queue still makes sense, because writers exclude each
other.

Naively, the replication subsystem can send each page to the
content-addressed bucket as it reads them from the queue, and update
its internal mapping.  When it sees a publish/end of transaction
record, it can send the updated mapping to the location-addressed
bucket.

The content-addressed blobs will be named after a umash fingerprint
of their data, and will contain a zstd-compressed version of these
data.  We will also set the [content type](https://tools.ietf.org/id/draft-kucherawy-dispatch-zstd-00.html#rfc.section.3.1)
accordingly.

The location-addressed blobs will contain a protobuf-encoded
directory.  At first, I'm thinking a proto with a size field and 
a raw "bytes" field.  The second field contains the directory
as a flat array of concatenated 16-byte chunk ids.  These blobs
too will be zstd-compressed.

While the above works, it can result in more S3 operations than we
would like.

We will try to buffer writes, until we run into an internal memory
allocation limit: we expect repeated writes to, e.g., btree root
pages, so we will buffer writes for up to, e.g., 5 minutes after the
corresponding publish fence, and never publish overwritten pages to S3.

Internal buffering works with a mapping from page id to content.

The content will always include the umash fingerprint of the page, and
may include buffered contents, not yet sent to S3.  It will also
include a flag to denote whether the contents have made it to S3 or
not.

The mapping will always contain a consistent (safely published) view;
in-flight updates will be buffered in a separate identical data
structure until we see a barrier.

Overwriting the consistent view will naturally drop overwritten
buffered writes.

Since we don't assume that updates are buffered, and the
content-addressed bucket can contain arbitrary blobs, we
also get to flush buffered pages whenever we want.

We simply have to make sure all the pages have been flushed to S3
before uploading the new location-addressed mapping blob.

We will also only attempt to flush the current mapping once it has
been more than k (e.g., 5) minutes since an update.  As a special
case, we will also publish snapshots (with page contents sync'ed to
S3) immediately.

We'll have a single replication subsystem for all file objects, and
for all database files.  File objects will send refcount updates, and
everything will be keyed on normalised file paths (or a fingerprint
thereof).

HA
--

Once we have logging, let's add an option to reconstruct a file from
scratch.  We'll be able to test the logic by snapshotting a file,
constructing a copy, and looking for differences.

After that, we'll want to allow read-only connections that poll S3 for
new mapping files regularly, whenever sqlite acquires the read lock.

The chunks downloaded from S3 can first live in memory, and later in a
cache directory (nuked after every reboot).

Much like the write end, the mapping doesn't have to assume chunks are
available: in the worst case, we'll fetch missing chunks from S3.

We may also want to improve latency by letting clients directly
request the latest map file sent to S3.  This will let clients hit
coronerd more frequently than we'd like to hit S3 (remember, costs are
per API call).

<p><hr style="width: 50%" /></p>
