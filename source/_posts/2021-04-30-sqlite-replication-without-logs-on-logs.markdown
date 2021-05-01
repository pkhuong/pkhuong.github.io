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

There are downsides to this approach, compared to [shipping sqlite's WAL](https://litestream.io/).  I can think of these three:

1. Writing to a db without using our replicating VFS will break
   everything
2. Write transactions must fit in RAM.
3. The classic rollback mode does not support concurrent readers
   while a write is in flight.

We can fix #1 by using a special on-disk format that will break the
regular OS VFS, so that's not too much of an issue.  I'm not sure what
I think of #2.  In practice, most sqlite *databases* comfortably fit
in RAM...

The last point is more subtle than it looks, because the WAL's mode
support for readers while a write is in flight is not great: sqlite's
[WAL file can grow without bounds as long as a read transaction is open](https://www.sqlite.org/cgi/src/doc/wal2/doc/wal2.md).
This failure mode can be surprising in production, and is particularly
annoying when one independently ships a journal to persistent remote
storage.

Concurrent reads, at least on a given replica, are also irrelevant for
the use case I'm interested in.  Backtrace's backend uses sqlite to
store configuration data, and caches the database's contents as
heap-allocated objects.  Concurrent readers access the latter
in-memory object, which are synchronised more loosely and efficiently
than sqlite.  Our primary reason for journaling writes to sqlite is
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
