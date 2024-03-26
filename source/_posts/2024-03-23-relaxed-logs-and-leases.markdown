---
layout: post
title: "Relaxed logs and strict leases: a sturdy foundation?"
date: 2024-03-23 13:52:00 -0400
comments: true
published: true
hidden: true
draft: true
categories: 
---

A few years ago, a friend wondered whether we could emulate something like compare-and-swap on blob stores, now that [S3 offers strong read-after-write consistency](https://aws.amazon.com/blogs/aws/amazon-s3-update-strong-read-after-write-consistency/).
It would be great to offload more coordination to scalable and highly available storage, but it turns out that not providing powerful synchronisation primitives tends to be a key part of achieving scale and reliability.[^other-clouds]

[^other-clouds]: Runner-ups don't have that luxury, so both Azure blob storage and Google cloud storage natively offer conditional PUTs.

I've been coming back to that question because I work at a place where there's a mandatory dependency on a reasonably fast and consistent clustered file system, and because something about [Helland's BIG DEAL position paper](https://www.cidrdb.org/cidr2024/papers/p63-helland.pdf) feels right to me.
I now believe we can decouple coordination from storage, and thus synchronise writes without complicating the storage layer.

The [wait-free hierarchy](https://www.cs.yale.edu/homes/aspnes/pinewiki/WaitFreeHierarchy.html) introduced in [Herlihy's "Wait-free synchronization" (1991)](https://dl.acm.org/doi/10.1145/114005.102808) tells us we shouldn't expect a straightforward solution: atomic registers (like plain blobs) have a consensus number of 1, so can't provide a wait-free compare-and-swap even for 2 processes.[^dekker]
We could hack something up with versioned blobs: use each blob as a operations log, let each version corresponds to a position in the log, push store descriptors for conditional operations to the log, and apply the conditional operations in log order.

That doesn't contradict Herlihy's wait-free hierarchy because a log is even stronger than a queue with peek at the next output, and a queue with peek has infinite consensus number.
Cute hack, but probably not a great idea; the S3 user guide mentions that [blobs with millions of versions can cause a high rate of 503 errors](https://docs.aws.amazon.com/AmazonS3/latest/userguide/troubleshooting-versioning.html#performance-degradation), so I guess someone tried it for real, with an unsurprising outcome.

[^dekker]: Dekker's algorithm (and other classic mutual exclusion algorithms) show that we can use O(n) sequentially consistent atomic register to implement a lock for n processes. That's different than a wait-free synchronisation primitive because locks are allowed (expected!) to get stuck when the current holder never releases the lock (e.g., the process crashed).

The fundamental challenge with logs is making sure everyone observes the same total order for all messages (all log entries).
That's what makes it hard to scale logs to high rates of concurrent accesses, and the usual workarounds (e.g., batching, or sharding across multiple logs) simplify the problem by coarsening or relaxing the total order (e.g., messages don't interleave across batches, and we have a total order for all entries in a given shard, but not across shards).

In other words, we make logs scale by making them weaker coordination primitives... bad news for those of us who want to coordinate processes with logs.

I think I've found a pattern that works with dumb storage, where we rely on the application correctly interacting with a [lease](https://en.wikipedia.org/wiki/Lease_(computer_science)) service to handle most of the coordination,
and only fall back to log order for the rare cases where leases expire before being released, i.e., only when processes crash or get stuck.

Of course, this doesn't help the worst case, so it's useless in theory.
In practice, crashes are relatively rare (otherwise you have a bigger problem), and I'd bet that the complexity of handling hard cases efficiently tends to significantly increases the prevalence of bugs that necessitate handling.
The happy path also avoids a common downside of time-based coordination protocols: the commit rate is independent of the bound on clock error.
Instead, we bottleneck commits on a new global lease service.

Leases for the happy path, partial log order in the worst case
==============================================================

Assume loosely synchronised clocks and a lease system that works with absolute timestamps
(i.e, one can reserve a domain until a specific absolute deadline, rather than for a given duration).
Whenever an application process writes a message, it includes a timestamp that will be used to deliver messages in the correct order;
that timestamp must be earlier than the expiration time on the associated lease(s).
Processes must acquire leases correctly to prevent conflicting operations from executing at the same time,
and we'll assume that, on the usual happy path, processes release or renew leaves long (several seconds) before expiry.

These leases do not necessarily map to any concrete object in persistent storage.
Rather, they simply represent temporarily exclusive ownership of an application-level logical domain.
For example, a multi-tenant application might acquire leases on a tenant granularity, but comingle together persistent data for multiple small tenants.

On the happy path, processes always release leases gracefully.
Before releasing a lease, a process must also publishes an upper bound for the highest timestamp on any message it wrote;
that timestamp watermark must be less than its leases' expiration time.
When a process acquires a set of leases, it can take the maximum timestamp watermark associated with any of its new leases' most recent holders, and make sure to only write messages with an even higher timestamp.

On the happy path again, the storage system is thus only responsible for storing a bag of messages.
Readers can gather all the messages and order them by timestamp.[^inefficient]
That ordering isn't total, and messages can become visible out of order, so message delivery isn't globally consistent.
It does however captures all logical dependencies, as long as the application uses leases correctly:
a pair of potentially conflicting operations must acquire overlapping sets of leases,
so the intersecting leases guarantee that any messages sent by the second operation must have timestamps strictly greater than all messages sent by the first.
Leases should only be released once the associated writes are globally visible, so that also takes care of out-of-order visibility.

[^inefficient]: Of course, reading everything all the time isn't super efficient, but it's conceptually correct. We can easily come up with some sort of idempotent summarisation protocol for practical performance.

Unlike solutions that solely rely on bounded clock error (in the style of Spanner), the commit rate on the happy path doesn't slow down when the clock error bounds widen: the lease service guarantees monotonic message timestamps.
Commits are instead bottlenecked on the lease service.
Even with leases, large *actual* clock errors could delay commits, since it makes sense to pause for a little while instead of writing messages with timestamps far in the future.
Robust implementations should use something like [Roughtime](https://blog.cloudflare.com/roughtime) to statelessly reject messages with incongruous timestamps.

But what about about the sad path, when a lease expires?
--------------------------------------------------------

The key challenge about lease expiry is asynchronicity:
a message could be constructed well before the lease's deadline, but only become globally visible after the deadline.
That's a really hard hole to plug efficiently, but we expect such late messages to happen rarely, only when a lease expires without being released.
This means we can favour performance in the common case over responsiveness when handling expirations, and thus use coarse timestamps, which opens up simple solutions.

If we had scalable multi-writer logs, we could shard out writes to a lot of logs,
and have a metronome process periodically (e.g., every second or half-second) write a timestamp to each log.
The metronome's periodic heartbeat messages lets us weakly correlate real time with logical position in every log shard.
Messages will sometimes be delayed, so we could, for example, reject messages with a timestamp more than 1 second older than the preceding metronome hearbeat.
There's nothing bad about duplicate metronome messages, so a process that's blocked on an expired lease can also emit its own heartbeats.

That's a toy example of the capability we're looking for,
but we want to target weak storage systems that do not natively expose logs.
We'll instead assume the storage system can give us multi-writer / multi-reader *bags* (unordered lists) of (logs of) messages,
and that we can come up with a wait-free protocol to close a bag to further writes, i.e., make a bag read-only;
bags are enough because messages provide their own logical ordering, with embedded timestamps.

Given such closable bags, we can steer each message to a bag based on its timestamp (e.g., one bag for each aligned second since epoch),
and close bags when they're clearly in the past (e.g., close the bag for [10:00:01 AM, 10:00:02 AM) after 10:00:04 AM).
We assume a wait-free closing protocol, so processes can always kick off that transition instead of blocking on an expired lease.

On a POSIX-ish filesystem, this case corresponds to a [maildir-style organisation](https://en.wikipedia.org/wiki/Maildir) where each bag is a different directory, and we can stop writes to a directory with `chmod`.
Closing bags after a 2-second grace period means that, even with 100 ms of clock error, producers will usually not see their writes rejected unless something went wrong.

On a filesystem, the metadata overhead from all these small files might cause issues.
We can instead have a closable bag of closable single-writer logs.
The core idea is that we can close a bag of logs by writing a lock file that lists all the logs that were found at closing time, along with their size.
I'll flesh that out in the [appendix](#appendix)

A similar approach also works for maildir-style bags of messages, when we can't easily make the maildir read-only (e.g., the maildir is actually a path prefix in S3).
We close a maildir by publishing a list of of all valid items in the maildir in a lock blob.
A versioned blob gets us first-write-wins semantics for the lock blob, as long as readers remember to fetch the contents of the first version.

These approaches all have the same goal:
to let external processes periodically provide a frame of reference to roughly map between real time (which drives lease expiration), and position in the log of messages.
We can hope to achieve better scaling than regular logs because there is no storage-level ordering within a bag of messages (and only within each single-writer log, for bags of logs).
The external ordering provided by the messages' timestamps lets us recover logical ordering in the common case, when everything is fine.
When something does go wrong, the slow lease expiration means that we only need a rough order between a time after which a lease has clearly expired, and all the messages in a bag.
That's a coarse partial order, so hopefully that doesn't turn into an unexpected coordination bottleneck,
but suffices to let everyone agree on what messages must be rejected because they became visible after their lease expired.
When processes do encounter an expired lease, they can either wait a little or directly help mark the passage of time, then resume normal operations a few seconds after the expiration time.

This is obviously similar to approaches based on clock error bounds (e.g., Spanner) or on deterministically ordering message delivery for all messages in an epoch, once the epoch is closed.
However, the similarities are only material *when a lease expired before being released*, i.e., when something went wrong.
Only in such error cases will we see processes wait for enough time to elapse that in-flight messages become either visible or rejected.
The blast radius of such pauses is also constrained to the expired leases;
processes that do not attempt to acquire expired leases remain on the happy path.
This makes me hope that we can configure conservative timeouts without worrying too much about their impact on performance.

Is it really easier to manage leases than storage?
==================================================

The reason we can decouple the normal and worst-case commit latencies is the addition of a lease service:
commits are only fast when managing leases is fast, and leases are gracefully released before they expire.
Once a lease is allowed to expire, our conservative grace periods mean that we'll have to wait for a few seconds before assuming that the process that lost the lease will see its writes fail.

This of course raises an important question: is it actually easier to implement leases than persistent logs?

I believe the answer is yes, simply because leases are small and have a short lifetime.

The (meta)data associated with leases is small (dozens of bytes), so we can hope to fit everything in RAM, which makes it much easier to achieve decent performance than if the data were stored on persistent storage, even NVMe SSDs.

The short lifetimes simplify crash recovery.
Let's say leases can be held for at most 10 seconds.
This upper bound means that we can always recover from a crash by waiting for a little over 10 seconds before starting a fresh lease server, tabula rasa.

It's also easy to gracefully deploy a new lease service by transitioning clients from only using the old server to using both servers, and finally using only the new server. 
This sort of migration tends to be much more challenging with persistent data.
Again, the short lifetime of individual leases lets us quickly move from one state to the next, without migrating state.

Application processes also operate on leases individually (there is no cross-lease operation), so the workload is trivially shardable.
And, just like transitioning to a new server, changing the sharding scheme can be done without moving data, simply by waiting for leases to expire (and then a little bit more to make sure any commit watermark we'd lose is clearly in the past).

Coordinating with leases separates small and short-lived coordination information from the actual data, which tends to be larger and longer-lived.
This segregation lets us easily handle leases with a solution specialised for quickly churning through small values.

Notice the decoupling
=====================

What I find most exciting about this approach is that organisation of persistent data, any data sharding strategy in particular, is fully decoupled from the lease hierarchy.
When working with primitives like conditional stores, it's really hard to update disjoint keys atomically.
More generally, a sharding strategy lets us scale, but only by baking in access patterns.

Our approach with weakly ordered closable bags of messages augmented with leases offers a solution to this dilemma.

A process can always hold a multitude of leases concurrently;
acquiring in a fixed order can help avoid deadlocks, but, either way those are eventually resolved by expiration (turning into a livelock problem).
There is also no correspondence between leases and where new messages are written, so processes can naturally write a message (batch of messages) that is logically associated with multiple leases,
without going through a special multi-shard commit protocol.

We can even add special support for 2-phase lease acquisition by noticing that no write happens until a process has acquired all the leases it needs.
It's a lot easier to invalidate a lease when we know it doesn't map to any write (that's a no-op).
Processes could thus start by tentatively acquiring all the leases they need, and only then update from tentative to full-blown leases, do their work, and release their leases.
With respect to correctness, a tentative lease may be broken for any reason at any time:
any eventual upgrade to full lease will simply fail, and processes aren't supposed to write until they hold a full lease.

There is a read/write trade-off
===============================

There's a downside to letting writes for arbitrary logical domains be scattered in unordered bags:
reads now have to reassemble the state of the world from a lot of different messages.
I don't think that'll create particularly thorny issues.

In practice, there's often room for some hierarchical consolidation.
For example, with 100+ cores on one machine, we could try to manage single-writer logs on a per-machine basis rather than per-core or per-service.
That's a weak affinity: we don't need ordering, the consolidation is purely for performance, so it's always possible to break a local lock and open a new log.

Speeding up reads by aggregating changes in a materialied view also tends to be a simpler problem than coordinating writes.
If some aggregation process gets stuck, it only means that reads are slow for a while, not paused or, worse, incorrect.
It should also be easy to make the publication of views idempotent;
this means we could always spin up another aggregator if the old one is stuck.

In the end, it is true that storage systems usually handle more reads than writes, but we know how to optimise reads better than writes, so it probably makes sense to focus on simplifying the write path at the expense of slower naïve reads.

<p><hr style="width: 50%"></p>

Appendix: one concrete instance of this pattern
===============================================

<a name="appendix" href="#appendix">¶</a> While I believe there's a place for something like this pattern in fancy RPC-heavy backends with replicated services,
I'm currently more interested in small (dozens to a few hundred machines) clusters tied together with a shared file system.
I guess many serverless workloads with a NFS mount look similar.

The bag-of-logs aprooach seems well suited to clustered or networked file systems.
Each log is a directory;
under that directory is a tree of subdirectories, one leaf subdirectory per time bucket (e.g., one per second);
each of these leaf subdirectories serves as a closable bag of single-writer logs.
Finally, in each time bucket, we have a set of data files, one file per writer (per machine).

In order to close a time bucket (because it's associated with a time range clearly in the past, e.g., more than 2 seconds old), a process must:

1. Declare its intention to close the time bucket by ensuring a "prelock" file exists in the time bucket directory
2. Mark all (currently existing) data files read-only
3. Prepare a lock file
4. Ensure a lock file exists in the time bucket directory by `link(2)`ing it place
5. Mark the directory read-only

The lock file consists of a list of data files and their size, as of step 3.
Steps 1 and 4 may fail because a prelock or a lock file already exists;
such failures are fine and simply mean that another process is helping us make progress.

A process writes to a log by ensuring the time bucket for its message(s) exists, and opening or creating (in that time bucket directory) a data file named after the machine it's running on;
this assumes some machine-local locking to avoid concurrent writes to the same file.

If the data file was newly created (more conservatively, is empty), the writer process must check whether the time bucket directory contains a prelock file. If so, the write must fail.
The operation may also fail because the data file is read-only, or because the time bucket is read-only;
these are both clear signs that the time bucket's bag is closed for new write.

Once we have a writable data file, the writer process appends the new message, closes the file, and checks again if it's read-only;
we can skip that step if the current time is clearly not too late, e.g., less than one second after the time bucket's range.
If the file is now read-only, some process is trying to close the time bucket, and the writer must help that other process:
the only way to determine whether the write succeeded is to confirm that its offset made it to the lock file.

We could try to manage leases directly on the filesystem,
but that's going to be a metadata-heavy affair, and networked file systems aren't great at that.
I think a specialised lease service makes more sense here.
There's no need for persistence, so we can stick plain SQLite or lmdb files on tmpfs
(that's right, not even real in-memory data structures),
and, with a bit of internal sharding to spread out contention, easily support a couple thousand lease operations per second.

<p><hr style="width: 50%"></p>
