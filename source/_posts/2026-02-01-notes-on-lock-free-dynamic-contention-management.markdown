---
layout: post
title: "Notes on lock-free dynamic contention management"
date: 2026-02-01 22:05:24 -0500
comments: true
hidden: true
draft: true
categories: 
---

<small>I'm pretty sure I half wrote something in this vein, and it was more detailed,
but I can't find it, so here's what came out today.</small>

As a problem domain, resource (memory, id, etc.) management provides an easy way out to ensure lock-freedom:
it's easy to acquire a small chunk of resources and pass that to an efficiently thread-local shard.
The problem then turns into one of preserving progress guarantees and practical throughput
without incurring too much fragmentation overhead.

On Linux, [Restartable Sequences](https://docs.kernel.org/next/userspace-api/rseq.html) give us a one-size-fits-most solution,
where one can usually take a lock-free data structure,
stamp one shard per core,
and then remove atomics from the critical section.

I see two main downsides to restartable sequences (rseq):

1. it's hard to scale down to less than `num_cores` shards, and modern machines can have a lot of cores
2. adapting critical section to the one-instruction commit format can introduce complexity (slower code),
   compared to locking

Some might also worry that these critical section practically have to be written in assembly language,
and are thus harder to maintain.

If we lean on the fact that we can always increase the shard count in response to contention,
I believe it's possible to find other reasonable approaches that scale down better...
and might even compete with rseq at the higher end.
It could also be possible to combine the two approaches and never have to scale to more shards
than the number of cores (at which point we just switch to rseq), but that's only an option if
we contort the critical sections to fit the rseq format.

I might have once argued that the most interesting part of rseq is
finally having a fast way to get the current core id...
this post shows why.

A pragmatic approach
--------------------

I think the simplest approach here is to fully decouple lock-freedom from contention management.

1. Take a general lock-free data structure, with full atomics
2. Make multiple copies, and assign one to each thread (e.g., randomly, or based on the core id, and reassign from time to time)
3. Detect contention, e.g., via CAS failure, by detecting unexpected versions, etc...
4. Create more copies when there is contention
5. Also, try to drop unused shards when there's less contention

We still pay for full atomics, but save a lot on contention, and contention is usually what makes atomics slow.
This is a good, simple 80/20 solution; the one you probably want.

Read on for fancy stunts.

But you said we could have our lock and be happy
------------------------------------------------

It's counter-intuitive, but we can build a lock-free system that involves locks...
as long as we can create more locks when we find they're all taken,
just like we can create more resource management shards on the fly.

Since we can just create more locks on demand, let's see if we can make locking faster.
In particular, let's try to avoid atomics at the head of the critical section:
I have anecdotally observed that profiling interrupts tend to happen just after LOCKed instructions,
and [Travis Downs concurs, with more convincing experiments](https://travisdowns.github.io/blog/2019/08/20/interrupts.html#:~:text=if%20you%20want.-,Atomic%20Operations,-What%20about%20atomic).
This makes me worry that preemption interrupts are more likely to kick in
just after we've acquired a lock than just before we do.

A lock that can be acquired without atomics should thus be less likely to be held by a preempted thread.
This post describes a lock with an initial atomic-ful acquisition phase, but afterwards no atomic for lock acquisition or release.[^atomic-release]
The result should hopefully be less frequent preemption in the middle of critical sections.

[^atomic-release]: I'd be interested in a lock that can be acquired without atomic instructions, but must be released with atomics. That too would avoid shifting interrupts inside critical section.  Unclear if that's even possible though

Hazard pointers for biased locks, take 1
----------------------------------------

Our exclusive locks will be split in two parts:

1. a lock object with a versioned pointer to the lessee struct (multi-writer/multi-reader, written with CAS)
2. a flag word *in the lessee struct*, and a backpointer to the lock object (single-writer/multi-reader, written with regular stores)[^two-counters]

[^two-counters]: We just want to protect against ABA when the lessee takes a different lock. A pair of lock/unlock counters would also work.

Only the lessee may "take" its lock, but the lease can be broken at any time..
except when the lessee has asserted ownership by setting its flag to true.
By itself, setting the flag word does nothing; it's only meaningful when the
flag word it set while the lessee stills holds a lease on the lock.

Assume the current thread has taken a lease on a lock.  To acquire the lock, it:

1. sets its flag word to true
2. [compiler fence]
3. checks that it still holds the lease on the lock (including version counter), otherwise clears the flag word and fails.

Unlocking is just a regular store to clear its flag word.

On the other side, we can break a lease by:

1. Incrementing the version in the lock
2. [membarrier (reverse fence)]
3. waiting until the lessee's backpointer is free or points back to a different lock

As usual, the membarrier is slow, so we amortise it with a limbo list.
The lock semantics only mean that a lock could remain on the limbo list forever,
if its lessee is stuck in the middle of a critical section.

This all works, but it's not great because we can't transfer ownership without a membarrier
(or involving the current lessee).

Again, with some per-core fanciness
-----------------------------------

The rseq approach maps resource shards to cores 1:1, so critical section must be abortable:
we must leave the core's resource free for the next thread when the scheduler switches us out.
We can similarly specialise for intra-core ownership (lease) transfer,
without assuming a 1:1 mapping.
This should help us avoid full atomics, while providing regular locking semantics.

A lock structure now has:

1. version (probably worth having an odd/even scheme to denote locks in the limbo list)
2. pointer to the lessee
3. a core id

We need the new third field because a lessee will now only be able to acquire the lock only while it executes on that core id.

The lessee still has a flag word coupled with a backpointer to the lock object, and now a copy of the lease's core id.

Assume the current thread has taken a lease on a lock (via regular locking, a CAS loop, ...).  To acquire the lock, it:

1. sets its flag word to true
2. [compiler fence]
3. checks that its core id still matches the lease's, otherwise clear the flag word and fail[^rdpid]
4. [compiler fence]
5. checks that it still holds the lease, otherwise clear the flag word and fail

[^rdpid]: Getting a core id is usually pretty fast; must be fast for rseq. If you can use rseq, then [it's just a memory load](https://github.com/torvalds/linux/blob/18f7fcd5e69a04df57b563360b88be72471d6b62/tools/testing/selftests/rseq/rseq.h#L167-L174). On linux/x86-64 without RSEQ that's LSL or RDPID. I'd be more comfortable with an actual memory location, but I'm pretty sure this works as expected (in terms of x86-TSO).  Other platforms should also be fine, otherwise this feels to me like a broken sched_getcpu.

This ordering is important because ownership is monotonic, but the core you're scheduled on isn't.
When the check in step 5 succeeds, we know there was a point at which the lessee had set
its flag word, was running on the expected core id, and still held the lease.
With the combination of these three conditions, we know the lessee now has an actual lock,
until it releases its flag word.

Breaking a lease, in the general case, is still just incrementing the lock's version / membarrier / wait until lessee doesn't hold the lock.

### Intra-core lease transfer

However, we can do better when breaking the lease from the core id in the lock structure:
threads scheduled on the same core observe each other's stores in order.

Let's say we're on core 1, and we wish to break the a lock's lease, where the lessee has a lease for the same core id 1.

We still start by incrementing the lease's version counter.

If the lessee has set its flag word, we still have to wait; we
don't necessarily have to wait for a membarrier, if everything is on
the same core, but that's a minor optimisation.

The interesting cases happen when we (in order):

1. increment the version in the lock structure with an atomic instruction
2. confirm we're on core id 1
3. observe that the lessee has *not* set its flag word.

N.B., there's no guarantee that the load at step 3 happens on core 1, but,
if we were migrated between steps 2 and 3, the OS had to ensure we'd see our own writes,
so step 3 should still observe everything that had happened on core 1 as of step 2.
A pair of (ABA-free) acquire/release counters in the lessee structure would probably make the proofs easier.

This can only happen because:

1. the current lessee is after step 1, but on a different core -- the lessee will fail the acquisition on its own
2. the current lessee is truly outside its critical section (before step 1 of the acquisition)

In the first case, we claim the lessee must be on a different core because, otherwise, we'd have observed the new flag word.
Usually, we then expect the lessee to fail its core id check.
However, the lessee could be migrated back to core 1 between steps 1 and 3;
that's OK, because step 5 will detect the mismatched version in the lock structure (because the locker's step 3 must happen after the lease breaker's step 2 [otherwise the lease breaker would observe the flag word in its step 3]).

The second case also works fine because the next acquisition will either be on core id 1,
in which case the lessee will definitely observe the new version of the lock structure (and fail step 5),
or it will be on a different core, which reduces to the first case.

This is a bit non-traditional, but we essentially introduced happens-before constraints via the fact
that a core runs only one thread at a time, and that a core sees its own writes.

Since we can always create new allocator shards, we can protect shards with biased locks where:

1. each lock has a core-bound lessee (the lease only works on that core)
2. the lessee can acquire the lock without atomics, as long as it's on the correct core
3. other threads scheduled on the same core can break the lease, when the lessee isn't in a critical section
4. other threads can still break the lease, but must go through a slow membarrier (a reverse fence)

Simpler primitives than restartable sequences
---------------------------------------------

I think this biased locking approach is promising:
coupled with some unspecificed contention manager, 
it can scale directly with contention, including scaling down,
probably won't overshoot too much past the number of cores
(unless threads are *always* in a critical section)...
and, importantly, it lets us write regular critical sections in high level programming languages.

In order to achieve this, we only needed a fast way to get the current core id.
I remember thinking that it might be nice to couple that core id with a preemption counter for the running thread,
but I can't remember where that would fit in the above.

A reverse barrier is also helpful, but it's more for generality in
this case.  If I had to make the case for a primitive in that vein,
I'd probably go for something like
https://github.com/pkhuong/barrierd: the blocking membarrier syscall
is `rcu_synchronize` in userspace, and using `rcu_synchronize` in
production is pretty much always a bug.

<p><hr style="width: 50%"></p>
