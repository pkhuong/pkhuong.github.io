---
layout: post
title: "Six versions accessing: wait-free protected versions with bounded cardinality"
date: 2025-12-30 10:32:36 -0500
comments: true
categories: 
---

<small>For [Elijah "moonchild" Stone](https://web.archive.org/web/20251111171856/https://outerproduct.net/).</small>

This post describes a reader/writer version management scheme I recently came up with.
I find it interesting because it

1. is wait-free for readers and for the writer
2. doesn't need atomics (including fences) on [TSO](https://en.wikipedia.org/wiki/Memory_ordering#In_symmetric_multiprocessing_(SMP)_microprocessor_systems), or on [ARMv8-a](https://developer.arm.com/documentation/ddi0487/latest)[^req] for readers that keep up with the writer
3. supports dynamic reader registration (dually, supports sleeping readers)
4. bounds the number of protected (live) versions as a function of the number of stuck readers, assuming all other readers keep up

[^req]: We really need load-load ordering and a lack of value speculation, which boils down to TSO in practice... but we can fake it on RMO with [16-byte atomic loads (LDP)](https://developer.arm.com/documentation/dui0801/g/A64-Floating-point-Instructions/LDP--SIMD-and-FP-) or maybe with a fake data dependency between the first load and the second load's address. OTOH, the surrounding code would probably need something like acquire semantics for the LDP anyway.

The last point is interesting for the higher level system, since it's now practical to allocate storage for all protected versions statically...
[safe memory reclamation (SMR)](https://queue.acm.org/detail.cfm?id=2488549) makes sense even without dynamic storage management!

We can always build regular *memory* reclamation on top of the version tracking,
but we can also directly use the bounded set of protected versions to, e.g., implement multi-version read transactions in constant space.

Even when embedded in vanilla epoch-based memory reclamation,
the scheme brings something interesting to the table,
since it combines a QSBR fast path with support for sleeping (disabled) readers, thanks to the hazard pointer slow path.
The QSBR fast path and its fence-freedom on TSO can be useful even now that we have [membarrier(2)](https://pvk.ca/Blog/2019/01/09/preemption-is-gc-for-memory-reordering/#:~:text=Asymmetric%20flag%20flip%20with%20interrupts%20on%20Linux),
e.g., when we want to support short epoch update periods (less than every few hundred nanoseconds),
or when running on isolated cores.

We need at least three protected versions to avoid blocking (i.e., to implement double buffering: one version for the writer, another for readers that are up to date, and the last for readers that are about to switch to the new read version).
If we're willing to introduce blocking, we can even make progress with two versions (single buffering).
Ideally, we'd make progress despite \\(k\\) blocked readers by allowing up to \\(3 + k\\) or even \\(2 + k\\) versions.

Turns out that's hard to do while preserving property 2 (atomic-free readers on the happy path), 
so we'll instead require a footprint of \\(3 (k + 1)\\) versions, where \\(k\\) is the number of stuck readers.
In practice, I find that being robust to just one stuck reader (or a group of readers that got stuck at the same time)
is pretty useful, for the additional space overhead of three versions,
especially since the extra three versions don't even have to be used until there actually are stuck readers.

When there are too many stuck readers for the protected version budget,
the system keeps running, but the writer's version is frozen.
Readers are unaffected, and the writer can still write, but can't move on to a fresh version;
in practice, this can mean that the limbo list grows without bound (the usual failure mode for epoch based reclamation),
or, in a [MVCC](https://en.wikipedia.org/wiki/Multiversion_concurrency_control) system, that the writer is unable to make its updates visible to readers
(they can still be committed to stable storage, they're just not visible in memory).

That's yet another animal in the ménagerie of safe memory reclamation (SMR) schemes.
I like to make sense of all these design options and how they fit in higher level system design with to the following reductionist take on [SMR](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-579.pdf#page=77):
it's all remixes of [hazard pointers](https://ieeexplore.ieee.org/document/1291819) (HP), [quiescent state based reclamation](https://preshing.com/20160726/using-quiescent-states-to-reclaim-memory/) (QSBR), and [proxy collection](https://mastodon.social/@pervognsen/112447606750157685).[^proxy-gc]

[^proxy-gc]: I don't have a reference for [proxy collection](https://mastodon.social/@pervognsen/112447606750157685). The best I can think of is Joe Seigh's Usenet posts on [comp.programming.threads](https://danluu.com/threads-faq), but I can't remember which one made it click for me. Maybe [Joe's `proxies` repo](https://github.com/jseigh/proxies) will work for you.

For example, casting epoch-based reclamation as a proxy collection system where we manage versions (epochs), and versions hang on to (serve as proxies for) objects makes it clear that the core epoch management logic in EBR is just a specialised version of hazard pointers...
and thus all the atomic-free read-side tricks with membarrier and interrupt-atomic copy port directly between the pointer protection read-side in hazard pointers and the version (epoch) publishing read side of EBR.

Decoupling the proxy collection piece (what set of objects does each version protect from reclamation) from the version acquisition/protection logic also clarifies the relationship between EBR and QSBR.

In classical EBR, readers protect one version (one epoch), and each version (epoch) hangs on to all objects that weren't logically deleted before that epoch (before it became the writer's active epoch).

QSBR looks similar, but I believe it's illuminating to view it as a dual to EBR.  In QSBR, readers relinquish all versions older than some watermark, and implicitly protect all versions at least as recent as the watermark.
In that world, QSBR readers protect a half-unbounded version range (in practice, it's bounded to the most recent version used by the writer), and each version V hangs on all objects that were logically deleted while V was the writer's active version.

The modularisation between version management and proxy collection is imperfect:
for example, EBR readers can publish an old version without checking that it's still the writer's version,
since publishing a stale version implicitly protects a superset of those protected by the correct version, the writer's version
(but that makes invariants harder to check, since readers don't know the version they were supposed to publish).

That's the usual for abstractions: imperfect, but still useful as a tool for thought.
I feel the modularity is particularly helpful when reasoning about SMR mechanisms at a higher level than just deferred destructors, e.g., when combining SMR with heavier weight abstractions like [object software transactional memory](https://dl.acm.org/doi/10.1145/1233307.1233309):
in OSTMs, it can make sense to manage versions explicitly, at a higher level than physical memory management.

I came up with this version management scheme in a similar context with native versions,
and ripping out the proxy collection concern
highlights its nature as a hybrid of QSBR on the happy path and hazard pointer in the general case.

The basic setup
---------------

As usual for hazard pointer and EBR approaches, the writer can logically handle each reader in isolation,
but should process readers in batches to improve efficiency.

For each reader, we have a record in which two fields are logically owned by the writer---the stable version and hazard pointer limit are updated by the writer and read by the reader---and the remaining current version field is *usually* written by the reader and read by the writer.
Only when the current version's HELP flag is set can both the reader and writer update it,
with atomic compare-and-swap.

When the writer updates the stable version and hazard pointer limit (`hp_limit`) fields, 
it always updates the hazard pointer limit first, and the stable version second.
At first sight, a release store to the stable version would suffice, but
we'll see that we also want a store-load fence after the update to the stable version;
when updating a batch of records on x86oids, 
we can achieve that with regular release stores for all but the last update to the `reader_record.stable_version`,
and updating the `stable_version` field for the last record in the batch with an atomic exchange.

Symmetrically, the reader always loads the stable version first, with an acquire load,
followed by the hazard pointer limit.

```
reader_record:
  - stable_version: most recent version the reader should try to stick in current_version, non-zero after startup
  - hp_limit: (hazard pointer limit) we'll get to it later, in the invariants section
  - current_version: version currently protected by the reader, zero when record is inactive;
                     lends one bit to a HELP flag.
```

Unlike general hazard pointer records,
we need only a single HELP flag bit for cooperative wait-freedom:
we always know what field the reader wants to snapshot in its current version,
since each reader record owns its stable version field.

In a practical implementation,
we'll probably want the stable version and hazard pointer limit[^dual] fields in the same cache line,
and give the current version a dedicated cache line.
Each reader should also have a private copy of its current version,
so that each reader only *stores* to the current version's cache line
(e.g., with a blind cache line update, which can avoid [RFO](https://en.wikipedia.org/wiki/MESI_protocol#Read_For_Ownership)).

[^dual]: Assuming we stick the stable version and the hazard pointer limit fields in the same cache line, there is no downside with respect to cache coherence in replacing the hazard pointer limit with a QSBR limit (that tells the reader how far it can advance its current version without entering the hazard pointer slow path) or a mode flag. The result would probably slightly easier to understand, but would have more edge cases where readers are stuck on the slow path when they re-enter the QSBR range, yet the writer fails to acknowledge that fact for a while.  I'm confident we could fix that by adding logic to avoid no-op updates, but that introduces an additional *and harder to predict* branch condition.

The hazard pointer limit is associated with its reader, so must live in a the reader record,
but it would be possible to have a global stable version field shared by all readers.
The analysis would be the same, and the impact on performance depends on a lot of considerations.
In general though, this protocol is biased towards reader performance,
and giving each reader its own single-reader/single-writer `current_version` field
tends to improve reader performance, at the expense of slowing down the writer...
exactly the tradeoff we're looking for.

Parameters
----------

There are two global parameters; each affects only the writer's behaviour.

The QSBR leeway (counted in versions) limits how far a reader's current version can be behind the stable version
while letting the reader use the QSBR (atomic-free on TSO) update.
This leeway must be at least one version, otherwise the QSBR path is always disabled.
I think setting it to two makes sense, because this means a reader that keeps up in EBR stays on the QSBR fast path.

The version capacity bounds the number of versions that may be protected concurrently,
including the writer's current write version
(which differs from the reader's stable version in versioned transactions).
The capacity should be at least three, for non-blocking progress when everyone's keeping up.
Increasing the version capacity past three versions lets the system advance the write version (make regular progress, including reclaiming old versions) despite the presence of stuck readers:
each increment by *one more than* the QSBR leeway lets us tolerate at least one more stuck reader.

With a QSBR leeway of two and a version capacity of six,
readers that would keep up (not force the writer to slow down) in EBR stay on the QSBR path,
and we can tolerate at least one stuck reader.

Readers also apply an arbitrary iteration limit on the basic hazard pointer loop.
A limit of two iterations ensures that we'll only enter the fallback wait-free update when the reader is really slow compared to the writer, or keeps getting interrupted.

Invariants
----------

The stable version and hazard pointer limit fields increase monotonically, which will simplify reasoning.
The current version (after discarding the HELP flag) increases monotonically,
except when it's reset to 0... and transitioning out of the zero state enters a special slow path.
TL;DR: we can mostly assume monotonicity.

Unlike traditional EBR, we aim to make progress despite the presence of stuck readers,
so it's impossible to bound the maximum distance between any two versions,
which means we can't use modular comparisons.

It's hard to fit strictly monotonic version counters in 32 bits.
There's plenty of room in 64-bit integers though, even after stealing one bit
(e.g., the top/sign bit) for the HELP flag.

Protected versions
------------------

Each reader record protects a set of versions.
From the writer's point of view, it's trivial to take an atomic snapshot of any given reader record,
since the reader updates only one field in the record (the current version),
so it makes sense to talk about the set of versions protected by a reader record.

When the reader record's current version is 0, the record doesn't protect anything (protects the empty set).

When the reader record's current version is *at most QSBR leeway versions behind* the *next* stable version,
the record protects versions \\([\texttt{current_version}, +\infty).\\)
The writer controls the highest version actually in existence, so we can shrink that to
\\([\texttt{current_version}, \texttt{stable_version}],\\)
which includes at most \\(\texttt{QSBR_leeway}\\) versions.
Such a reader record is in QSBR mode, and can advance its current version with only an acquire load, i.e., without any fence or atomic under TSO.

Otherwise, when the record isn't in QSBR mode and its current version is strictly less than the hazard pointer limit,
the record is in hazard pointer mode and protects versions \\([\texttt{current_version}, \texttt{hp_limit});\\)
the writer ensures this interval spans at most \\(\texttt{QSBR_leeway} + 1\\) versions.
In hazard pointer mode, readers must use a full-blown hazard pointer (i.e., fenced) update when advancing the current version to or past the hazard pointer limit.

Finally, in all other cases, the record reflects a reader in the middle of a failed hazard pointer update
and protects nothing.

Reader logic
------------

The reader attempts to advance its current version by first loading a somewhat consistent snapshot of the reader record:

1. (acquire) load the stable version
2. load the hazard pointer limit
3. grab the current version, from a private copy or with a relaxed load

This load order reverses the writer's store order, 
so monotonicity guarantees that the hazard pointer limit loaded in step 2 is at least as high as if we'd taken an atomic snapshot in step 1.
This is safe because observing a later hazard pointer limit simply means that we may spuriously enter the safe hazard pointer slow path.

If the current version is non-zero and greater than or equal to the hazard pointer limit, we can use a QSBR update:
just store the stable version from step 1 in the globally visible current version
(and update the reader's private copy).
There is no store-load fence on this QSBR fast path:
it all works thanks to causal dependencies between the reader's stores and the writer's *lack of store* to the hazard pointer limit
(which is tied to the writer's stable version updates).
That's the happy path for readers that keep up with the writer.

Otherwise, the reader is too far behind the stable version, and must use a hazard pointer-style update to snap its current version ahead.
That's the safe slow path, which also handles the transition out of the zero state.[^stable-eql-limit]
After a hazard pointer update, the postcondition is that there was a time when new current version was globally visible and it was equal to the stable version.
If the reader later falls off the QSBR range, the writer will first bump the hazard pointer limit, and everything still works out.

[^stable-eql-limit]: It's a bit unsettling that updating the current version to the stable version could leave us with \\(\texttt{current_version} = \texttt{hp_limit},\\) which yields an empty range for \\([\texttt{current_version}, \texttt{hp_limit}).\\)  However, a successful hazard pointer update means the current version is within the QSBR leeway of the stable version, so the record protects \\([\texttt{current_version}, \texttt{stable_version}].\\)

We start with a regular hazard pointer update loop, for a bounded number of iterations (a limit of two iterations is plenty).
The initial guess for the stable version is the value we loaded in step 1.
We then:

a. store the guessed stable version in current version, with a store-load fence (e.g., atomic exchange)\\
b. check if our guess was correct (the stable version is indeed equal to our guess)

If the guess is correct, we successfully performed a hazard pointer update, and return from the advance subroutine (remember to update the reader's private copy of its current version).
Otherwise, we try again, up to the iteration bound.

When readers hit the iteration bound, we use a slower wait-free cooperative update.
This final backstop ensures the advance subroutine is wait-free.

A reader enters the cooperative mode by setting the HELP flag (e.g., the sign bit) in its current version field,
followed by a store-load fence.
That's a regular store and a fence, or, on TSO, an atomic OR to get both in one instruction.

The reader then runs the same hazard pointer loop, except with compare-and-swap and keeping the HELP flag set:

c. compare-and-swap our guess for the stable version (with the HELP flag set) in the current version\\
d. check if our guess was correct, otherwise try again in c., with an updated guess\\
e. clear the HELP flag (with an atomic AND, or another compare-and-swap)

And, on exit, update the reader's private copy of its current version and return from the advance subroutine.

The key part is that the check in d. can fail at most twice.

The compare-and-swap in step c. (and e.) can fail because the the current version field doesn't have the expected value.
This can only happen if the writer noticed our call for help and CASed in the most up to date stable version
(without the HELP flag).  In that case, we're done!

The check in d. can fail only when the writer has updated the stable version in the middle of the advance loop.
However, the HELP flag is set throughout the final advance loop,
so the writer is sure to observe the flag during its *second* update.
That's why the check in d. can fail at most twice (once for the initial guess, again for an unlucky writer update that just missed the HELP flag),
and the whole loop can run at most three times.

All the loops are bounded, by fiat or because they can only fail so many times, so the reader's advance routine is wait-free.

Writer logic
------------

The only complicated logic in the writer is collecting the set of all protected versions before incrementing its stable version.

The "protected versions" section explains how to compute that set for any specific reader record;
the writer has exclusive write ownership over all fields in the reader record except for the current version,
so it's trivial to take an atomic snapshot of the record, as long as we remember to mask off the HELP flag.
We must also keep in mind that the writer is about to increment the stable version by 1, so we must take that
increment into account when constructing the set of protected versions for a given record...
and we must prepare for potential hazard pointer updates, so the stable version is always protected.

There is one complication here: we need to detect slow readers that must be forced into hazard pointer mode.

When a reader's current version is at most \\(\texttt{QSBR_leeway}\\) versions behind the *next* stable version,
it is considered to be in QSBR mode, and protects \\([\texttt{current_version}, \texttt{stable_version}].\\)
This interval contains at most \\(\texttt{QSBR_leeway}\\) versions.

Otherwise, the reader may have *just* fallen behind by enough to be kicked out of QSBR mode, and must then be switched to hazard pointer mode.

If the reader's current version is exactly \\(\texttt{QSBR_leeway} + 1\\) behind the *next* stable version
(exactly \\(\texttt{QSBR_leeway}\\) versions behind the stable version),
the writer must ensure the reader enters hazard pointer mode:
when the reader's current version is greater than or equal to its hazard pointer limit,
the writer updates the reader's hazard pointer limit to the next stable version,
so the record, now in hazard pointer mode, protects \\(\texttt{QSBR_leeway} + 1\\) versions.

It's important to execute this transition only when the reader *just* fell off the QSBR fast path:
the hazard pointer update loop can temporarily publish trash versions to the current version field,
and we'd prefer to disregard those.
Since the transition happens right at the edge, when the reader just falls off the fast path,
we must always scan readers before incrementing the writer's stable version.

Notice how the downgrade to hazard pointer modes actually preserves the set of versions protected by the reader record while it was in QSBR mode
(a sufficiently delayed reader could observe the next stable version, but is then guaranteed to observe the new hazard pointer limit).
That's what makes it safe to wait until the reader notices the hazard pointer limit,
without explicit heavyweight synchronisation on a shared writable cell.

Now that the reader has been diverted to the hazard pointer slow path if needed,
the record protects \\([\texttt{current_version}, \texttt{hp_limit}),\\)
or the empty set when \\(\texttt{hp_limit} \leq \texttt{current_version}.\\)

We want the union of protected versions across all reader records.
Naïvely, this would call for an arbitrary set data structure.
However, we have a bound on the number of protected versions, so we can statically allocate the set's capacity, and flag a failure when we'd need more than that bound.
We also perform blind insertions until after the per-reader loop,
so we can use an amortised integer sort/dedup over a statically allocated array.

When few enough versions are protected to fit in our version budget after adjoining the *next* stable version,
we can increment the stable version (otherwise, we must return with failure).

We increment the stable version with a release store to each reader record's stable version field.
We also need a store-load fence after the stable version updates, so, on TSO, we can use a regular
release store for all but the last record, and conclude with an atomic-exchange for the last record.

The fence is obviously important for the wait-free helping scheme.
More subtly, it's also load bearing for the hazard pointer scan in the *next instance* of the per-reader loop above:
we must guarantee we'll observe when a reader *just* falls off the QSBR range.
Observing a stale current version for a given reader after incrementing the stable version could
lead to a reader catching up via the hazard pointer path, and the writer failing to notice that,
potentially until the reader is far from the QSBR range.
The writer would incorrectly treat that reader as having been stuck in failed hazard pointer updates the whole time.

Finally, we check if any reader record needs help (has the HELP flag bit set).

For each reader record, we load the current version field, and check if the HELP flag is set.
If the HELP flag is set, we try to atomically clear the flag and update the current version field to the new stable version with a compare-and-swap.
When the compare-and-swap succeeds, or fails because the actual value didn't have the HELP flag set,
we're done helping that reader record.
Otherwise, we must try again...
but the stable version doesn't change while the writer is helping a reader record make progress,
so we expect to attempt to help a reader record at most three times in a row.

Extra fanciness, extensions, and improvements
---------------------------------------------

I already noted where the few store-load fences needed under TSO can be implemented atomic exchanges.

Under RMO, I'm pretty sure it's possible to avoid the acquire load of the stable version in the read-side code,
by loading both the stable version and the hazard pointer limit with an [atomic 16-byte load](https://reviews.llvm.org/D67485),
or by carefully introducing a data dependency between the hazard pointer limit's *load address* and the stable version's value.[^acquire]
Even the latter should be fine for latency because we want a single predictable conditional branch around the QSBR fast path,
and the hazard pointer limit isn't used in the fast path itself (feeds only into a predictable control dependency)...
but the code that uses the current version probably needs an acquire load anyway.

[^acquire]: Careful though, depending on how the reader's current version is used, we probably still need acquire semantics for the load from the stable version or the 16-byte load of the stable version and hazard pointer limit.

In the QSBR fast path, we usually prefer to avoid spurious write traffic for no-op updates
(stable and current versions are already equal) by generating the store destination address with a conditional move,
and directing useless updates to a core-local location
(a constant-time conditional move is important, because speculative stores can still cause cache coherence traffic).
It can also be helpful to use cache line-wide stores (e.g., AVX-512 or [FSRM](https://git.kernel.org/pub/scm/linux/kernel/git/tip/tip.git/commit/?h=x86/asm&id=f444a5ff95dce07cf4353cbb85fc3e785019d430) stores) to avoid [reads for ownership](https://en.wikipedia.org/wiki/MESI_protocol#Read_For_Ownership).

When readers care about runtime latency more than having the most recent updates, it can make sense to
defer updates *in the QSBR fast path* by one advance call:
as long as we're in the fast path, it's safe to use the maximum of the current version and any older value observed in the stable version field as the stable version we wish to advance to.
Waiting to advance to a new stable version until we've observed it twice gives the writer core more time to evict updates out of its private caches into shared ones.

Readers could also remain on the QSBR fast path when they observe a fresh hazard pointer limit,
but still have an old stable version lower than the hazard pointer limit,
which can happen when the writer fails to increment its version (too many stuck readers),
or as a race condition that grows more likely with the number of readers.
Strictly speaking, we *must* enter the slow path when either:

1. the current version is 0
2. the current version is strictly less than the hazard pointer limit, and the stable version is greater than or equal to the hazard pointer limit

I don't see much room for fanciness on the write side, except for the aforementioned fence-as-atomic-exchanges.
On recent Intel machines, it can make sense to [CLDEMOTE](https://www.felixcloutier.com/x86/cldemote) after updates to the reader record,
when readers try to advance their current version infrequently enough (at least a couple hundred cycles between calls).

This all seems to work, and there's basically no overhead (except for the static footprint) compared to regular double buffering when everyone's keeping up, so I'm not really thinking about it anymore.
It might be interesting to simplify the hazard pointer limit system, and maybe replace it with a flag, if only to simplify reasoning about the protocol.
Otherwise, in terms of performance, the most impactful improvement would probably be to reduce the footprint overhead to handle stuck readers, while preserving the QSBR fast path...
but I don't see how to achieve that (yet).

<small>I used this design at $DAYJOB.
Send <a href="mailto:p${MY_LAST_NAME}+mvcc@jumptrading.com">me an email</a> *and please mention something you like about robust non-blocking <span style='color: #fff; font-size: 0; opacity: 0;'>lobster </span>synchronisation* if that sounds interesting.</small>


<p><hr style="width: 50%"></p>
