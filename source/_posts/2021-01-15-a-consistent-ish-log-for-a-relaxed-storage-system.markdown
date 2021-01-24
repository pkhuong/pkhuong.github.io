---
layout: post
title: "A consistent-ish log for a relaxed storage system"
date: 2021-01-15 17:33:24 -0500
draft: true
hidden: true
comments: true
categories: 
---

Change data capture (CDC) is a powerful primitive to build new
features on top of a pre-existing root data store, without coupling
the new logic with the old data store.

However, in order to derive reliable systems out of captured changes,
the changes must be consistent; ideally, the state of the root data
store could actually be reproduced, with bitwise fidelity, from the
CDC stream.  That's why write-ahead log shipping is such a good way to
extract change data out of a legacy system.

What are we to do when the root / "source of truth" data store does
not implement anything like write transactions?  That's our problem at
Backtrace, where stores to individual (columns, row) pairs are atomic
(thanks to the hardware memory model), but everything else can
arbitrarily interleave independent write operations.

Even the LL/SC conditional store functionality isn't that atomic: it
is vulnerable to low-level data races, and fails partially (at a row
granularity) *by design*.

How do we get useful change logs out of that?

We have to employ several tricks to make that works.  The main
components are

1. Tagging changesets with strictly monotonic ids.
2. Tracking change events at the level of epochs, with a
   double-buffering logic.
3. Relying on frequent quiescent states to recover sanity

Monotonic ids for changesets
----------------------------

We may not have transactions, but we still have sets of logically
related changes.  For example, when we populate a bunch of attribute
for the same row because we just analysed a crash object, that's
really one big change.  Similarly, when we clear everything for a row,
or when we execute a "set" (UPDATE) operation on a bunch of rows.

We want to introduce the concept of a "changeset," and to identify
them.  Internally, we call these changesets "write batches" because
changeset could cause confusion given our business domain (they're
definitely not p4 changelists), and because the term hopefully
conveys how little structure is in there.

However, given the free-form embedding of structure of CRDB and the
amount of legacy code, it doesn't make sense to explicitly pipe write
batch ids throughout the write interface.

We instead rely on thread-local variables to hand-roll (deep-bound)
dynamic scoping.  The embedding program (coronerd) can define write
batches with a macro that unwinds things correctly on exit.

However, it's still not realistic to assume that every store/update
will be associated with a write batch.  That's why we don't expose a
"what is the current write batch id" getter.  Operations that need
such an id must instead use a macro that will ensure a write batch
exists before letting the caller know what write batch id to use.

In effect, we're creating short-lived batches, like autotransactions,
when the caller does not define one.

Once we've ensured a batch surrounds a low-level operation like
writing to a row's individual attribute, we can record a change
event (wrote to row x, replaced old value y with new value z),
and tag that event with a batch id.

Now that we know how to tag operations with write batches, we
simply have to come up with an id scheme.

It's super useful to work with ranges of ids rather than sets of ids,
so we really want our ids to have a lattice structure. Let's
strengthen that and assume a strict total ordering, within a "change
capture" domain (e.g., a tenant's project, with a main table, an
issues table, and a metrics table).

We can't guarantee strict total ordering in general, because programs
restart, and we may look at ids generated across multiple machines.

That's why we'll define our write batch ids are pairs of "domain id",
and "monotonic id."  The domain id is a random 63-bit value, which
should hopefully not collide given our scale, while the monotonic id
is guaranteed unique and monotonically increasing for an OS process
(and a domain id is tied to its process).

Our monotonic ids are based on POSIX `CLOCK_MONOTONIC` (well, really
Linux `CLOCK_BOOTTIME`) timestamps, except we compute an offset to
convert `CLOCK_MONOTONIC` (`CLOCK_BOOTTIME`) to `CLOCK_REALTIME` (we'd
really want `CLOCK_TAI`, but that's broken on Linux) when the program
starts up, and use the same offset for all ids generated in the
program.  This offset lets us work with timestamps that roughly match
reality (seconds since the Unix epoch), while also guaranteeing
monotonically increasing values.

However, there is no guarantee that `CLOCK_MONOTONIC` timestamps
differ between two calls, only that they never go backwards.  That's
why we reserve a couple bits for a counter.

We convert each timestamp to 32.32 fixed point, zero out the bottom 7
bits, and then try to update a global 64-bit variable: this variable
lets us compare the id we're about to generate with the last id
generated, and confirm strict monotonicity.  If we'd go backwards, we
instead atomically increment that global by 1, and use the result as
our new timestamp.  Since we zero out the bottom 7 bits, the increment
won't carry over into the timestamp portion unless we generate more
than 128 ids in the time it takes for the monotonic clock to tick...
and Linux ticks every ~40ns.  It doesn't seem likely that we'll be
able to generate more than one id per nanosecond: even with
multi-threading, we're bottlenecked on the global monotonically
increasing variable.

For convenience, we also reserve 0 as a sentinel value for monotonic
ids: whenever we'd generate 0, we instead generate 1.

So, we have write batches, and each write batch is identified with a
pair of (domain id, monotonic unique id).  We can also report atomic
changes (one row and one attribute), and tag them with the batch id
and table name.

Oh yeah, in theory, a 32-bit second counter can wrap around.  Use
modular comparisons, and assume 68-year delays are out of scope.

Track changes by epochs
-----------------------

If we had strong enough isolation guarantees (e.g., sqlite's big write
lock), the work would stop here: tag transactions with monotonic ids
in order, report change events as they enter the WAL, and finish with
the number of event in each transaction.

That's not us.  We don't have transactions, and we certainly don't
serialise updates.  So, even if we tracked change event counts per
write batch id, we wouldn't really know when one batch's event could
or definitely didn't overlap with another.

The safe-memory reclamation literature has a classic solution for this
problem, one that doesn't rely on reaching a global quiescence point:
epoch-based reclamation.

For each change-tracking domain, we can partition the monotonic id
space in epochs, such that only write batches in adjacent epochs might
overlap.

In the common case, we know that we regularly hit fully quiescence
points, where no write is in progress (in the worst case, we might
enforce one to happen every couple minutes).  We really want consumers
of our change log to be able to determine when they can disregard our
relaxed internal consistency guarantees, and assume things make sense.

That's why we also want to eagerly publish empty epochs to denote
quiescence points.

A domain's "sequencer" is responsible for mapping monotonic ids to
epochs, and for reporting metadata for each epoch: rather than
reporting the number of change events for each write batch, we report
the sum for a given epoch, which spans a range of ids in the same
domain.  This lets us bound the space usage for metadata, and helps
steer readers towards processing change events in a way that makes
sense.

The sequencer is either quiescent (no write batch in flight), or
contains two epochs:
1. the older epoch is not accepting new batches, and is waiting for
   its write batches to drain out.
2. the newer epoch is accepting all new batches.

When the all the older epoch's batches have drained out (signaled that
they will not create new change events), we can report that the older
epoch is closed.  We do so by sending a change metadata record with
the following information:

1. The sequencer's domain id
2. The epoch's `begin_monotonic_id` (the `end_monotonic_id` of the
   previous epoch, or 0 if this is the sequencer's first epoch)
3. The epoch's `end_monotonic_id`
4. The total number of change events for write batches in the
   sequencer's domain with monotonic id in `[begin_monotonic_id,
   end_monotonic_id)`.

The `begin_monotonic_id` is simply inherited from the last epoch
closed by the sequencer.  How do we set `end_monotonic_id`?

When we ask the sequencer for a write batch, we can either find the
sequencer in a quiescent state, or in a draining state.  If it's
quiescent, we generate a new monotonic id, and create a new epoch that
ends immediately after that id.  If the sequencer is draining, we
generate a new monotonic id, and associate it with the sequencer's
newer epoch.

When the sequencer's older epoch drains out, we in turn close the new
epoch, by setting its `end_monotonic_id` to a fresh monotonic id.
That new epoch becomes the older draining one, and we create a fresh,
empty, "newer" epoch.  This gets us back to the two-epoch state.

If the now draining epoch is already drained out, we close it
immediately, and the sequencer returns to the empty state.

So, we count change events by epochs rather than by write batch id, in
constant space.  We also quickly find the epoch for a given write
batch, since there are only two epochs, and can thus easily increment
the change event counter on the fly.  We also know when we're missing
a change event count, since epochs are contiguous.  Finally, the
double-buffering approach bounds the inconsistency: two write batches
may only overlap (have interleaved their atomic updates) if they are
in adjacent epochs.

Rely on frequent quiescence
---------------------------

Our write load is bursty, so we expect the sequencer to regular enter
a quiescent state, with no write batch in flight.  In fact, we could
even enforce that, by locking out write batch generation when the
sequencer hasn't quiesced in, e.g., 10 minutes.

We also know that the atomic updates for two write batches can only
have applied in interleaved order if the batches are in the same
epoch, or in adjacent ones.  That's nice because we only need to keep
two epochs in a "floating state," but what's even nicer is knowing
that the change data can be interpreted unambiguously.

And we do get that, whenever we can report that the sequencer was in a
quiescent state when a given epoch drained out.  That's why sequencers
close and drain out the "newer" epoch as soon as possible: we quiesced
after a given epoch whenever the next epoch observed no write at all.

Log readers can thus accumulate and cancel out individual change
events until they find a quiescence point, and then apply everything,
knowing that the resulting state corresponds to a snapshot of the
source crdb that had once existed.

That's not ideal, but it's something.
