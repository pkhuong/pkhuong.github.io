---
layout: post
title: "Low write and space amplification for everyone"
date: 2022-01-23 14:58:23 -0500
draft: true
hidden: true
comments: true
categories: 
---

Or minmaxing the LSM I/O model.

Summary: anything can have LSM-like space and write amplification, as
long as it's afforded the same cache size and allowed a similar hit on
CPU usage.

[Log-structured merge trees (LSM trees)](https://www.cs.umb.edu/~poneil/lsmtree.pdf)
are reputed to offer [lower write and space amplification than B-trees](http://smalldatum.blogspot.com/2015/11/read-write-space-amplification-b-tree.html),
since B-Trees must rewrite full pages (either in-place or Copy-on-Write).

The argument also goes that read amplification for LSMs is negligible
because all but the bottom-most layer is cached in RAM: this lets LSMs
pay for \\(\mathcal{O}(1)\\) I/O, for the bottom layer, regardless of
fragmentation in the higher layers (closer to the L0 memtable).

There are multiple constraints on the penultimate and bottom layers.
They all conspire to demand a leveled compaction strategy for the
bottom layer, and to prevent the fanout between the two layers from
growing much wider than \\(10\times\\).  If we want the LSM to pay for
at most 1 I/O for the bottom layer, we can't use size-tiered
compaction for that layer; size-tiered compaction would also incur a
lot of space amplification for that last layer.  With leveled
compaction, the worst-case write amplification when promoting from the
penultimate to the bottom layer is equal to the fanout, so can't be
much higher than \\(\approx 10\times\\).  In other words, assuming
that LSMs can cache all but the the bottom layer is equivalent to
assuming they can cache \\(\approx 10\%\\) of the data, and even a bit
more for metadata regarding the bottom layer.  There is a direct
inverse relationship between the layer of write amplification when
promoting to the bottom layer and the amount of data above the bottom
layer, i.e., to the cache size.  Reducing the cache to \\(5\%\\) would
incur a \\(20\times\\) write amplification factor in the final
promotion.

Comparisons between B-Trees and LSMs tend to give a similar advantage
to B-Trees: assume all internal nodes are cached.  However, given the
high branching factor of B-Trees (e.g., \\(\approx 30\\) records per
leaf node), internal nodes add up to much less than \\(10\%\\) of the data
(on the order of \\(3-4\%\\)).

Here's my unsurprising claim: if we assume that accesses to a buffer
of capacity equal to \\(10\%\\) of the base data cost us 0 I/O, we can
recover similar space and write amplification bounds for B-Trees (or
any other data structure constructed on top of large variable-length
pages), without worsening the number of read IOps.  Much like LSMs, the
price is additional CPU time.

A log-structured page manager
-----------------------------

We will achieve this bound by mediating all page accesses through a
page manager built on top of a write-optimised table designed for
small keys (e.g., 128-bit uuids) and large (tens of kilobytes) values.
We assume the manager can maintain an in-memory hash table with
constant-space descriptors for each key-value pair.  That's reasonable
because each value is a page (multiple kilobytes), and the manager is
responsible for bounding write amplification, regardless of page size.
We can thus assume pages are at least, e.g., \\(1000\times\\) as large
as their descriptors.

We will fully exploit the "free" buffer for \\(10\%\\) of the data set
size with a two-level structure: a flat log of complete pages, and a
buffer that's initially empty.  When the buffer is empty, the flat log
contains no redundant pages (i.e., it is fully garbage collected),
with no space amplification except for what's already incurred by the
B-Tree (which shouldn't be much because the manager allows
variable-length pages).

Each page is described by a
[cons-list](https://en.wikipedia.org/wiki/Cons) (LIFO single-linked
list) of deltas that terminate either with `NIL` (for an empty initial
page), or a base page.  Each delta starts with a "next" pointer, a
locator (e.g., segment file id, offset, and size hint) for the next
oldest element in the cons list.  This structure is similar to the
approach taken by [Conway et al for external hashing](https://arxiv.org/abs/1805.09423),
but simpler because we can assume values are much larger than
keys.

The in-memory hash table maps 128-bit page uuids to locators for the
head of each page's cons list of deltas and base page.

When reading from a page, the manager will find the locator for the
list head for that page, and walk the list to fetch more deltas until
it reaches the end (`NIL` or a base page).  The application is
responsible for interpreting the deltas and the list (e.g., it could
replay the edits on a copy of the page).

As long as deltas are only in the buffer (\\(10\%\\) of the dataset
size), this adds up to 1 read I/O for the base page, or less.

Writes to a page are exactly what you expect: log the delta (along
with a locator for the old list head) in the buffer, and update the
in-memory hash table to point to the new delta for that page.  This
log of delta can also serve as a WAL, and there is thus marginal write
amplification (assume no amplification, 1x).

Eventually, the buffer will grow to \\(10\%\\) of the data set size,
and will have to be flushed.  We do so by fully scanning the flat log
of pages, and applying all changes to that log; new pages go in fresh
log segments.  The details don't really matter, as long as each byte
in the flat log is rewritten at most once.  This flush amortises to a
\\(10\times\\) write amplification factor.

Write amplification in the append-only buffer is negligible, and
flushes are triggered by size in bytes rather than number of logical
changes.  This lets the page manager achieve its write and space
amplification bounds regardless of page size; pages can even have
different sizes, as long as they're large enough to dominate their
entry in the in-memory hash table.

Combine all that together, and we find a page manager that achieves a
total of \\(11\times\\) write amplification, at most \\(10\%\\) space
amplification, and no read amplification compared to the base data
structure (B-Trees), as long as that data structure also has its
separate cache.  We assumed B-Trees cache \\(3-4\%\\) of the total
data size; combined with the page manager, that's \\(14\%\\).  We
could of course lower the buffer's RAM footprint, e.g., \\(6\%\\) at
the price of an \\(18\times\\) write amplification factor (and
\\(6\%\\) space amplification).  However, the page manager decouples
write amplification from the page size, so we could simply increase
the size of B-Tree leaf pages and reduce the internal node footprint
to \\(\approx 1\%\\) of the data set size.

Making this a little bit more practical
---------------------------------------

LSMs proponent might model accesses to \\(10\%\\) of the data set as
"free," but I think everyone can agree that's not really the case.

We probably want to structure the buffer like a size-tiered layer:
each layer has at most one entry for each page, except for the
\\(L_0\\) log, which is fully time-ordered.  When the \\(L_0\\) log
grows to \\(1\%\\) of the dataset size, it's shuffled to create a new
intermediate log in \\(L_1\\).  When we have \\(10\\) such \\(L_1\\)
logs, we can flush them to the final base log.

Size-tiered promotions cost us very little write amplification
(\\(1\\) per layer), so we're still doing great on that front.  In
fact, we can add more layers to increase the growth factor from
\\(L_0\\) to the total data set size.  However, given the size of our
values, the benefits aren't as clear as with LSMs.  If we do that,
we'll need per-layer directories (that still fit comfortably in RAM,
and with linked list locators that tell us which directories to skip).

We can also incrementalise flushes over time by, e.g., GCing
\\(10\%\\) of base logs whenever we we add one more tier (with a
footprint equal to \\(1\%\\) of the whole dataset).  If we do that, we
will also want to version fully materialised pages, to know what
deltas to disregard.

Append-only workloads can also do a lot better: we only need to GC old
pages when they have been mutated or deleted.  We could amortise GC
scans independently of flattening fresh data into new segments in the
base log.  Write amplification is then only an issue for mutations,
while appends only pay for copies from \\(L_0\\) to \\(L_1\\) and
\\(L_1\\) to a fresh segment in the final base log.

In all cases, freer-form restructuring comes at the expense of
more directories (hash table from page uuid to location) for each
layer that may be restructured independently of more recent data.

Another problem with the linked list structure is the way it fully
serialises I/O.  We can instead store multiple locators (e.g., up to
one per tier) directly in the in-memory hash table.  This both allows
parallel I/O, and makes it easier to restructure old data without
rewriting younger changes that refer to that old data.

With such an approach, we could flatten changes to new log segments,
and then scan older segments only to delete useless records.

Lifetime and locality hints would also make sense.  For example,
internal B-Tree nodes are longer lived than leaves, so we probably
want to store internal nodes and leaves in different log segment
files.  That doesn't change the worst-case bounds, but increases the
practical probability that we will scan a segment and find there's
nothing to do (we can also decline to rewrite a segment when the
wasted space is \\(<2\%\\), to heuristically waste a bit more space
and reduce write I/O).

That's just an LSM!
-------------------

Well yes... but really specialised for the lookups performed by the
page manager: only point lookups for short keys, where the records
associated with each key add up to tens of kilobytes.

The lower bound on the footprint of each key is essential to the
strategy of just storing every key in an in-memory data structure.

Combined with the knowledge that we only care about point lookups,
we can also implement a trivial version of fractional cascading.

I think this adventure in storage system entomology is useful, because
it shows how we can decouple the write amplification properties of
LSMs from the specific associative data structure implemented with
LSMs... and how the fundamental tradeoff is additional CPU time to
interpret deltas in read operations.

How do you recover from crashes?
--------------------------------

That's a fair question, and probably boils down to journaling updates
to the in-memory hash table, with periodic snapshots.  However, I
think it's important to treat the two concerns (persistence because
data doesn't fit in RAM and durability) separately: LSMs solve both
problems at once, and it's good to explore how we can scale or relax
each concern independently.  The in-memory hash table is also known to
be much smaller than the data itself (e.g., we can assumes pages are
large enough for the hash table to be \\(<0.1\%\\) of the data), so
that's a simple problem.

Should the hash table updates be journaled in the data (page/delta)
log or separately?  Probably in the same \\(L_0\\) segment, then
separately as we restructure segments: entries in the hash table
journal has a much shorter lifetime than the data.  Each segment or
layer can also include its own directory to speed up recovery.  The
directory won't necessarily be much smaller than the deltas, but can
instead be amortised against the page itself (there's a bounded number
of layers in the system, and each page appears a bounded amount of
time in each layer).

What other data structures can we implement?
--------------------------------------------

This post uses B-Trees with large pages as a running example for the
page manager's consumer.  However, we could also use the same manager
to back persistent hash tables (e.g., with
[extendible hashing](https://en.wikipedia.org/wiki/Extendible_hashing)) or
[exponential search trees](https://arxiv.org/abs/cs/0210006).
The freedom to vary the page size and the ability to rely on the page
manager to control write and space amplification feels like a huge
simplifier for persistent and transactional data structures.
The fact that pages can be identified by name (e.g., a crypto hash of
their logical path from the root) rather than by index is just gravy.
