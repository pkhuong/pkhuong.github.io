---
layout: post
title: "Low write and space amplification for everyone"
date: 2022-01-23 14:58:25 -0500
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
are reputed to offer [lower write and space amplification than B-Trees](http://smalldatum.blogspot.com/2015/11/read-write-space-amplification-b-tree.html),
because B-Trees must rewrite full pages (either in-place or Copy-on-Write)
regardless of how few records they modified.

The argument also goes that read amplification for LSMs is actually
negligible because all but the largest layer is cached in RAM.
This lets LSMs pay \\(\mathcal{O}(1)\\) I/O, only for the largest layer,
regardless of fragmentation in the remaining layers closer to the
\\(L_0\\) memtable.

There are multiple constraints on the penultimate and largest layers.
They all conspire to demand a leveled compaction strategy for the
largest layer, and to prevent the fanout between the two layers from
growing much wider than \\(10\times\\).  If we want the LSM to pay for
at most 1 I/O for the largest layer, we can't use size-tiered
compaction for that layer; size-tiered compaction would also incur a
lot of space amplification for that last layer.  With leveled
compaction, the worst-case write amplification when promoting from the
penultimate to the largest layer is equal to the fanout, so can't be
much higher than \\(10\times\\).  In other words, assuming
that LSMs can cache all but the largest layer is equivalent to
assuming they can cache \\(\approx 10\%\\) of the data, and even a bit
more for metadata regarding the largest layer.  There is a direct
inverse relationship between the level of write amplification when
promoting to the largest layer and the amount of data above the largest
layer, i.e., to the cache size.  Reducing the cache to \\(5\%\\) would
incur a \\(20\times\\) write amplification factor in the final
promotion.

Comparisons between B-Trees and LSMs tend to give a similar advantage
to B-Trees: assume all internal nodes are cached.  However, given the
high branching factor of B-Trees (e.g., \\(30\\) 128-byte records per
small 4K leaf page), internal nodes add up to maybe \\(3-4\%\\) of
the data, much less than \\(10\%\\).

Here's my unsurprising claim: if we can stage writes in a buffer large
as LSMs' (up to \\(10\%\\) of the total data), and assume that reads
in that buffer cost us 0 I/O, we can recover similar space and write
amplification bounds for B-Trees (or any other data structure
constructed on top of large variable-length pages), without worsening
the number of read IOps.  Much like LSMs, the price is additional CPU
time.

A log-structured page manager
-----------------------------

We will achieve these bounds by mediating all page accesses through a
page manager built like a write-optimised key-value store designed for
small page id keys (e.g., dense linearly allocated indices) and large
(multiple kilobytes) page values.  We assume the manager can maintain an
in-memory dictionary[^array] with constant-space descriptors for each
page.  That's reasonable because each page is large enough to dominate
its descriptor (e.g., \\(200\times\\) as large), and the manager is
responsible for bounding space and write amplification, regardless of
page size.

[^array]: For dense indices, we can simply use an array keyed on the index. A hash table keyed on 128-bit hashes or uuids probably makes for a nicer programing interface.

We fully exploit the "free" buffer for \\(10\%\\) of the data set
size with a two-level structure: a flat log of complete pages, and a
buffer of deltas and pages that's initially empty.  When the buffer is
empty, the flat log contains no redundant pages (i.e., it is fully
garbage collected), with no space amplification except for what's
already incurred by the data structure built on top of the pages
(which shouldn't be much because the manager allows variable-length
pages).  Unlike LSM runs, the log's contents aren't ordered: we assume
each access costs a full random I/O, and an in-memory dictionary tells us
what bytes to read.  Fully utilising each access falls on the data
structure built on top of the page manager, e.g., a B-Tree.

Each page is described by a
[cons-list](https://en.wikipedia.org/wiki/Cons) (LIFO single-linked
list) of deltas that terminate either with `NIL` (for an empty initial
page), or a base page.  Each delta starts with a locator (e.g.,
segment file id, offset, and size hint) for the next oldest element in
the cons list.  This structure is reminiscent of 
[Conway et al's take on external hashing](https://arxiv.org/abs/1805.09423),
but simpler because we can assume values are much larger than keys.

The in-memory dictionary maps page ids to locators for the
head of each page's cons list of deltas and base page.

When reading from a page, the manager will find the locator for the
list head for that page, and walk the list to fetch more deltas until
it reaches the end (`NIL` or a base page).  The application is
responsible for interpreting the deltas and the list, for example, by
replaying edits on a copy of the page.

As long as deltas are only in the buffer (\\(10\%\\) of the dataset
size), this adds up to 1 read I/O or less for the base page.

Writes to a page are exactly what you'd expect: log the delta (along
with a locator for the old list head) in the buffer, and update the
in-memory dictionary to point to the new delta for that page.  This
log of delta can also serve as a WAL, and there is thus marginal write
amplification (assume no amplification, \\(1\times\\)).

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
entry in the in-memory dictionary.

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

But that's just a cost model hack!
----------------------------------

Yes. But it's interesting to see how must we can ram through the
model's blind spots, and to consider whether other data structures
that perform well according to the same model might exploit the same
weaknesses.  Maybe people running LSMs at scale find that their
storage is CPU-bound or more memory hungry than they'd like...

Now with some intellectual honesty
----------------------------------

[Iacono and Patrascu](https://arxiv.org/abs/1104.2799)
observed that the key problem in external memory hashing is
determining where we can find a given key: given such
a directory, O(1) lookup in a frozen subtable is easy.

[Conway and friends](https://arxiv.org/abs/1805.09423) showed one way
to simplify the trick.

We don't need such complexity: we know our values are large, and keys
small (e.g., 128-bit uuid), so we can easily amortise *one* in-memory
record per key-value pair.

That in-memory record can tell us:

1. where we can find the base data
2. where we can find any additional diff

The base data contains an updated version of the page's full contents.
The diff consists of application-specific payload; there's only one
such payload per record, so the application must concatenate multiple
diffs until it wants to generates a fully flattened value.

I think it makes sense to keep around sqrt(page size) diffs before
flattening.

Assume we have a base layer without any redundant data (i.e., only
base data, no diffs), in small (dozen MBs maybe) segments, each tasked
with non-overlapping ranges of the keyspace.

Let's say we have up to ~20% of the data set size in queued writes.

Let's structure that 20% as ~200 layers with 0.1% of the data size each
(plus the in-memory ingress layer).  Each layer is a frozen
hash dictionary, easy to construct and then search.

The layers also function as a FIFO queue: whenever we add a layer to
the queue, we must remove another.

We can remove a queued layer by dumping it to the base layer (i.e.,
applying any diff to the current base data and updating the
corresponding base chunk with that new flattened payload).  Of course,
if we're going to rewrite a base segment, we might as well absorb all
other records in that segment.  We can easily find them because
there's an in-memory table mapping keys to data locators.

Hashing helps here: the write patterns cannot create "clumps," except
for outright repeated keys, and flattening handles that.  Letting each
layer drive the rewrite victims like that evens out only thanks to
hashing.

Because we apply changes eagerly, as soon as the corresponding base
segment is rewritten, layers will often be mostly filled with
information that has already been dumped or is otherwise obsolete
(e.g., a base or diff record that has already been replaced).

The prevalence of useless records can make it hard to get a bound on
write amplification.

It's also possible to compress a few layers together such that they
contain on redundant data, and re-enter the queue as a fresh layer.
For example, we'll probably want to do that with the in-memory ingress
layer.

TBD what's the best policy here.

That's just an LSM!
-------------------

Well yes... but really specialised for the lookups performed by the
page manager: only point lookups for short keys, where the records
associated with each key span multiple kilobytes.

The lower bound on the footprint of each key is essential to the
strategy of just storing every key in an in-memory data structure.

Combined with the knowledge that we only care about point lookups by
strict equality, we can also implement a trivial version of fractional
cascading.

I think this adventure in storage system entomology is useful, because
it shows how we can decouple the write amplification properties of
LSMs from the specific associative data structure implemented with
LSMs... and how the fundamental tradeoff is additional CPU time to
interpret deltas in read operations.

Dealing with deltas
-------------------

An obvious practical problem with the page manager is that an
application might have to apply a lot of deltas whenever it accesses a
page.

We can at least simplify the workload for most external tree-shaped
data structure, where the bulk of updates target leaf nodes: we can
usually amortise full rewrites for internal nodes.  This means deltas
can correspond to LSM updates (upsert or remove the record for a key).
Since deltas don't leave the cached tier, it also "doesn't hurt" to
implement logical deltas like "increment a value."

Assuming a fully software-managed cache, we can also start caching
fully resolved pages as soon as the deltas for a page consume as
much storage as the page itself.  That imposes an upper bound on
the number of deltas one might have to apply.

It may be simpler to store the resolved page in the \\(L_0\\) log
itself.  However, each time we do, we incur another \\(1\times\\)
write *and space* amplification in the cached tier, which in turn
doubles the write amplification when promoting to the final log.

There's definitely an interesting policy question here: if promotion
to the final log is estimated to incur a \\(10\times\\) write
amplification, we'd rather GC \\(L_1\\) if it's more than 10%
redundant.  So... maybe we can still flatten deltas on disk for
heavily mutated pages?

Either way, it sounds like we want small leaf pages, to avoid having
to deal with too many deltas.  However, we want large pages to
amortise the descriptor overhead.  It sounds like we might want a van
Emde Boas-style layout trick so that one descriptor can refer to a
bundle of contiguous subpages, while letting reads focus on a specific
subpage.  That is something the application could manage, by quickly
skipping over irrelevant deltas.  However, it's not clear how best to
store and refer to subpages with deltas applied.  Another list head in
the descriptor? An array of descriptor in the log, with a shared
in-memory list head?

Also, how do we index in a bundle of subpages, when subpages can have
arbitrary sizes?  Do we just have to read the whole bundle (and a
small header for offsets)?  That's not unreasonable if we assume
bundles are only used to compact small subpages.  We might even
let the page manager bundle small pages together semi-arbitrarily!

We assume lookups in the in-memory dictionary are free, so we could
simply search for two keys: first for the subpage's id, and then for
the bundle id.  The final log only stores contiguous page bundles,
while \\(L_1\\) and \\(L_0\\) store subpages.  These younger tiers are
small (at most \\(10\%\\) of the total dataset size), so we can afford
more descriptors per bundle (i.e., more subpages per bundle).

Also, how small can the in-memory descriptor be?

With a flat array of dense page ids, the key is implicit. We could
try to do the same thing with short prefixes of hashes, but that
quickly wastes a lot of RAM (or incurs a lot of collisions).

For up to \\(2^{30}\\) pages, a 64 bit in-memory hash suffices to make
collisions rare enough that they shouldn't impact performance.
However, if the location of the value in memory tells us about the
high bits of that hash, we only need to store a small number of bits
(I think around \\(\Theta([\lg \lg n]^2)\\)) to make collisions
unlikely.

As for the locator, we minimally need a segment (file) id and an
offset.  Let's say we cap segments to 16MB (or more if we align
records to 8 or 16 bytes).  The offset needs 3 bytes.  Assuming we
only care about total footprints up to 100TB (remember, we're
designing around a cache that's \\(10\%\\) as large), we only
need 3 more bytes for the segment id.

I think even with hashing, we can safely fit descriptors in 10 bytes
(3 bytes for the partial hash, 3 bytes for the segment id, 3 bytes
offset, 1 byte size hint).  At an 80% load factor, and RAM footprint
for the dictionary equal to 1% of the data set size, we can afford
pages of 1250 bytes or more.

With a double-lookup scheme (small subpages get their own descriptor
when in the buffer), we could assume subpages are at least 250 bytes,
let subpages of 2500 bytes or more always have their own descriptor,
and otherwise group subpages of fewer than 2500 bytes in bundles of up
to 10 subpages.  In the worst case, we only have subpages in the
buffer, so \\(5\% \cdot 10\% = 0.5\%\\) of total storage for
in-memory locators to the buffer, and the base log has 9 locators
for 2500 byte pages, and 1 for a 250 byte page, so \\(10 \cdot 12.5 / (9\cdot 2500 + 250) = 0.55\%\\).  The total is \\(1.5\%\\), and we can
clearly tweak the constants a bit.

Does a read of 25KB count as one I/O? It's not far!

TODO: tweak the numbers for a max bundle size of 16KB.

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

<p><hr style="width: 50%"></p>
