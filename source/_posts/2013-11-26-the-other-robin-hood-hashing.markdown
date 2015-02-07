---
layout: post
title: "The other Robin Hood hashing"
date: 2013-11-26 01:35
comments: true
categories: 
---

Emmanuel Goossaert recently
[played with Robin Hood linear probing](http://codecapsule.com/2013/11/11/robin-hood-hashing/)
and, after failing to reproduce
[some of my results](http://www.pvk.ca/Blog/more_numerical_experiments_in_hashing.html),
emailed me for advice or comments.  The discussion lead to a
[small tweak to his deletion routine](http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/),
after which we declared replication successful.

I think the underlying confusion is frequent; I've seen it in a
couple discussions where people report conflicting experiences with
Robin Hood hashing.

The scheme described in
[Pedro Celis's dissertation (PDF)](https://cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf)
is a variant of double hashing.  Hashing is based on simulating random
allocation with a hash function; double hashing asks for a second hash
function to simulate independent random probing sequences for each
key.  This is really useful to reduce the worst-case probe sequence
length, and Robin Hood hashing further improves the distribution's
tail by reducing the variance: better locations are given to far-off
entries after bumping out well-off ones, hence the name.

Twenty-five years later, I don't think any variant of double hashing
makes sense.  If we allow multiple hash functions and reads to
uncorrelated addresses (and writes that bump entries out), we might as
well go for simpler strategies with awesome worst-case bounds, like
2-left or cuckoo hashing.  Then again, I'm not convinced that these
schemes are useful in software either: we can do a lot of probing
in the time it takes to read a second random address [*].  In
hardware, the trade-offs seem completely different; I wouldn't be
surprised if two 128 KB SRAM chips were cheaper than a single 256 KB
chip.

[*] We could hope for memory-level parallelism… but, in my experience,
TLB misses are where (uncached) hash lookups hurt, and those don't
seem to be resolved in parallel.  In cache, linear probing eliminates
bank conflicts and is really quick.

When I looked into Robin Hood hashing, I was interested in a
degenerate variant in which probing sequences are linear.  This
variant also seems the one others usually mean when they say "Robin
Hood hashing," nowadays.  The algorithms for inserts and lookups in
Celis's dissertation still work: probe locations and distances just
happen to be computed trivially.  However, the analysis doesn't hold
anymore.  This is where later work like Viola's
[Distributional analysis of Robin Hood linear probing hashing with buckets](http://www.dmtcs.org/dmtcs-ojs/index.php/proceedings/article/viewArticle/dmAD0127)
and the
[associated journal paper](http://www.dmtcs.org/dmtcs-ojs/index.php/dmtcs/article/viewArticle/1359)
comes in.  The bounds are worse than with (pseudo)random probing
sequences, but each probe is a lot quicker.

Such weaker bounds also mean that Celis's suggestion of using
tombstone (markers for deleted entries) doesn't perform as well.
Linear probing clumps up, even with Robin Hood rebalancing.  Tombstones
can be cleared out with a full rehash after \\(\Theta(n)\\)
operations, or deletion instead implemented by copying later elements
backwards.  I'm a fan of the latter option: when ties are broken
deterministically (e.g., by comparing keys), the layout of the hash
table is independent of the sequence of insertions and deletions
executed to get there.

When there's a disagreement over the performance of Robin Hood
hashing, it may help to specify the probing sequence.  Older
(pre-2000) references and experiments probably refer to the original
twist on double hashing; newer ones may instead describe the linear
probing variant.  Robin Hood double hashing seems obsolete, and the
linear probing variant isn't what Celis described.  It may be more
appropriate to refer to the latter as "Robin Hood linear probing."
