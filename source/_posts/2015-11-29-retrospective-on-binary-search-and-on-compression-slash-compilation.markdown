---
layout: post
title: "Retrospective on binary search and comp{ress,ilat}ion"
date: 2015-11-29 03:25:18 -0500
comments: true
categories: 
---

I never know whether to prefer closure or fresh questions, in
research.  I got a little of both lately.  First, closure on searching
in permutations of sorted arrays, and second, more avenues on the
compilation as compression angle.

Array Layouts for Comparison-Based Searching
--------------------------------------------

In July 2012, I started really looking into searching in static sorted
sets, and found the literature disturbingly useless.  I reviewed a lot
of code, and it turned out that most binary searches out there are not
only unsafe for overflow, but also happen to be badly
micro-optimised for small arrays with simple comparators.  That lead
to [Binary Search *eliminates* Branch Mispredictions](http://pvk.ca/Blog/2012/07/03/binary-search-star-eliminates-star-branch-mispredictions/),
a reaction to popular assertions that binary search has bad constant
factors (compared to linear search or a breadth first layout) on modern
microarchitecture, mostly due to branch mispredictions.  That post has code
for really riced up searches on fixed array sizes,
so here's the size generic inner loop I currently use.

{% codeblock lang:c %}
const uint32_t x = ...;
const uint32_t *base = ...;
size_t n = ...;
size_t half;

while ((half = n / 2) > 0) {
    const uint32_t *mid = &base[half];
    base = (*mid < x) ? mid : base;
    n -= half;
}
{% endcodeblock %}

The snippet above implements a binary search, instead of [dividing by three to avoid aliasing issues](http://pvk.ca/Blog/2012/07/30/binary-search-is-a-pathological-case-for-caches/).  That issue only shows up with array sizes that are
(near) powers of two.  I know of two situations where that happens a lot:

1. bad benchmarks;
2. data structures built around powers of two (e.g., [Saxe & Bentley dynamisation](http://repository.cmu.edu/cgi/viewcontent.cgi?article=3453&context=compsci)).

The fix for the first case is to do proper benchmarking on a wide
range of input sizes.  Ternary or offset binary search are only really
useful in the second case.  There's actually a third case: when I'm
about to repeatedly search in the same array, I dispatch to unrolled
ternary searches, with one routine for each power of two.  I can
reduce any size to a power of two with one initial iteration on an
off-center "midpoint."  Ternary search has a high overhead for small
arrays, unless we can precompute offsets by unrolling the whole thing.

My work on binary search taught me how to implement binary search not
stupidly--unlike real implementations--and that most experiments on
searching in array permutations seem broken in their very design (they
focus on full binary trees).

I don't think I ever make that explicit, but the reason I even started
looking into binary search is that I wanted to have a fast
implementation of searching in a van Emde Boas layout!  However,
none of the benchmarks (or analyses) I found were convincing, and
I kind of lost steam as I improved sorted arrays: sortedness tends
to be useful for operations other than predecessor/successor search.

Some time in May this year, I found [Pat Morin's fresh effort](http://cglab.ca/~morin/misc/arraylayout-v2/) on the
exact question I had abandoned over the years: how do popular
permutations work in terms of raw CPU time?  The code was open,
and even good by research standards!  Pat had written the annoying
part (building the permutations), generated a bunch of tests I could
use to check correctness, and avoided obvious microbenchmarking
pitfalls.  He also found a [really nice way to find the return value for BFS searches](https://github.com/patmorin/arraylayout/blob/master/src/eytzinger_array.h#L137) from the location where the search ends with fast
bit operations (`j = (i + 1) >> __builtin_ffs(~(i + 1));`, which he
explains in the paper).

I took that opportunity to improve constant factors for all the
implementations, and to really try and explain in detail the
performance of each layout with respect to each other, as well as how they
respond to the size of the array.  That sparked a very interesting
back and forth with Pat from May until September (!).  Pat eventually took
the time to turn our informal exchange into a [coherent paper](http://arxiv.org/abs/1509.05053).  More than 3 years after I
started spending time on the question of array layouts for implicit
search trees, I found the research I was looking for... all it took was
a bit of collaboration (:

Bonus: the results were unexpected! Neither usual suspects (B-tree or
van Emde Boas) came out on top, even for very large arrays.  I was
also surprised to see the breadth-first layout perform much better
than straight binary search: none of the usual explanations made sense
to me.  It turns out that the improved performance (when people weren't
testing on round, power of two, array sizes) was probably an
unintended consequence of bad code!  Breadth-first is fast,
faster than layouts with better cache efficiency, because it prefetches well
enough to hide latency even when it extracts only one bit of
information from each cache line; its performance has nothing to do with
cachability.  Our code prefetches explicitly, but slower branchy
implementations in the wild get implicit prefetching, thanks to
speculative execution.

Conclusion: if you need to do a lot of comparison-based searches in >
L2-sized arrays, use a breadth-first order and prefetch.  If you need
sorted arrays, consider sticking some prefetches in a decent binary
search loop.  If only I'd known that in 2012!

Comp{ress,ilat}ion
------------------

A couple months ago, I found [LZ77-like Compression with Fast Random Access](http://www.dcc.uchile.cl/~gnavarro/ps/dcc10.1.pdf) by Kreft and Navarro.  They
describe a Lempel-Ziv approach that is similar to LZ77, but
better suited to decompressing arbitrary substrings.  The hard part about
[applying LZ77 compression to (byte)code](http://pvk.ca/Blog/2014/03/30/refactoring-with-lz77-compilation-is-compression/)
is that parses may reuse any substring that happens to appear
earlier in the original text.  That's why I had to use Jez's algorithm
to convert the LZ77 parse into a (one word) grammar.

LZ-End fixes that.

Kreft and Navarro improve random access decompression by restricting
the format of "backreferences" in the LZ parse.  The parse decomposes
the original string into a sequence of phrases; concatenating the
phrases yields the string back, and phrases have a compact
representation.  In LZ77, phrases are compressed because they refer
back to substrings in prior phrases.  LZ-End adds another constraint:
the backreferences cannot end in the middle of phrases.

For example, LZ77 might have a backreference like

    [abc][def][ghi]
       ---------

to represent "cdefg."  LZ-End would be forced to end the new phrase at "f"

    [abc][def][ghi]
       -------

and only represent "cdef."  The paper shows that this additional
restriction has a marginal impact on compression rate, and uses the
structure to speed up operations on compressed data.  (The formal
definition also forbids the cute/annoying self-reference-as-loop idiom
of LZ77, without losing too much compression power!)

We can apply the same idea to compress code.  Each phrase is now a
subroutine with a `return` at the end.  A backreference is a series
of calls to subroutines; the first call might begin in the middle, but
matches always end on `return`, exactly like normal code does!  A
phrase might begin in the middle of a phrase that itself consists of
calls.  That's still implementable: the referrer can see through the
indirection and call in the middle of the callee's callee (etc.), and
then go back to the callee for a suitably aligned submatch.

That last step looks like it causes a space blowup, and I can't bound it (yet).

But that's OK, because I was only looking into compressing traces as a
last resort.  I'm much more interested in expression trees, but
couldn't find a way to canonicalize sets (e.g., arguments to integer
`+`) and sequences (e.g., floating point `*`) so that similar
collections have similar subtrees... until I read Hammer et al's work
on [Nominal Adapton](http://arxiv.org/abs/1503.07792), which solves a
similar problem in a different context.

They want a tree representation for lists and tries (sets/maps) such
that a small change in the list/trie causes a small change in the
tree that mostly preserves identical subtrees.  They also want the
representation to be a deterministic function of the list/trie.  That
way, they can efficiently reuse computations after incremental changes
to inputs.

That's exactly my sequence/set problem!  I want a tree-based
representation for sequences (lists) and sets (tries) such that
similar sequences and sets have mostly identical subtrees for which I
can reuse pre-generated code.

Nominal Adapton uses a hash-based construction described by Pugh and
Teitelbaum in 1989 (Incremental computation via function caching) to
represent lists, and extends the idea for tries.  I can "just" use the
same trick to canonicalise lists and sets into binary trees, and
(probabilistically) get common subexpressions for free, even across
expressions trees!  It's not perfect, but it should scale pretty well.

That's what I'm currently exploring when it comes to using compression
to reduce cache footprint while doing aggressive specialisation.
Instead of finding redundancy in linearised bytecode after the fact,
induce identical subtrees for similar expressions, and directly reuse
code fragments for subexpressions.

More to come
------------

I thought I'd post a snippet on the effect of alignment and virtual
memory tricks on TLBs, but couldn't find time for that.  Perhaps later
this week.  In the meantime, I have to prepare a short talk on the
software transactional memory system we built at AppNexus.  Swing by
[23rd Street on December 15](http://meetup.com/TechTalks-AppNexus-NYC/) if you're in New York!
