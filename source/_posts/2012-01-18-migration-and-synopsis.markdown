---
layout: post
title: "Migration and Synopsis"
date: 2012-01-18 18:56
comments: true
categories: 
---

This blog has been going for five years.  Back then, it seemed like
the only widely-used static blog generators were
[Blosxom](http://www.blosxom.com/) or
[pyBlosxom](http://pyblosxom.bluesock.org/).  They weren't that hard
to set up, but getting everything _right_ rather than good enough is a
lot of work.  Latex and MathML support was also very weak, so I wound
up using a (insane) one-off hack with
[tex4ht](http://tug.org/tex4ht/).  I feel like
[Octopress](http://octopress.org/) and
[MathJax](http://www.mathjax.org/) now do everything I need out of the
box, better than anything I could design by myself.

The permalinks from the old blog are still around, but not the rss
feeds or the date-based links.

I figure this is a good opportunity to make sure the (marginally
useful) permalinks are available somewhere else than via google.

Lisp-related posts
------------------

[Another way to accumulate data in vectors](http://pvk.ca/Blog/Lisp/accumulating_data_in_vectors.html)
describes a copying-free extendable vector.  The advantage over the
usual geometric growth with copy is that the performance with respect
to the number of elements added is much smoother.  Runtimes are then
more easily predictable, and sometimes improved (e.g. right when a
copy would be needed).  It's also more amenable to a lock-free
adaptation, while preserving O(1) operation complexity (assuming that
`integer-length` on machine integers is constant time), as shown in
[Dechev et al's "Lock-free Dynamically Resizable Arrays"](http://www2.research.att.com/~bs/lock-free-vector.pdf).

[Common Cold](http://pvk.ca/Blog/Lisp/CommonCold/) is a really old
hack to get serialisable closures in SBCL, with serialisable
continuations built on top of that.  Nowadays, I'd do the closure part
differently, without any macro or change to the source.

[Concurrency with MVars](http://pvk.ca/Blog/Lisp/concurrency_with_mvars.html)
has short and simple(istic) code for
[mvars](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Concurrent-MVar.html),
and uses it to implement same-fringe with threads.

[Constraint sets in SBCL: preliminary exploration](http://pvk.ca/Blog/Lisp/constraint-sets.html)
summarises some statistics on how constraint sets (internal SBCL data
structures) are used by SBCL's compiler.

[SBCL's flow sensitive analysis pass](http://pvk.ca/Blog/Lisp/flow_sensitive_analysis_in_sbcl.html)
explores what operations on constraint sets actually mean.  This,
along with the stats from the previous post, guided a rewrite, not of
constraint sets, but of the analysis pass that uses them.  The
frequency of slow operations or bad usage patterns is reduced enough
to take care of most (all?) performance regression associated with
the original switch to bit-vector-based constraint sets, without
penalising the common case.

[Finalizing foreign pointers just late enough](http://pvk.ca/Blog/Lisp/finalizing_foreign_pointers_just_late_enough.html)
is a short reminder that attaching finalizers to system area pointers
isn't a good idea: SAPs are randomly unboxed and consed back, like
numbers.

[Hacking SSE Intrinsics in SBCL (part 1)](http://pvk.ca/Blog/Lisp/hacking_SSE_intrinsics-part_1.html)
walks through an SBCL branch that adds support for SSE operations.
Alexander Gavrilov has kept a fork on life support
[on github](https://github.com/angavrilov/sbcl-old).  There's still no
part 2, in which the branch is polished enough to merge it in the
mainline.

In the meantime,
[Complex float improvements for sbcl 1.0.30/x86-64](http://pvk.ca/Blog/Lisp/SSE_complexes.html)
built upon the original work on SSE intrinsics to implement operations
on `(complex single-float)` and `(complex double-float)` with SIMD
code on x86-64.  That sped up most complex arithmetic operations by
100%.  That work also came with support for references to unboxed
constants on x86oids; this significantly improved floating point
performance as well, for both real and complex values.

[Initialising structure objects modularly](http://pvk.ca/Blog/Lisp/modular-struct-initialisation.html)
is a solution to a problem that I hit, trying to implement non-trivial
initialisation for structures, while allowing inheritance.  Tobias
Rittweiler points out that the protocol is very similar to a common
CLOS pattern where, instead of functions that allocate objects, class
designators are passed.  It also looks a bit like the way Factor
libraries seem to do struct initialisation, but with actual
initialisation instead of assignment (which matters for read-only
slots).

[An Impure Persistent Dictionary](http://pvk.ca/Blog/Lisp/persistent_dictionary.html)
is an example of a technique I find really useful to implement
persistent versions of side-effectful data structures.  Henry Baker
has a [paper](http://home.pipeline.com/~hbaker1/ShallowArrays.html)
that shows how shallow binding can be used to implement persistent
arrays on top of functional arrays, with constant-time overhead for
operations on the latest version.  It's a really nice generalisation
of trailing in backtracking searches.  Here, I use it to get
persistent hash tables in only a couple dozen lines of code.

[Pipes](http://pvk.ca/Blog/Lisp/Pipes/) is an early attempt to develop
a DSL for stream processing, like an 80%
[SERIES](http://series.sourceforge.net/).  I've refocused my efforts
on [Xecto](http://pvk.ca/Blog/Lisp/Xecto/), which only handles
vectors, rather than potentially unbounded streams.  The advantage is
that Xecto looks like it has the potential to be simpler while
achieving near-peak performance to me; the main downside is that
vectors don't allow us to represent control flow as data via lazy
evaluation... and I'm not sure that's such a bad thing.

The post on
[string-case](http://pvk.ca/Blog/Lisp/string_case_bis.html) is an
overview of how I structured a CL macro to dispatch that compares with
`string=` instead of `eql`.  If I were to do this again, I'd probably
try and improve `string=`; I later tested an SSE comparison routine,
and it ended up being, in a lot of cases, faster and simpler (with a
linear search) than the search tree generated by `string-case`.

[The type-lower-bound branch](http://pvk.ca/Blog/Lisp/type_lower_bound.html)
describes early work on a branch that provides a way to shut the
compiler up about certain failed type-directed optimisations.  A lot
of the output from SBCL's compiler amounts to reports of optimisations
that couldn't be performed (e.g. converting multiplication by a
constant power of two to a shift), and why (e.g. the variant argument
isn't known to be small enough).  Sometimes, there's nothing we can do
about it: we can't show the compiler that the argument is small enough
because we know that it will sometimes be too large!  Yet, CL's type
system (like most) does not let us express that information.
Programmers are expected to provide upper bounds on the best static
type of values (e.g. we can specify that a value is always a `fixnum`,
although it may really only be integers between 0 and 1023).  We would
like a way to specify lower bounds as well: "I know that this will
take arbitrary `fixnum` values."  Once we have that, the compiler can
skip reporting optimisations that we know can't be performed (as
opposed to those we don't know whether they can be performed).

Finally,
[Yet another way to fake continuations](http://pvk.ca/Blog/Lisp/yet_another_way_to_fake_continuations.html)
sketches a simple but somewhat inefficient way to implement
continuations for pure programs.  It may be useful for IO-heavy
applications (web programming), or in certain cases similar to
backtracking search, but in which most of the work is performed
outside of backtracking (e.g. during constraint propagation).

General low-level programming issues
------------------------------------

[SWAR implementation of (some #'zerop ...)](http://www.pvk.ca/Blog/LowLevel/SWAR-some-zerop.html)
sketches how we can use SIMD-within-a-register techniques to have fast
search for patterns of sub-word size.  A degenerate case is when we
look for 0 or 1 in bit vectors; in these case, it's clear how we can
test whole words at a time.  The idea can be extended to testing
vectors of 2, 4, 8 (or any size) -bit elements.  I haven't found time
to move this in SBCL's runtime library (yet), but it would probably
be a neat and feasible first project.

[Revisiting VM tricks for safepoints](http://www.pvk.ca/Blog/LowLevel/VM_tricks_safepoints.html)
explores the performance impact of switching from instrumented
pseudo-atomic code sequences to safepoints.  The bottom line is that
it's noise.  However, some members of the russian Lisp mafia have used
it as inspiration, and have managed to implement seemingly solid
[threaded SBCL on Windows](https://github.com/akovalenko/sbcl-win32-threads/wiki)!
It's still a third-party fork for now, but some committers are working
on merging it with the mainline.

[Fast Constant Integer Division](http://www.pvk.ca/Blog/LowLevel/fast-integer-division.html)
has some stuff on integer division by constants.  It's mostly
superseded by Lutz Euler's work to implement the same algorithm as
GCC.  There are some interesting identities that can be used to
improve on that algorithm a tiny bit and, more interestingly, to
implement truncated multiplication by arbitrary fractions.  I only
stumbled upon those a long time after I wrote the post; I'll try and
come back to this topic in the coming months.

[There's more to locality than caches](http://www.pvk.ca/Blog/LowLevel/more_to_locality_than_cache.html)
tracks my attempts to understand why a data structure designed to be
cache-efficient did not perform as well as expected.  It turns out
that cache lines aren't exactly read atomically (so reading two
adjacent addresses may be significantly slower than only one), and
that sometimes L2 matters less than TLB.  The latter point was an
important lesson for me.  TLBs are used to accelerate the translation
of virtual addresses to physical; _every_ memory access must be
translated.  TLBs are usually fully associative (behave like
content-addressed memory or hash tables, basically), but with a small
fixed size, on the order of 512 pages for the slower level.  With
normal (on x86oids) 4KB pages, that's only enough for 2 MB of data!
Even worse: a cache miss results in a single access to main memory,
which is equivalent to ~60-100 cycles at most; a TLB miss, however,
results in a lookup in a 4 level page table on x86-64, which often
takes on the order of 2-300 cycles.  Luckily, there are workarounds,
like using 2 MB pages.

[Napa-FFT(2) implementation notes](http://www.pvk.ca/Blog/LowLevel/napa-fft2-implementation-notes.html)
is where I try to make the code I wrote for a Fast Fourier transform
understandable, especially _why_ it does what it does.  Napa-FFT and
Napa-FFT2 are vastly faster than Bordeaux-FFT (and than all other CL
FFT codes I know, on SBCL), but it's still around 20-50% slower than
the usual benchmark, FFTW.  Napa-FFT3 is coming, and it's a completely
different approach which manages to be within a couple percent points
of FFTW, and is faster on some operations.

[0x7FDE623822FC16E6 : a magic constant for double float reciprocal](http://www.pvk.ca/Blog/LowLevel/software-reciprocal.html)
is a surprisingly popular post.  I was trying to approximate
reciprocals as fast as possible for a mathematical optimization
method.  The usual way to do that is to use a hardware-provided
approximation and then improve it with a couple iterations of Newton's
method.  The post shows how we can instead use the way floats are laid
out in memory to provide a surprisingly accurate guess with an integer
subtraction.  I actually think the interesting part was that it made
for a practical use case for the golden section search...

[Some notes on Warren](http://www.pvk.ca/Blog/LowLevel/some-notes-on-warren.html)
has a couple notes about stuff in Warren's book
[Hacker's Delight](http://www.hackersdelight.org/).  The sign
extension bit probably deserves more attention; it seems like someone
on #lisp asks how they can sign-extend unsigned integers at least once
a month.

[Two variations on old themes](http://www.pvk.ca/Blog/LowLevel/two-neat-tricks.html)
has some stuff on Linux's ticket spinaphores, and is the beginning of
my looking into Robin Hood hashing with linear probing for
cache-friendly hash tables.

[Interlude: Numerical experiments in hashing](http://www.pvk.ca/Blog/numerical_experiments_in_hashing.html)
covers a first stab at designing a hash table that exploits cache
memory.  2-left hashing looks interesting, but its performance was
worse than expected, for various reasons, mostly related to the fact
that caches can be surprisingly complicated.  Two years later,
[More numerical experiments in hashing: a conclusion](http://www.pvk.ca/Blog/more_numerical_experiments_in_hashing.html)
revisits the question, and settles on Robin Hood hashing with linear
probing.  It's a tiny tweak to normal open addressing (insertions can
bump previously-inserted items farther from their insertion point),
but it suffices to greatly improve the worst and average probing
length, while preserving the nice practical characteristics of linear
probing.  I've also started some work on implementing SBCL's hash
table this way, but there are practical issues with weak hash
functions, GC and mutations.

Miscellaneous stuff
-------------------

In
[Specify absolute deadlines, not relative timeouts](http://www.pvk.ca/Blog/Coding/deadline-vs-timeout.html)
and
[the sequel](http://www.pvk.ca/Blog/Coding/deadline-vs-timeout-part-2.html),
I argue that we should have interfaces that allow users to specify an
absolute timeout, with respect to a monotonic clock.  Timeouts are
convenient, but don't compose well: how do we implement a timeout
version of an operation that sequences two calls to functions that
only offer timeouts as well?  Any solution will be full of race
conditions.  PHK disagrees; I'm not sure if all of his complaints can
be addressed by using a monotonic clock.

Finally,
[Space-complexity of SSA in practices](http://www.pvk.ca/Blog/Implementation/SSA_in_practices.html)
has some early thoughts on how Static single assignment scales for
typical functional programs.  It's fairly clear that many compilers
for functional languages have inefficient (wrt to compiler
performance) internal representations; however, it's not as clear that
the industry standard, SSA, would fare much better.
