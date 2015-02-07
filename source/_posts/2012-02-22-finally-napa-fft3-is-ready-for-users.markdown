---
layout: post
title: "Finally! Napa-FFT3 is ready for users"
date: 2012-02-22 20:32
comments: true
categories: 
---

[Napa-FFT3](https://github.com/pkhuong/Napa-FFT3) is in the latest
[Quicklisp](http://www.quicklisp.org/beta/) release.  Unlike previous
attempts that were really proofs of concept, this one feels solid
enough for actual use.

This third version is extremely different from the first two: rather
than trying to compute in-order FFTs without blowing caches, it
generates code for bit-reversed FFTs.  The idea came from
[Brad Lucier](http://www.math.purdue.edu/~lucier/), who sent me a
couple emails and showed how nicely his FFT scaled (it's used in
[gambit](http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Main_Page)'s
bignum code).  Bit-reversed FFTs don't have to go through any special
contortion to enjoy nice access patterns: everything is naturally
sequential.  The downside is that the output is in the wrong order (in
[bit-reversed](http://en.wikipedia.org/wiki/Bit-reversal_permutation)
order).  However, it might still be an overall win over directly
computing DFTs in order: we only need to execute one bit-reversal
pass, and we can also provide FFT routines that work directly on
bit-reversed inputs.

My hope when I started writing Napa-FFT3 was that I could get away
with a single generator that'd work well at all sizes, and that
bit-reversing would either not be too much of an issue, or usually not
needed (e.g., for people who only want to perform convolutions or
filtering).

Overview of the code
--------------------

The
[forward](https://github.com/pkhuong/Napa-FFT3/blob/master/forward.lisp)
and
[inverse](https://github.com/pkhuong/Napa-FFT3/blob/master/inverse.lisp)
transform generators are pretty simple implementations of the
[split-radix FFT](http://en.wikipedia.org/wiki/Split-radix_FFT_algorithm).

Generator for "flat" base cases output code for a specialised compiler
geared toward large basic blocks.  The specialised compiler takes
potentially very long traces of simple operations on array elements,
and performs two optimisations: array elements are cached in variables
(registers), and variables are explicitly spilled back into arrays,
following [Belady's algorithm](http://en.wikipedia.org/wiki/Cache_algorithms#B.C3.A9l.C3.A1dy.27s_Algorithm).  That allows us to easily exploit the
register file, without taking its size directly into account in the
domain-specific generators, and even when we have to cope with a
relatively naïve machine code generator like SBCL's.

Larger input sizes instead use a generator that outputs almost-normal
recursive code; there's one routine for each input size, which helps
move as much address computation as possible to compile-time.

Even with code to handle scaling and convolution/filtering, I feel
that the generators are easily understood and modified.  They
currently only support in-order input for the forward transform, and
in-order output for the inverse, but the generators are simple enough
that adding code for all four combinations (in-order input or output,
forward or inverse transform) would be reasonable!  I believe that's a
win.

Better: it seems my hope that we can execute bit reverses quickly was
more than justified.  I'm not quite sure how to describe it, but the
[code](https://github.com/pkhuong/Napa-FFT3/blob/master/bit-reversal.lisp)
is based on recursing on the indices from the middle bits toward the
least and most significant bits.  The result is that the there's
exactly one swap at each leaf of the recursion, and that, when cache
associativity is high enough (as is the case for the x86 chips I use),
all the cache misses are mandatory.  Better, the recursiveness ensures
that the access patterns are also TLB optimal, when the TLB
associativity is high enough (or infinite, as for my x86oids).

There's one issue with that recursive scheme: it's really heavy in
integer arithmetic to compute indices.  Again, I generate large basic
blocks to work around that issue.  The last couple levels (three, by
default) of the recursion are unrolled and compiled into a long
sequence of swaps.  The rest of the recursion is executed by looping
over a vector that contains indices that were computed at
compile-time.

Correctness
-----------

I have a hard time convincing myself that *code generators* are
correct, especially without a nice static type system.  Instead, I
heavily tested the final generated code.  I'm using Common Lisp, so
array accesses were all checked automatically, which was very useful
early in the development processes.  Once I was convinced certain that
all accesses were correct, I turned bound and type checking off.  The
[first test file](https://github.com/pkhuong/Napa-FFT3/blob/master/ergun-test.lisp)
implements a set of randomised tests proposed by
[Funda Ergün](http://www.cs.sfu.ca/~funda/publications.html).  That
was enough for me to assume that the FFTs themselves were correct.  I
then turned to a
[second set of tests](https://github.com/pkhuong/Napa-FFT3/blob/master/tests.lisp)
to try and catch issues in the rest of the code that builds on
straight FFTs.

The process did catch a couple bugs, and makes me feel confident
enough to let other people use Napa-FFT3 in their programs.

Performance
-----------

Napa-FFT and Napa-FFT2 managed to come reasonably close to FFTW's
performance.  When I started working on Napa-FFT3, I hoped that it
could come as close, with much less complexity.  In fact, it performs
even better than expected: Napa-FFT3 is faster than Napa-FFT(2) at
nearly all sizes, and outperforms FFTW's default planner for
out-of-cache transforms (even with the bit-reversal pass).
