---
layout: post
title: "A one-instruction write barrier"
date: 2012-08-14 21:25
comments: true
categories: Puzzle
---
[Hölzle's two-instruction write barrier [PDF]](http://www.hoelzle.org/publications/write-barrier.pdf)
for garbage collectors looks like

     addr = destination
     offset = addr>>k;
     (cards-(heap_base>>k))[offset] = 1 ; mark one byte
     write to addr

Some SBCL users allocate Lisp object lookalikes in the C heap, and we
have stack-allocated values; I have to test whether the address is in
range or mask the offset to avoid overflows.

Or, we could exploit X86's bit-addressing instructions:

     addr = destination
     bts cards, addr
     write to addr

where `cards` is a vector of 256 or 512MB (there's some trickery to handle
negative offsets). `bts` will index into that vector of 4G bits, and
set the corresponding bit to 1.  On X86-64, we can force `cards` to be
in the lower 4GB, and stick to 32 bit addressing: the instruction will
also implicitly mask out the upper 32 bit of `addr` before indexing
into `cards`.  Too bad it's around twice or thrice as slow as a shift
and a byte write (or even shift, mask and byte write) and really sucks
with SMP.

There are also [hacks [PDF]](<http://weinholt.se/scheme/alignment-check.pdf)
to
[abuse](https://groups.google.com/d/msg/comp.lang.lisp/qQdpmfHhJj8/43LfCzBiCJAJ)
alignment checking as hardware lowtag (tag data in the lower bit of
addresses) checks.  Who says that contemporary machines don't support
safe languages well? (:
