---
layout: post
title: "Some scalar SSE2 pitfalls"
date: 2013-06-07 18:01
comments: true
categories: 
published: false
---

I recently discovered that OCaml exhibited
[code generation issues](http://caml.inria.fr/mantis/print_bug_page.php?bug_id=5180)
on x86-64 that also plagued SBCL until 2009.  Hopefully, this post
will help others avoid what seem to be common mistakes.

Contemporary x86oids all support SSE2 and deprecate x87, as SSE2 has
all the basic scalar (single and double precision) floating point
operations; in fact, SSE2 instructions are mandatatory for x86-64
chips.

x87 is "quirky."  However, its quirks are obvious: RPN with a spacious
8-value stack, and 80 bit floats internally.  By now, workarounds are
also very well known: swap values to the top of stack to simulate a
register file (especially now that swaps are nearly free when combined
with arithmetic), and either change the precision flag or round-trip
temporary values through memory.

