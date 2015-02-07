---
layout: post
title: "An unified framework for integer division by constants"
date: 2014-03-11 18:41
comments: true
published: false
categories: 
---

Integer division in Common Lisp
===============================

Integer division in C (and in most language with integer division)
truncates, i.e., round toward zero.  Programmers can implement other
rounding choices on top of that… and handling overflows correctly is
paindul.

CL instead makes the choice explicit to the programmer: do they want
to `truncate`, `floor`, or `ceiling`?  This means more work for
implementors, but also much less for programmers.  The design choice
is an OAOO win and exposes better optimisation opportunities than the
alternatives -- copy/pasting a piece of code (that's often incorrect),
or an opaque call to a third-party library.

What I find interesting is that trying to support all three division
functions helped me unify all the approaches for truncated division
that I know of.  The unification also shows how to improve error bound
for interesting common cases, and thus get away with simpler code
sequences.

First, the easy cases
=====================

Some of the strength reduction patterns in the sequel don't work for
the trivial cases , and all are more complicated than necessary when 
