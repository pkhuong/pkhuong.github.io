---
layout: post
title: "EGCD doesn't solve integer division by constants, but it helps"
date: 2012-11-22 14:11
comments: true
published: false
categories: 
---

Techniques that strength-reduce integer divisions by constants into
sequences of simple operations -- multiplications, additions and
shifts -- are really interesting to me: they're useful, because
integer division is so slow, but constant divisors relatively common,
and the problem exploits both assembler-level micro-optimisation and
fun number (or group) theory.

There is a lot of depth to the optimisation, yet I regularly (once or
twice a year) hear brilliant people claim that finding constants for
division-by-multiplication is just a straightforward application of
the extended GCD.  Apart from one well-known special case, this is
false.  However, examining that special case leads to nice, widely
applicable, improvements to popular generic approach.

This is how SBCL recently gained full support for integer division by
constants (floor, truncate and ceiling, signed and unsigned),
including special tricks for tagged arithmetic.  Arguably, it's also
different from, but competitive with, the implementations in GCC and
clang.

Modular inverses versus division
================================

Introductory courses on group theory or number theory present the
[EGCD algorithm](http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm)
(along with [Bézout's identity](http://en.wikipedia.org/wiki/B%C3%A9zout's_identity)) as a simple and efficient way to compute multiplicative inverses in
\\(\mathbb{Z}/n\mathbb{Z}\\) (arithmetic modulo n), or prove their absence.

For example, Euclid's algorithm let us determine that 3 and 16 are
coprime. Therefore, there exists an integer (a class of integers)
\\(i\\) such that \\(3i \equiv 1 \mod 16\\).  We can easily brute force
this, and find \\(11\cdot 3 = 33 \equiv 1 \mod 16\\), but the extended GCD
is asymptotically much more efficient. The idea is then to consider
machine arithmetic with wraparound as arithmetic modulo \\(2\sp{w}\\),
where \\(w\\) is the word size in bit.

Let's test this multiplicative inverse.

\\(i = 11\\), and 
\\[6\cdot i = 66 \equiv 2 \mod 16,\\]
which is indeed 6/3!

\\[5\cdot i = 55 \equiv 7 \mod 16,\\]
and something's obviously not working: 7
is nothing like a truncated or rounded value of 5/3.

A more algebraic approach would be to simply point out that \\(d\\)
has a multiplicative inverse \\(i \in \mathbb{Z}/n\mathbb{Z}\\) iff
\\(i\\) and \\(d\\) are both coprime with \\(n\\), and that
multiplication by \\(i\\) is thus a bijection, while truncated
division is surjective. Multiplication by the multiplicative inverse
in \\(\mathbb{Z}/n\mathbb{Z}\\) only corresponds to truncated division
iff the division is remainder-free, i.e. when the dividend is a
multiple of the divisor. In that case, we have \\(x = 3q\\), for
example, and \\(11x = 33q \equiv q \mod 16\\).

At least, this situation is actually fairly common: subtracting two
pointers involves an exact division by the pointed type's size to
compute the difference in elements rather than in bytes (`char`)…
Except for one detail. The divisor d has a multiplicative inverse in
\\(\mathbb{Z}/n\mathbb{Z}\\) only if \\(d\\) and \\(n\\) are coprime,
and machine arithmetic is usually performed modulo \\(2\sp{w}\\). In
that case, the multiplicative inverse exists if and only if \\(d\\),
the element size, is odd.  This additional condition isn't as commonly
satisfied.

We can fix this! Exact division by \\(d\cdot 2\sp{k}\\) (\\(d\\) is
odd) can be performed in two steps: first, divide by \\(2\sp{k}\\)
(with a right shift), and then divide by \\(d\\), with a
multiplication by its multiplicative inverse modulo \\(2\sp{w}\\).

This seems straightforward enough, except that it's not obvious what
happens with negative values.  The beauty of two's complement
representation is that it is equivalent to modular arithmetic.  With
\\(w\\) bit per word, negative values like \\(-3\\) are represented as
\\(2\sp{w}-3\\), which happens to be equivalent to \\(-3\\) modulo
\\(2\sp{w}\\)!  Thus, we can treat signed and unsigned values with
\\(w\\) bit words the same, with multiplicative inverses modulo
\\(2\sp{w}\\) (arithmetic shifts are designed to handle negative
inputs correctly).

For example, -3 in two's complement, with a word-width of 4
(i.e. modulo 16) is 13; we find
\\[11\cdot 13 = 143 \equiv 15 \equiv-1\mod 16.\\]

We've successfully used multiplicative inverse of 3 modulo 16 to
divide signed and unsigned multiples of 3, by performing the
computations on 4 bits.

Note that this obliviousness is an essential reason why two's
complement is interesting: unless we are interested in double-width
results, we can use the same circuits for addition, subtraction and
multiplication, regardless of signedness.

In short, multipliers computed with the EGCD algorithm may only be
used to convert divisions into multiplications when the dividend is a
multiple of the divisor, and an additional right shift is needed to
handle any power of two in the divisor's factors.

Handling the general case
=========================

The classic methods to quickly emulate integer division by constants
(that I know of) are based on approximating the reciprocal \\(1/d\\)
with a fraction of the form \\(m/2\sp{k}\\).  Such fractions are
interesting because (arithmetic) right shifts are equivalent to
floored divisions by powers of two.

Over-approximation the reciprocal, such that \\(m/2\sp{k} \geq 1/d\\),
lets us compute truncated divisions (rounded toward zero).

Under-approximating the reciprocal, and then compensating with an
addition, instead lets us compute floored divisions (rounded toward
negative infinity).

In C, integer division is specified to always be a truncated division;
Common Lisp, however, natively offers floored, truncated and ceilinged
divisions.

Regardless of the language, both approximations are useful: truncated
and floored divisions coïncide for nonnegative (unsigned) integers.
This is quite the happy accident, as it affords us additional freedom
in exactly the most difficult case.

Over-approximating reciprocals
------------------------------

Let's say we have a divisor \\(d > 0\\) and constants \\(m\\) and \\(k\\)
such that

\\[\frac{m}{2\sp{k}} = \frac{1}{d}+\epsilon,\\]

with \\(\epsilon>0\\).

For example, \\(1/3\\) could be approximated with \\(3/8\\), as in the
following drawing.

On nonnegative inputs, we're only interested in the integer part, for
integer inputs.  Intuitively, it looks like the error for the
approximated product should be small enough (on the order of
\\(1/3\\)) for the final result to be exact.

For negative inputs, it's slightly more complicated: the approximated
function rounds toward zero, but the approximation rounds down (toward
minus infinity).  However, as the drawing shows, it looks like the
approximation is always one less than the goal (truncated division)
for negative values.  That makes sense: the error is strictly
positive, and thus negative outputs are slightly too negative before
flooring.  After flooring, that error is amplified into an offset by
one.

More formally,

\\[\left\lfloor \frac{xm}{2\sp{k}}\right\rfloor = \left\lfloor \frac{x}{d}+x\epsilon\right\rfloor = q + \lfloor r/d + x\epsilon\rfloor,\\]
where \\(x = qd+r\\), \\(0 \leq r < d\\).

Thus, for \\(x\in \mathbb{N}\\), the approximation \\(m/2\sp{k}\\) is
correct as long as \\(x\epsilon < 1/d\\) (in which case the last
term floors to zero).

Negative integers are slightly more complicated.

When \\(|x\epsilon| < 1/d\\) and \\(x\\) is a multiple of \\(d\\), it
suffices to increment the result of the floor by one to find
\\(x/d\\).  When \\(x\\) isn't a multiple of \\(d\\), incrementing
\\(\lfloor x/d\rfloor\\) by one yields the truncated division.

Thus, over-approximating (more precisely, rounding away from zero)
\\(1/d\\) lets us compute a truncated division, by incrementing the
final result only when it's negative.

How do we minimise the error \\(\epsilon\\)? Once \\(k\\) is fixed, it
suffices to let \\(m = \lceil 2\sp{k}/d\rceil\\), with \\(0\leq
\epsilon < 2\sp{-k}\\).  If \\(d\\) is negative, the floor must
instead be taken.  I'm assuming that \\(d\\) isn't a power of two, as
these divisors can be handled more directly, and doing so eliminates
fencepost issues.

If \\(|x| \leq 2\sp{w}\\) (a strict inequality doesn't really help), we
must have \\(|x|\epsilon < 1/d\\). Given that \\(\epsilon < 2\sp{k}\\),
it suffices to show that \\(|x|/2\sp{k} \leq 1/d\\).  Let 
\\[k = w + \lceil \lg d \rceil.\\]
We have \\(|x|/2\sp{k} \leq 1/2\sp{k} \leq 1/d\\), and the approximation
is thus safe.

However, letting \\(k = w+\lceil \lg d \rceil\\) can lead to pretty
large multipliers \\(m\\): the only bound is
\\[\left\lceil\frac{2\sp{w+\lceil\lg d\rceil}}{d}\right\rceil < 2\sp{w+1}.\\]
Thus, in the worst case, we may have to multiply by a \\((w+1)\\)-bit
constant to over-approximate the reciprocal with sufficient precision.
Intuitively, this happens when rounding up introduces a lot of error,
i.e. \\(d\\) is nearly a power of two.  Most of the time, \\(w\\) bits
(or even fewer) suffice.

Under-approximating reciprocals
-------------------------------

