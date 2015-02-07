---
layout: post
title: "Integer division, step 0: no remainder"
date: 2014-05-11 01:25
comments: true
categories: 
---

Exciting times in SBCL-land!  Not only will Google Summer of Code
support two students to work on SBCL (one will improve our support for
correct Unicode manipulation, and the other our strength reduction for
integer division), but we also sprouted a
[new Linux/ARM port](http://sourceforge.net/p/sbcl/sbcl/ci/b793e2b291ee55ec43399d0367f59d6a56b27433/tree/NEWS?diff=2971347c95b8910019f43a0ba6de7b2e36997c85)!
As
[Christophe](http://christophe.rhodes.io/notes/blog/posts/2014/just_like_old_times/)
points out, this a nice coincidence: (most?) ARM chips lack hardware
integer division units.  I find the integer division project even more
interesting because I believe we can cover all three standard division
operators (floor, truncate, and ceiling) with an unified code
generator.

I first looked into integer division by constants four years ago, and
I was immediately struck by the ad hoc treatment of the
transformation: I have yet to find a paper that summarises and relates
algorithms that are currently in use.  Worse, the pseudocode tends to
assume fixed-width integer, which drowns the interesting logic in
bignum-management noise.  Back when I had free time, I uploaded
[an early draft](http://discontinuity.info/~pkhuong/fast-truncate-4.pdf)
of what may become a more enlightening introduction to the topic.  My
goal was to unite all the simplification algorithms I'd seen and
to generalise them to SBCL's needs: our optimisers benefit from
precise integer range derivation, and codegen ought to deal with
tagged fixnums.  The draft should take shape as the GSoC
project progresses.

There is one widespread -- but very specialised -- integer division
algorithm that does not fit in the draft: multiplication by modular
inverses.  I'm guessing it's common because it's the first thing that
comes to mind when we say division-by-multiplication.  The
transformation is also so specialised that I find it's usually
mentioned in contexts where it wouldn't work.  Still, it's a nice
application of algebra and the coefficients are simple enough to
generate at runtime (even in C or assembly language), so here goes.

Multiplicative inverses for integer division
============================================

Let \\(a\\) and \\(m\\) be naturals.  The multiplicative inverse of
\\(a\\) modulo \\(m\\) is a natural \\(b\\) such that 
\\(a \times b \equiv 1 \mod m\\).  Machine arithmetic is naturally modular 
(e.g., mod \\(m = 2\sp{32}\\)).  This seems perfect!

There are a couple issues here:

1. we have to find the modular inverse;
2. the modular inverse only exists if \\(\mathop{gcd}(a, m) = 1\\);
3. multiplicative inversion and integer division only coincide when
the remainder is zero.

For a concrete example of the third issue, consider
\\(11\\), the multiplicative inverse of \\(3 \mod 16\\): \\(3\times 11
= 33 \equiv 1 \mod 16\\) and \\(6 \times 11 = 66 \equiv 2 \mod 16\\).
However, \\(4 \times 11 = 44 \equiv 12 \mod 16\\), and \\(12\\) is
nowhere close to \\(4 \div 3\\).

This post addresses the first two points.  There is no workaround
for the last one.

We can generate a modular inverse with the
[extended Euclidean algorithm](http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm).
Wikipedia shows the iterative version, which I can never remember, so I'll
instead construct the simple recursive one.

We already assume that
\\[\mathop{gcd}(a, b) = 1 \\]
and we wish to find \\(x, y\\) such that
\\[ax + by = 1.\\]
[Bézout's identity](http://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity)
guarantees that such coefficients exist.

Things are simpler if we assume that \\(a < b\\) (they can only be
equal if \\(a = b = 1\\), and that case is both annoying and
uninteresting). 

If \\(a = 1\\), \\(a + b0 = 1\\).

Otherwise, let \\(q = \lfloor b/a\rfloor\\) and \\(r = b - qa\\).
\\[\mathop{gcd}(a, r) = \mathop{gcd}(a, b) = 1,\\]
and, given
\\[ax' + ry' = 1,\\]
we can revert our change to find
\\[ax' + (b - qa)y' = a(x' - qy') + by' = 1.\\]

We're working in modular arithmetic, so we can sprinkle `mod m`
without changing the result.  In C, this will naturally happen for
unsigned integers, via overflows.  In CL, we can still force modular
reduction, just to convince ourselves that we don't need bignums.

{% codeblock %}
(defun inverse (a m)
  (labels ((egcd (a b)
             (cond ((= 1 a)
                    (values 1 0))
                   ((> a b)
                    (multiple-value-bind (y x)
                        (egcd b a)
                      (values x y)))
                   (t
                    (multiple-value-bind (q r)
                        (truncate b a)
                      (multiple-value-bind (y x)
                          (egcd r a)
                        (values (mod (- x (* y q)) m)
                                y)))))))
    (let* ((x (egcd a m))
           (i (if (< x 0) (+ x m) x)))
      ;; i better be a's inverse...
      (assert (= 1 (mod (* a i) m)))
      i)))
{% endcodeblock %}

And a quick sanity check:

    CL-USER> (loop for m from 2 upto (ash 1 10)
                   do (loop for i from 1 below m
                            when (= 1 (gcd i m))
                            do (inverse i m)))
    NIL ; no assertion failure

The second issue is that the multiplicative inverse only exists if our
divisor and our modulo (e.g., \\(2\sp{32}\\)) are coprime.  The good
news is that \\(\mathop{gcd}(a, 2\sp{w})\\) can only be a power of
two.  We only have to factor our divisor \\(a = 2\sp{s} v\\), and find
\\(i\\), the multiplicative inverse of \\(v\\).  Division by \\(a\\)
is then a right shift by \\(s\\) and a multiplication by \\(i\\).


{% codeblock %}
(defun trailing-zeros (x)
  "Return the largest integer s such that 2^s | x"
  (assert (plusp x))
  (1- (integer-length (logxor x (1- x)))))

(defun divisor (d m)
  (let* ((zeros (trailing-zeros d))
         (inverse (inverse (ash d (- zeros)) m)))
    (lambda (x)
      (mod (* (ash x (- zeros)) inverse) m))))
{% endcodeblock %}

And now, a final round of tests:

    CL-USER> (defun test-divisor (d m)
               (let ((divisor (divisor d m)))
                 (loop for i upfrom 0
                       for j from 0 by d below m
                       do (assert (= (funcall divisor j) i)))))
    TEST-DIVISOR
    CL-USER> (loop for width from 1 upto 20
                   for mod = (ash 1 width)
                   do (loop for x from 1 below mod
                            do (test-divisor x mod)))
    NIL

A simple transformation from integer division to shift and
multiplication… that works only in very specific conditions.

What are modular inverses good for, then?
=========================================

I've only seen this transformation used for pointer subtractions in
C-like languages: machines count in chars and programs in
whatever the pointers point to.  Pointer arithmetic is only
defined within the same array, so the compiler can assume that the
distance between the two pointers is a multiple of the object size.

The following program is deep in undefined behaviour, for
example.

{% codeblock foo.c %}
#include <stdio.h>

struct foo {
	char buffer[7];
};

int main(void)
{
	struct foo *x = (struct foo *)0;
	struct foo *y = (struct foo *)9;

	printf("%zd %i\n", y - x, y < x);
	return 0;
}
{% endcodeblock %}

    pkhuong:tmp pkhuong $ clang foo.c && ./a.out
    -2635249153387078801 0

What I find interesting is that, if we pay attention to the
correctness analysis, it's clear that general div-by-mul
transformations benefit from known common factors between the divisor
and the dividend.  In the extreme case, when the dividend is always a
multiple of the divisor, we can convert the division to a single
double-wide multiplication, without any shift or additional multi-word
arithmetic.  On architectures with fast multipliers or ones that let
us compute the high half of product without the low part, the general
case (coupled with a tight analysis) may be marginally quicker than
this specialised transformation.  Yet, both GCC and clang convert
pointer subtractions to shifts and multiplications by modular
inverses.

In the end multiplicative inverses seem mostly useful as a red
herring, and as a minimal-complexity low hanging fruit.  The only
reason I use them is that it's easy to generate the coefficients in C,
which is helpful when allocation sizes are determined at runtime.
