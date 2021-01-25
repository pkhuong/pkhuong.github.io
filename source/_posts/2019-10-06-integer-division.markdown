---
layout: post
title: "Integer division, step 1: non-negative integers"
date: 2019-10-06 12:57:20 -0400
draft: true
hidden: true
comments: true
categories: 
---

Integer division instructions are famously slow,
on the order of 10-30 times as slow as multiplication;
much slower than floating point division and, until recently on x86, not pipelined.
To a large extent, that's because compilers are expected to optimize away the majority of divisions,
since they tend to have constant divisors.
SBCL acquired a basic implementation of that "div-by-mul" transformation in 2014 thanks to [Erik Varga and Google's Summer of Code](https://www.google-melange.com/archive/gsoc/2014/orgs/sbcl/projects/erikvarga.html).
Let's explore a more general form of the transformation, and see how we can improve code generation by using the bound propagation pass in SBCL and handling tagged fixnums specially.

This post is the second in a series I [started in May 2014](https://www.pvk.ca/Blog/2014/05/11/integer-division-step-0-no-remainder/).
I'm reasonably confident I can get to the next one, on truncation by fractions, before 2026.

The simplest "div-by-mul" transform
-----------------------------------

If you've ever worked with fixed point arithmetic,
it's pretty natural to approximate division by an integer with a multiplication by its reciprocal, in fixed point.

The easiest rounding mode for fixed point multiplication is to truncate down (that's
what right shifts implement),
so we'll use that and round our approximate reciprocal up.
For example, on a 16 bit machine, we could approximate a division by 10
with a truncated multiplication by \\(\left\lceil 2\sp{16} / 10\right\rceil/2\sp{16} = 6554/2\sp{16}\\).

Armed with this constant, we can approximate \\(\lfloor 1337 / 10\rfloor\\) with
\\(\lfloor (5446 \cdot 1337)/2\sp{16}\rfloor = 133\\), which gives us the exact result.
In practice, the inner multiplication would be a [hardware `16 x 16 -> 16, 16` multiplication](https://c9x.me/x86/html/file_module_x86_id_210.html),
and the division, while semantically a right shift,
would simply be implemented by reading the register holding the high half of the
multiplication's result, and discarding the low (16 bit) half.

We definitely have an efficient implementation of something close to a division by 10.
\\(6554 / 10\sp{16}\\) is only slightly larger than \\(1/10\\):
the absolute error \\(\varepsilon = 6554 / 2\sp{16} - 1/10 = 1 / 163840 \approx 6.1 \cdot 10\sp{-6}\\).  On what range is the final *truncate* approximation exact?

We can go look for the first approximation error,
and determine that this div-by-mul approximation starts failing at 16389,
for which it returns 1639 rather than 1638.

The absolute error on the reciprocal becomes a relative error on the multiplication.
We might expect exactness up to 163840 (i.e., much wider than a 16 bit integer),
since that's the point at which the error exceeds 1.
However, we obtain the final integer result by truncating,
so even a small relative error can end in approximation failure, e.g.,
if \\(100.99\\) becomes \\(101.01\\) prior to truncation.

Our exact values are integers divided by 10, so the worst case (the closest to the next integer) happens for \\([9 \mod 10] / 10\\).
We want the absolute error after the fixed point multiplication to be less than
\\(1/10\\), our reciprocal, in order for the truncated result to be exact.
16389 is the smallest non-negative integer on which the approximation fails
because it's the least value greater than or equal to 16384 (at which point the multiplication is off by at least \\(1/10\\)) that's also in \\(9 \mod 10\\).

If we want to handle all word-sized (16-bit, in this example) values, we need a more precise approximation of the reciprocal.  Unfortunately, that's not always practical.
For example, the 18-bit over-approximation of \\(1/7\\) is \\(37450/2\sp{18}\\),
and fails at \\(43693\\) (where the approximation returns \\(6242\\) instead of \\(6241\\)).  If we want any more precision, we need to multiply by a
17 bit integer, and that's not a nice operation on a 16 bit machine.
Still, [Granlund and Montgomery](https://gmplib.org/~tege/divcnst-pldi94.pdf) show how to implement this word-size-plus-one multiplication, and that's what GCC and clang generate.

If rounding the multiplier up introduces a lot of error,
maybe we can instead adaptively round down.
I first saw this alternative described by [Arch Robison](https://dblp.org/rec/html/conf/arith/Robison05),
and it's presumably considered in [ICC](https://en.wikipedia.org/wiki/Intel_C%2B%2B_Compiler).
Rounding the fixed point constant down means we must round up at runtime,
which is less efficient than the native truncation.
However, the two rounding approach nicely complement each other,
since we can always use one when the other would need a wider than word-sized multiplier.

The general error analysis
--------------------------

Let's start by generalising the error analysis for \\(1/10\\) on 16-bit integers,
and handle the over-approximation (round the constant up) case for all word-sized divisors and all word sizes.
We'll then do the same for the complementary under-approximation case.

We're approximating a truncated division by \\(d\\)
with a truncated fixed point multiplication by \\(m/2\sp{s}\\),
where \\(m = \lceil 2\sp{s}/d\rceil.\\)
This multiplier \\(m\\) fits in a \\(w-\\)bit machine word
as long as \\(s < w + \log\sb{2} d.\\)
Unless \\(d\\) is a power of two
(and should then be directly lowered to a right shift),
we can thus let \\(s = w + \lfloor \log\sb{2} d\rfloor\\)
and know that the multiplier will fit in a machine word.

Let the absolute error on this approximation be
\\(\varepsilon = m/2\sp{s} - 1/d.\\)
The final truncation will be exact for any \\(x \in \mathbb{Z}\\)
iff \\(0 \leq \varepsilon x < 1 - (x \mod d)/d,\\)[^you-know-what-i-mean]
which is always satisfied when
\\[0 \leq \varepsilon x < \frac{1}{d} \Longleftrightarrow 0 \leq x < \frac{1}{\varepsilon d}.\\]

[^you-know-what-i-mean]: I'll frequently switch between `mod` as an integer-valued function and `mod` as an equivalence class. The context should clarify which one I mean.

That's the error analysis for rounding the multiplier up at compile-time,
and rounding down at runtime.
The complementary approach rounds the multiplier down at compile-time,
and rounds up at runtime.

We're approximating a truncated division by \\(d\\)
with a slightly-nudged-up fixed point multiplication by \\(m/2\sp{s}\\)
where \\(m = \lfloor 2\sp{s}/d\rfloor.\\)
Again, this multiplier \\(m\\) fits in a \\(w-\\)bit machine word
as long as \\(s \leq w + \lfloor \log\sb{2} d\rfloor.\\)

Multiplication by \\(m/s\sp{s}\\) slightly underapproximates
a multiplication by \\(1/d.\\) Rather than approximating
\\[\left\lfloor \frac{x}{d} \right\rfloor \approx \left\lfloor\frac{mx}{2\sp{s}}\right\rfloor,\\]
for an arbitrary \\(x\in\mathbb{N},\\) we should instead use
\\[\left\lfloor \frac{x}{d} \right\rfloor \approx \left\lfloor\frac{mx + \delta}{2\sp{s}}\right\rfloor.\\]

What value will the fudge factor \\(\delta\\) take?

Since \\(\delta\\) compensates for a systematic underapproximation,
it should be as positive as possible, but no larger.
Here as well, the worst case happens for \\(x \in (d-1\mod d)\\):
if \\(\delta\\) is too high, we'll bump our intermediate value
\\((mx + \delta)/2\sp{s}\\) to the next integer.
We need \\(\delta \leq 1/d,\\)
and we already have a good underapproximation of \\(1/d\\):
\\(m/2\sp{s}!\\)

Let the additive error on the approximation \\(1/d \approx m/2\sp{s}\\) be
\\(\varepsilon = m / 2\sp{s} - 1/d \leq 0.\\)
The final truncation will be exact for any \\(x\in\mathbb{Z}\\)
such that
\\[\varepsilon x + \frac{m}{2\sp{s}} \geq 0 \Longleftrightarrow 0 \leq x \leq \frac{m}{|\varepsilon| 2\sp{s}} = \frac{1/d - |\varepsilon|}{|\varepsilon|} = \frac{1}{|\varepsilon| d} - 1.\\]

Until now, we have avoided the question of how to actually implement this fixed point multiply and add.
We can always perform the full multiplication, add \\(m\\) to the low half of the result, and propagate the carry to the high half.
If the hardware exposes saturating arithmetic,
we could check that \\(\lfloor(2\sp{w}-1)/d\rfloor = \lfloor(2\sp{w}-2)/d\rfloor,\\)
and factor \\(mx + m = m(x + 1)\\) with a saturating increment.

We are now equipped with two div-by-mul approaches,
along with their respective correctness conditions.
The two approaches are complementary, in that each is more accurate when the other one introduces more approximation error.
Let's make sure we can always use one or the other with word-sized multipliers, 
for any non-zero word-sized divisor \\(d\\) that is not a power of two.[^po2-works]

[^po2-works]: The same approach can also handle power of two, but the error analysis is different (the approximation can be exact), and it's more practical to convert divisions by a constant power of two into shifts right.

Given a constant non-power-of-two divisor \\(d,\\) \\(0 < d < 2\sp{w} - 1,\\)
we'll show that \\(s = w + \lfloor \log\sb{2} d\rfloor\\) only needs
word-sized arithmetic and is always exact for word-sized dividends.

Let's first confirm that this shift value \\(s\\) always results in a multiplier \\(m < 2\sp{w}.\\)

We have 
\\[m \leq \left\lceil \frac{2\sp{s}}{d}\right\rceil = \left\lceil \frac{2\sp{\lfloor \log\sb{2}d\rfloor}}{d} 2\sp{w} \right\rceil,\\]
with equality when we use the over-approximation multiply-add method.

The term inside the \\(\lceil\cdot\rceil\\) is always less than \\(2\sp{w},\\)
since, by hypothesis, \\(d\\) is not a power of two.
The closest \\(2\sp{\lfloor\log\sb{2}d\rfloor}/d\\) can get to 1 is when
\\(d = 2\sp{w-1} + 1.\\)  In that case, the inner term becomes
\\[\frac{2\sp{s}}{d} = \frac{2\sp{2 w - 1}}{2\sp{w-1}+1} = 2\sp{w}\frac{2\sp{w-1}}{2\sp{w-1}+1} = 2\sp{w} \left(1 - \frac{1}{2\sp{w-1}+1}\right) \leq 2\sp{w}-1.\\]
The multiplier \\(m\\) always fits in \\(w\\) bits,
the width of machine words,
as long as all the other quantities involved in the approximation also do.

We still have to show that one of the over- or under-approximation scheme
is precise enough for any word-sized dividend, given the shift value \\(s.\\)

Let's set the multiplier \\(m \approx \frac{2\sp{s}}{d}\\)
by rounding to nearest.
Since we rounded to nearest, we have \\(|m - 2\sp{s}/d| \leq 1/2,\\)
and thus
\\[|\varepsilon| = \left| \frac{m}{2\sp{s}} - \frac{1}{d} \right| = \left|\frac{m - 2\sp{s}/d}{2\sp{s}}\right| \leq \frac{1/2}{2\sp{s}} = \frac{1}{2\sp{s+1}} = \frac{1}{2\sp{w + \lfloor\log\sb{2}d\rfloor + 1}}.\\]

The correctness condition for the simpler over-approximation scheme needs
\\((\varepsilon d)\sp{-1}> 2\sp{w}-1\\)
in order to cover all non-negative integer below \\(2\sp{w};\\)
the under-approximation multiply-and-add scheme is slightly more demanding,
and requires \\((\varepsilon d)\sp{-1} - 1 \geq 2\sp{w}-1.\\)

Both conditions are always satisfied,
now that rounding to nearest gives us one extra bit of precision:
\\[(\varepsilon d)\sp{-1} \geq \frac{2\sp{w+\lfloor \log\sb{2}d\rfloor + 1}}{d} > 2\sp{w},\\]
since \\(2\sp{\lfloor \log\sb{2}d\rfloor + 1} > d.\\)

We found a sort of completeness
(we can always find a div-by-mul constant that fits in a machine word)
and soundness (the resulting approximation is always exact for all machine integers)
result.
Is it time to just implement this adaptive transformation,
or can we do better?

What else could we desire?
--------------------------

So far, we have a proof of reasonable feasibility:
it's always possible to convert unsigned integer division to a fixed point multiplication(-add)-and-shift,
where the constant multiplier is word-sized.
We don't know that our round-to-nearest approach will always find the best code sequence,
or even what makes a code sequence "best."

Given that we're choosing between similar code sequences
where most of the runtime overhead (latency and throughput) is incurred by the mandatory multiplication,
I'll simply count additional simple operations (add, add with carry, shift).
That's an x86-centric view, but probably a good enough proxy for other architectures.

This means that the best code sequence is a single (full) multiplication,
where we find the resulting quotient in the high register,
without any addition or shift.
We obtain this code sequence when we let the shift value \\(s\\) equal the word size \\(w\\),
and the round-up approximation is accurate enough.

After that comes a multiplication and a shift (regular round-up approximation).

Next is the round-down approximation, with shift \\(s = w\\) the word size,
and the dividend incremented before multiplying (factor out \\(mx + m = m(x + 1)\\)) when possible.
Arguably, when pre-increment is feasible (the range of the dividend does not span the full word range),
the increment is preferable to a shift.

Last comes the general round-down approximation,
with \\(s > w\\).

This yields two priority orders,
depending on whether the unknown dividend can equal \\(2\sp{w} - 1\\).

If `WORD_MAX` is possible, we should check for

1. Round-up constant with shift \\(s = w\\).
2. Round-up constant with \\(s = w + \lfloor \log\sb{2}d\rfloor\\).
3. Round-down constant with \\(s = w\\).
4. Round-down constant with \\(s = w + \lfloor \log\sb{2}d\rfloor\\).

Otherwise, we can do better in the "round-down" method:
we can pre-increment the variable value instead of propagating carries in a double-word value.  The ordering becomes

1. Round-up constant with shift \\(s = w\\).
2. Round-down constant with \\(s = w\\).
3. Round-up constant with \\(s = w + \lfloor \log\sb{2}d\rfloor\\).
4. Round-down contant with \\(s = w + \lfloor \log\sb{2}d\rfloor\\).

We need range information that's more precise than the C type system to use the latter order,
but SBCL's type propagation capabilities are more than sufficient.

When targeting 64 bit machines, it seems like it would make sense to use 32 bit multiplications opportunistically.
However, I'm mostly interested in x86-64,
and [Agner Fog's tables](https://www.agner.org/optimize/instruction_tables.pdf) show that integer multiplications perform nearly identically, regardless of bitwidth
(if anything, 32 bit multiplications are slower than 8 or 64 bit ones).[^no-16-bit]
Any win will be on code size, but,
if we can get away with an 8 or 32 bit constant,
we can definitely use a round-up constant with \\(s = 64.\\)
Thus, it only makes sense to look for smaller "round up" constants with \\(s = 8\\) or \\(s = 32.\\)
Anything else would be both slower and likely larger than the best sequence with a 64-bit multiplication.

[^no-16-bit]: As a rule, I try to avoid 16 bit arithmetic, since that seems to hit unexpected performance bottlenecks. For integer multiplication, this shows up as half the throughput of other widths.

A tighter analysis for low-tagged fixnums
-----------------------------------------

Language implementations with managed memory and type erasure
tend to have a specialised representation for small integers.
The naïve approach is used by CPython: heap allocate descriptors for all integers, and hope a cache of small descriptors does something.
A more reasonable approach exploits alignment of heap-allocated objects
to stash type "tag" bits in the low order (least-significant) bits.

On 64-bit architectures,
SBCL aligns every heap allocation to 16 bytes
(2 pointer-sized words),
and thus gets to use the bottom 4 bits of every word for type information.
The stock configuration reserves one bit (all even bit patterns) for 63-bit signed "fixnums",
with the remaining 8 odd bit patterns shared between a few pointer types
and small immediates like characters and single floats.
In that configuration, the value \\(3\\) is represented as \\(3\cdot 2 = 6.\\)[^32-bit-tag]

[^32-bit-tag]: There's less alignment on 32 bit architectures (8 bytes), so we instead use two zero tag bits for fixnums, so \\(3\\) is instead represented as \\(3 \cdot 4 = 12.\\)

SBCL also supports unboxed signed and unsigned integers,
and does not have to use a boxed (tagged) representation for machine integers.
However, it often makes sense to do so,
e.g., when the fixnums are used to index in arrays, or
are frequently loaded and stored in a tagged representation.

The algorithms presented so far only work on raw untagged integers.
It's easy enough to remove tag bits with an arithmetic shift right.
Then again, right shifts look a lot like divisions,
so we should be able to fold the untagging in the constant divisor.

First, let's quickly confirm that
\\[\left\lfloor \frac{\lfloor x/d\sb{1}\rfloor}{d\sb{2}}\right\rfloor = \left\lfloor \frac{x}{d\sb{1}d\sb{2}}\right\rfloor.\\]

Let \\(x = q d\sb{1}d\sb{2} + r\sb{2}d\sb{1} + r\sb{1},\\)
with \\(0\leq r\sb{1} < d\sb{1}\\) and \\(0\leq r\sb{2} < d\sb{2}.\\)
We can find such a decomposition for any \\(x\\);
it's essentially a mixed radix representation.

The right-hand side satisfies \\(\lfloor x/(d\sb{1}d\sb{2})\rfloor = q.\\)

On the left-hand side, we find \\(\lfloor x / d\sb{1}\rfloor = qd\sb{2} + r\sb{2}\\),
and thus 
\\[\left\lfloor \frac{\lfloor x/d\sb{1}\rfloor}{d\sb{2}}\right\rfloor = \left\lfloor \frac{qd\sb{2} + r\sb{2}}{d\sb{2}}\right\rfloor = q = \left\lfloor \frac{x}{d\sb{1}d\sb{2}}\right\rfloor.\\]

Thus, when rounding towards \\(-\infty,\\)
we can combine untagging and division,
or any number of integer division steps in general.

For unsigned fixnums on SBCL, that's enough to know that we can find a
"round-up" div-by-mul constant: fixnums always use a signed representation,
so the bit pattern for the most positive fixnum is \\(2\sp{w-1}-1,\\)
which means we can always find a round-up multiplier \\(m < 2\sp{w}.\\)

We should still be able to do better.
[We already know that \\(x / d\\) is easier when \\(x = qd\\)](https://www.pvk.ca/Blog/2014/05/11/integer-division-step-0-no-remainder/),
so it would make sense to find another simplification
when \\(x = qd\sb{1} + k\\) and \\(d = d\sb{1}d\sb{2}.\\)

Such a result wouldn't necessarily be useful in SBCL's case,
but it could help us get away with a lower shift value, and
thus more efficient code.
Statically typed languages have more representation freedom,
and can better apply such a simplification.
For example,
Haskell's [Word](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Word.html)
is the same size as its tagged [Int](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Int.html#t:Int) but unsigned.
Ideally, we would reliably find simple div-by-mul sequences
to simultaneously untag and divide by a constant.

<small>
Ideally ideally, we would also obtain a tagged result naturally,
but I don't see that happening:
at best, we might be able to mask out preshifted tag bits,
which isn't  better than shifting the untagged result
back in tagged format.
</small>

Smart implementers set the tag bits to all zeros for small integers,
so we're looking for a more lenient correctness bound for 
\\(\lfloor x / (d 2\sp{t})\rfloor\\),
where \\(0\leq x = q 2\sp{t} < 2\sp{w}.\\)

If we go back to the error bound in the general case,
the worst case happens when \\(x \in (d - 1)\mod d,\\)
which is clearly impossible (unless we have no tag bit, and \\(t = 0\\)).
In the "round up" case, we're essentially getting
\\(t\\) extra bits of precision.  Rather than
\\(0 \leq x < (\varepsilon d 2\sp{t})\sp{-1},\\)
we can get away with
\\(0 \leq x < (\varepsilon d)\sp{-1}.\\)
This is great: we only need one extra bit to guarantee
that we can always use a "round up" approximation,
and never have to emit a "round down" approximation,
with its slightly slower double-wide addition.

Let's generalise to integer division \\(\lfloor x / (d\sb{1}d\sb{2})\rfloor\\)
with \\(x = qd\sb{1} + r,\\)
and \\(r\\) either always small (\\(0\leq r \leq r\sb{\mathrm{max}} < d\sb{1}/2\\))
or always positive (\\(r \geq r\sb{\mathrm{min}} \geq 1\\)).

In the first case, we can adapt the error bound for the round up approximation.
Rather than
\\(0 \leq \varepsilon x < \frac{1}{d\sb{1}d\sb{2}},\\)
we only require
\\[0 \leq \varepsilon x < \frac{d\sb{1} - r\sb{\mathrm{max}}}{d\sb{1} d\sb{2}}  \Longleftrightarrow 0 \leq x < \frac{d\sb{1}-r\sb{\mathrm{max}}}{\varepsilon d\sb{1} d\sb{2}}.\\]

When \\(r\sb{\mathrm{max}} = 0,\\) we get
\\(0 \leq x < (\varepsilon d\sb{2})\sp{-1}.\\)
Otherwise, \\(d > 2,\\) and 
\\[\frac{d\sb{1} - r\sb{\mathrm{max}}}{d\sb{1}} \geq \frac{1}{2},\\]
which lets us weaken
\\[0 \leq x < \frac{d\sb{1}-r\sb{\mathrm{max}}}{\varepsilon d\sb{1} d\sb{2}} \Longrightarrow 0 \leq x < (2 \varepsilon d\sb{2})\sp{-1},\\]
which is still better than \\((\varepsilon d\sb{1} d\sb{2})\sp{-1}\\)
and yields at least one extra bit of accuracy when \\(d\sb{1} \neq 3.\\)[^what-if-d-eq-3]

[^what-if-d-eq-3]: If \\(d\sb{1} = 3\\) and \\(r\sb{\mathrm{max}} \neq 0,\\) either the "round up" doesn't need the extra accuracy, or we we can use the "round down" approximation for free, as we're about to show.

In the other case,
\\(x = qd\sb{1} + r\\) and (\\(d\sb{1}/2 \leq r\sb{\mathrm{min}} \leq r < d\sb{1}\.\\))
This looks similar to the intermediate result 
for the "round down" approximation,
when we can pre-increment the dividend.

In other words, we wish to divide \\(\lfloor x\sb{0} / (d\sb{1}d\sb{2})\rfloor\\)
and we already have \\(x = x\sb{0} + \delta,\\)
such that \\(\lfloor x\sb{0} / (d\sb{1}d\sb{2}) \rfloor = \lfloor x/ (d\sb{1}d\sb{2}) \rfloor,\\)
and \\(\delta \geq r\sb{\mathrm{min}} \geq 1.\\)

This lets us widen the correctness condition for the round down approximation:
\\[0 \leq x\sb{0} \leq \frac{\delta(1/d\sb{1}d\sb{2} - \varepsilon)}{|\varepsilon|} = \delta \left(\frac{1}{d\sb{1}d\sb{2}|\varepsilon|} - 1\right),\\]
which is never worse than \\(x \leq (|\varepsilon| d\sb{1}d\sb{2})\sp{-1} - 1,\\)
and doesn't need any explicit pre- or post-increment,
unlike usual round down code sequences.

In short, when truncating tagged non-negative integers,
we can always fold the untagging in the original truncated division.
Moreover, when potential tag values are less than half the tag range,
we can use "round down" constants,
and benefit from at least one extra bit of precision (unless the tagging scheme is in ternary).
This means we can always use a word-sized constant.
When potential tag values are always strictly positive,
we can also use "round down" constants, 
but do not necessarily benefit from additional precision.
However, the "round down" constants now get the increment for free,
so we know that one of the word-sized "round down" or "round up" constants
is accurate enough,
and may be implemented with a multiplication and perhaps a shift.

TL;DR: if we wish to compute \\(\lfloor x / (d\sb{1}d\sb{2}) \rfloor,\\)
where both \\(x\\) and the divisor \\(d\sb{1}d\sb{2}\\) are word-sized,
and we can either assume \\(x \in [0, \\lceil d\sb{1}/2\rceil) \mod d\sb{1}\\)
or \\(x \neq 0 \mod d\sb{1},\\)
there always exists a div-by-mul sequence with a
word-sized multiplication anda shift
(unless \\(d\sb{1} = 3\\) and \\(x \mod 3\\) may be \\(1\\)).

Implementing all that in SBCL
-----------------------------

The current div-by-mul transformation in SBCL does not do a particularly good job of optimising the code sequence.
In particular, it triggers as soon as the dividend is known to fit in a machine word,
and generates opaque code for the maximum value in that initial range.
This has the effect of hindering type propagation in later passes,
and of not benefitting from eventual improved type precision.

I think a better approach would be to optimistically replace the quotient computation in [`CL:TRUNCATE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_floorc.htm)
with a specialised intrinsic that is always translated to a VOP assembly template.
This intrinsic carries even more semantic information than a regular `CL:TRUNCATE` call,
so can easily participate in type propagation.
It can also be updated with tighter parameters as we learn more about its runtime-variant argument.
For example, we could
start with a template that's suitable for general round-down div-by-mul,
then improve to a pre-increment div-by-mul as the types become tighter,
and end up with a single multiply-no-shift at the end.

It's important to distinguish between these cases early,
before VOP template instantiation,
because each possibility has unique register requirements,
and thus affect register allocation...
and regalloc happens before VOP instantiation.

What's next?
------------

I haven't mentioned signed integer division so far.
Mostly, that's because I rarely use that operation.

It's also more complicated.

At first sight, it seems like division of signed integers by constants
should be easier: the absolute value of the dividend is at most \\(2\sp{w - 1}.\\)
However, most architectures only offer `unsigned * unsigned` and
`signed * signed` multiplications,
so we also lose one bit in the constant multiplier.
[Granlund and Montgomery](https://gmplib.org/~tege/divcnst-pldi94.pdf) have the details,
with a simpler `w+1` bit fixup.
I think we might also be able to select a "round up" or "round down" constant
at runtime, depending on the sign of the dividend.
Maybe that'll be the next post in the series;
I wouldn't hold my breath.

XXX also implemented the prioritised code sequence options in SBCL,
so you can get all this work for free, including the specialised
(less shifty) code sequences for division of tagged fixnums.

The next post in this series will most likely cover 
truncated multiplication by constant fractions in \\((0, 1) \cap \mathbb{Q}.\\)
Let's see if I can bang that out before turning 40.
