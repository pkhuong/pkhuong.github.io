---
layout: post
title: "Baseline implementations should be predictable"
date: 2021-05-14 01:53:04 -0400
comments: true
categories: 
---

I wrote [Reciprocal](https://crates.io/crates/reciprocal) because I
couldn't find a nice implementation of div-by-mul in Rust without
data-dependent behaviour. Why do I care?

Like [ridiculous fish mentions in his review of integer divisions on M1 and Xeon](https://ridiculousfish.com/blog/posts/benchmarking-libdivide-m1-avx512.html),
certain divisors (those that lose a lot of precision when rounding up
to a fraction of the form \\(n / 2^k\\)) need a different, slower,
code path in classic implementations. Powers of two are also typically
different, but at least divert to a faster sequence, a variable right
shift.

Reciprocal instead uses a unified code path to implement two
expressions, \\(f_{m,s}(x) = \left\lfloor \frac{m x}{2^s} \right\rfloor\\) and
\\(g_{m^\prime,s^\prime}(x) = \left\lfloor\frac{m^\prime \cdot \min(x + 1, \mathtt{u64::MAX})}{2^{s^\prime}}\right\rfloor\\),
that are identical except for the saturating increment of \\(x\\) in
\\(g_{m^\prime,s^\prime}(x)\\).

The first expression, \\(f_{m,s}(x)\\) corresponds to the usual
div-by-mul approximation (implemented in gcc, LLVM, libdivide, etc.)
where the reciprocal \\(1/d\\) is approximated in fixed point by rounding
\\(m\\) *up*, with the upward error corrected by the truncating
multiplication at runtime.  See, for example, Granlund and
Montgomery's [Division by invariant integers using multiplication](https://gmplib.org/~tege/divcnst-pldi94.pdf).

The second, \\(g_{m^\prime,s^\prime}(x)\\), is the multiply-and-add
scheme of described by Robison in [N-Bit Unsigned Division Via N-Bit Multiply-Add](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.512.2627&rep=rep1&type=pdf).

In that approximation, the reciprocal multiplier \\(m^\prime\\) is
rounded *down* when converting \\(1/d^\prime\\) to fixed point.  At 
runtime, we then bump the product up (by the largest value
\\(\frac{n}{2^{s^\prime}} < 1/d^\prime\\), i.e., \\(\frac{m^\prime}{2^{s^{prime}}}\\)) before dropping the low bits.

With a bit of algebra, we see that \\(m^\prime x + m^\prime = m^\prime (x + 1)\\)...
and we can use a saturating increment to avoid a 64x65 multiplication
as long as we don't trigger this second expressions for divisors
\\(d^\prime\\) for which
\\(\lfloor \frac{\mathtt{u64::MAX}}{d^\prime}\rfloor \neq \lfloor \frac{\mathtt{u64::MAX} - 1}{d^\prime}\rfloor\\).

We have a pair of dual approximations, one that rounds the reciprocal up to a
fixed point value, and another that rounds down; it makes sense to
round to nearest, which nets us one extra bit of precision in the
worst case, compared to always applying one or the other.  Luckily,[^or-is-it]
[all of `u64::MAX`'s factors (except 1 and `u64::MAX`) work with the "round up" approximation](https://github.com/pkhuong/reciprocal/blob/c4f6eeeb7108a778c6e8c1f8a5ac7c6df13e2943/src/lib.rs#L322)
that doesn't increment, so the saturating increment is always safe
when we actually want to use the second approximation (unless
\\(d^\prime \in \\{1, \mathtt{u64::MAX}\\}\\)).

[^or-is-it]: Is it luck?  Sounds like a fun number theory puzzle.

This duality is the reason why Reciprocal can get away with
64-bit multipliers.

Even better, \\(f_{m,s}\\) and \\(g_{m^\prime,s^\prime}\\)
differ only in the absence or presence of a saturating increment.
Rather than branching, Reciprocal executes a data-driven increment
by 0 (for \\(f_{m,s}(x)\\)) or by 1 (for
\\(g_{m^\prime,s^\prime}(x)\\)).  The upshot: predictable improvements
over hardware division, even when dividing by different constants.

Summary of the results below: when measuring the throughput of
independent divisions on my i7 7Y75 @ 1.3 GHz, Reciprocal consistently
needs 1.3 ns per division, while hardware division can only achieve
~9.6 ns / division (Reciprocal needs 14% as much / 86% less time).
This looks comparable to the
[results reported by fish for libdivide when dividing by 7](https://ridiculousfish.com/blog/posts/benchmarking-libdivide-m1-avx512.html#:~:text=intel%20xeon%203.0%20ghz%20(8275cl)).
Fish's [libdivide](https://github.com/ridiculousfish/libdivide) no
doubt does better on nicer divisors, especially powers of two, but
it's good to know that a simple implementation comes close.

We'll also see that, in Rust land, the
[fast\_divide crate](https://crates.io/crates/fastdivide)
is dominated by [strength\_reduce](https://github.com/ejmahler/strength_reduce),
and that strength\_reduce is only faster than [Reciprocal](https://github.com/pkhuong/reciprocal/)
when dividing by powers of two (although, looking at the disassembly,
it probably comes close for single-result latency).

First, results for division with the same precomputed inverse.  The
timings are from
[criterion.rs](https://github.com/bheisler/criterion.rs), 
for [\\(10^4\\) divisions in a tight loop](https://github.com/pkhuong/reciprocal/blob/c4f6eeeb7108a778c6e8c1f8a5ac7c6df13e2943/benches/div_throughput.rs#L29).

- "Hardware" is a regular HW DIV, 
- "compiled" lets LLVM generate specialised code,
- "reciprocal" is [PartialReciprocal](https://github.com/pkhuong/reciprocal/blob/d591c59044b3a4f662112aae73c3adae9f168ea6/src/lib.rs#L11),[^why-partial]
- "strength\_reduce" is the [strength\_reduce crate's u64 division](https://github.com/ejmahler/strength_reduce),
- and "fast\_divide" is [the fast\_divide crate's u64 division](https://crates.io/crates/fastdivide).

[^why-partial]: The struct is "partial" because it can't represent divisions by 1 or `u64::MAX`.


The last two options are the crates I considered before writing
Reciprocal.  The strength\_reduce crate switches between a special
case for powers of two (implemented as a bitscan and a shift), and a
general slow path that handles everything with a 128-bit fixed
point multiplier.  fast\_divide is inspired by libdivide and 
implements the same three paths: a fast path for powers of two (shift
right), a slow path for reciprocal multipliers that need one more bit
than the word size (e.g, division by 7), and a regular round-up
div-by-mul sequence.

Let's look at [the three cases in that order](https://github.com/pkhuong/reciprocal/blob/c4f6eeeb7108a778c6e8c1f8a5ac7c6df13e2943/benches/div_throughput.rs#L29).

\\(10^4\\) divisions by 2 (i.e., a mere shift right by 1)

    hardware_u64_div_2      time:   [92.297 us 95.762 us 100.32 us]
    compiled_u64_div_by_2   time:   [2.3214 us 2.3408 us 2.3604 us]
    reciprocal_u64_div_by_2 time:   [12.667 us 12.954 us 13.261 us]
    strength_reduce_u64_div_by_2
                            time:   [2.8679 us 2.9190 us 2.9955 us]
    fast_divide_u64_div_by_2
                            time:   [2.7467 us 2.7752 us 2.8025 us]

This is the *comparative* worst case for Reciprocal: while Reciprocal
always uses the same code path (1.3 ns/division), the compiler shows
we can do much better with a right shift. Both branchy implementations
include a special case for powers of two, and thus come close to the
compiler, thanks a predictable branch into a right shift.

\\(10^4\\) divisions by 7 (a "hard" division)

    hardware_u64_div_7      time:   [95.244 us 96.096 us 97.072 us]
    compiled_u64_div_by_7   time:   [10.564 us 10.666 us 10.778 us]
    reciprocal_u64_div_by_7 time:   [12.718 us 12.846 us 12.976 us]
    strength_reduce_u64_div_by_7
                            time:   [17.366 us 17.582 us 17.827 us]
    fast_divide_u64_div_by_7
                            time:   [25.795 us 26.045 us 26.345 us]

Division by 7 is hard for compilers that do not implement the "rounded down"
approximation described in Robison's
[N-Bit Unsigned Division Via N-Bit Multiply-Add](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.512.2627&rep=rep1&type=pdf).
This is the comparative *best* case for Reciprocal, since it always
uses the same code (1.3 ns/division), but most other implementations
switch to a slow path (strength\_reduce enters a general case that
is arguably more complex, but more transparent to LLVM). Even
divisions directly compiled with LLVM are ~20% faster than Reciprocal:
LLVM does not implement Robison's round-down scheme, so it
hardcodes a more complex sequence than Reciprocal's.

\\(10^4\\) divisions by 11 (a regular division)

    hardware_u64_div_11     time:   [95.199 us 95.733 us 96.213 us]
    compiled_u64_div_by_11  time:   [7.0886 us 7.1565 us 7.2309 us]
    reciprocal_u64_div_by_11
                            time:   [12.841 us 13.171 us 13.556 us]
    strength_reduce_u64_div_by_11
                            time:   [17.026 us 17.318 us 17.692 us]
    fast_divide_u64_div_by_11
                            time:   [21.731 us 21.918 us 22.138 us]

This is a typical result. Again, Reciprocal can be trusted to work at
1.3 ns/division.  Regular round-up div-by-mul works fine when dividing
by 11, so code compiled by LLVM only needs a multiplication and a shift,
nearly twice as fast as Reciprocal's generic sequence.  The fast\_divide
crate does do better here than when dividing by 7, since it avoids the
slowest path, but Reciprocal is still faster; simplicity pays.

The three microbenchmarks above reward special-casing, since they always
divide by the same constant in a loop, and thus always hit the same
code path without ever incurring a mispredicted branch.

What happens to independent divisions [by unpredictable precomputed divisors](https://github.com/pkhuong/reciprocal/blob/main/benches/div_throughput_variable.rs),
for divisions by 2, 3, 7, or 11 (respectively easy, regular, hard, 
and regular divisors)?

    hardware_u64_div        time:   [91.592 us 93.211 us 95.125 us]
    reciprocal_u64_div      time:   [17.436 us 17.620 us 17.828 us]
    strength_reduce_u64_div time:   [40.477 us 41.581 us 42.891 us]
    fast_divide_u64_div     time:   [69.069 us 69.562 us 70.100 us]

The hardware doesn't care, and Reciprocal is only a bit slower (1.8
ns/division instead of 1.3 ns/division) presumably because the relevant
`PartialReciprocal` struct must now be loaded in the loop body.

The other two branchy implementations seemingly take a hit
proportional to the number of special cases. The `strength_reduce` hot
path only branches once, to detect divisors that are powers of two;
its runtime goes from 0.29 - 1.8 ns/division to 4.2 ns/division (at
least 2.4 ns slower/division).  The `fast_divide` hot path, like libdivide's,
switches between *three* cases, and goes from 0.28 - 2.2
ns/division to 7.0 ns/division (at least 4.8 ns slower/division).

And that's why I prefer to start with predictable baseline
implementations: unpredictable code with special cases can easily
perform well on benchmarks, but, early on during development, it's
hard to tell how the benchmarks may differ from real workloads, and
whether the special cases "overfit" on these differences.

With special cases for classes of divisors, most runtime div-by-mul
implementations make you guess whether you'll tend to divide by powers
of two, by "regular" divisors, or by "hard" ones in order to estimate
how they will perform.  Worse, they also force you to take into
account how often you'll switches between the different classes.
Reciprocal does not have that problem: its hot path is the same
regardless of the constant divisor, so it has the same predictable
performance for all divisors[^partial],
and there's only one code path, so we don't have to worry about class
switch.

[^partial]: ...all divisors except 1 and `u64::MAX`, which must instead [use the more general `Reciprocal` struct](https://github.com/pkhuong/reciprocal/blob/d591c59044b3a4f662112aae73c3adae9f168ea6/src/lib.rs#L176).

Depending on the workload, it may make sense to divert to faster code
paths, but it's usually best to start without special cases when it's
practical to do so...  and I think
[reciprocal](https://crates.io/crates/reciprocal) shows that, for
integer division by constants, it is.

<p><hr style="width: 50%" /></p>
