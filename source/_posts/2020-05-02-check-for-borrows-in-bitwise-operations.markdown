---
layout: post
title: Check for borrows in bitwise operations
date: 2020-05-02 17:24:38 -0400
comments: true
categories: 
---

In the [fourth installment of his series on sorting with AVX2](https://bits.houmus.org/2020-02-01/this-goes-to-eleven-pt4),
[@damageboy](https://twitter.com/damageboy) has a short aside where he
tries to detect partitioning (pivot) patterns where elements less than
and greater than or equal to the pivot are already in the correct
order: in that case, the partitioning routine does not need to permute
the block of values.  The practical details are irrelevant for this
post; what matters is that we wish to quickly identify whether a byte
value matches any of the follow nine cases:

 * `0b11111111`
 * `0b11111110`
 * `0b11111100`
 * `0b11111000`
 * `0b11110000`
 * `0b11100000`
 * `0b11000000`
 * `0b10000000`
 * `0b00000000`

Looking at the bit patterns,[^b-for-bit-literal] the OP's solution with [popcount](https://www.felixcloutier.com/x86/popcnt) and [bitscan](https://www.felixcloutier.com/x86/bsf)
is pretty natural.  These instructions are somewhat complex (latency
closer to 3 cycles than 1, and often port restricted), 
and it seems like the sort of problem that would have had efficient
solutions before SSE4 finally graced x86 with a [population count](https://en.wikipedia.org/wiki/Hamming_weight) instruction.

[^b-for-bit-literal]: I use the `0b...` syntax throughout this post to denote bit literals, similarly to the usual `0x...` hexadecimal literals.

In the context of a sorting library's partition loop, `popcnt` and
`bsf` is probably more than good enough:
[the post shows that the real issue is branch mispredictions](https://bits.houmus.org/2020-02-01/this-goes-to-eleven-pt4)
being slower than permuting unconditionally.
This is just a fun challenge to think about (:

Warm-up: `is_power_of_two`
--------------------------

Detecting whether a machine integer is a power of two (or zero) is
another task that has a straightforward solution in terms of popcount
or bitscan.  There's also a simpler classic solution to this problem:

`x == 0 || is_power_of_two(x) <==> (x & (x - 1)) == 0`

How does that expression work?  Say `x` is a power of two. Its binary
representation is `0b0...010...0`: any number of leading zeros,[^big-endian] 
a single "1" bit, and trailing zeros (maybe none).  Let's see what happens when
we subtract 1 from `x`:

    x           = 0b00...0010...0
         x - 1  = 0b00...0001...1
    x & (x - 1) = 0b00...0000...0

[^big-endian]: While I prefer to work with little-endian bytes, I find everything makes more sense with big-endian bits.

The subtraction triggered a chain of [borrows](https://en.wikipedia.org/wiki/Carry_(arithmetic))
throughout the trailing zeros, until we finally hit that 1 bit.
In decimal, subtracting one from `10...0` yields `09...9`;
in binary we instead find `01...1`.
If you ever studied the circuit depth (latency) of carry chains
(for me, that was for circuit complexity theory), you know
that this is difficult to do well.
Luckily for us, [chip makers work hard to pull it off](https://en.wikipedia.org/wiki/Kogge%E2%80%93Stone_adder),
and we can just use carries as a data-controlled
primitive to efficiently flip ranges of bits.

When `x` is a power of two, `x` and `x - 1` have no "1" bit in common,
so taking the bitwise `and` yields zero.  That's also true when `x` is 0,
since `and`ing anything with 0 yields zero.  Let's see what happens
for non-zero, non-power-of-two values `x = 0bxx...xx10..0`,
i.e., where `x` consists of an arbitrary non-zero sequence of bits `xx..xx`
followed by the least set bit (there's at least one, since `x` is neither zero nor a power of two), and the trailing zeros:

    x           = 0bxx...xx10...0
         x - 1  = 0bxx...xx01...1
    x & (x - 1) = 0bxx...xx000000

The leading not-all-zero `0bxx...xx` is unaffected by the subtraction,
so it passes through the bitwise `and` unscathed (`and`ing any bit with
itself yields that same bit), and we know there's at least one non-zero
bit in there; our test correctly rejects it!

Stretching: decoding varints
----------------------------

When decoding variable length integers in [ULEB](https://en.wikipedia.org/wiki/LEB128#Unsigned_LEB128)
format, e.g., for [protocol buffers](https://developers.google.com/protocol-buffers/docs/encoding),
it quickly becomes clear that, in order to avoid byte-at-a-time logic,
we must rapidly segment ([lex or tokenize](https://en.wikipedia.org/wiki/Lexical_analysis), in a way) our byte stream to determine where each ULEB
ends.  Let's focus on the fast path, when the encoded ULEB fits in a
machine register.

We have `uleb = 0bnnnnnnnnmmmmmmmm...1zzzzzzz0yyyyyyy0...`: a sequence of bytes[^remember-endianness] with the topmost bit equal to 0, followed by a byte with the top bit set to 1, and, finally, arbitrary nuisance bytes (`m...m`, `n...n`, etc.) we wish to ignore.  Ideally, we'd
extract `data = 0b0000000000000000...?zzzzzzz?yyyyyyy?...` from `uleb`: we want to clear the
nuisance bytes, and are fine with arbitrary values in the 
ULEB's control bits.

[^remember-endianness]: Remember, while ULEB is little-endian, we use big bit-endianness.

It's pretty clear that the first thing to do is to
clear out everything but potential ULEB control bits (the high bit of
each byte), with `c = uleb & (128 * (WORD_MAX / 255))`, i.e.,
compute the bitwise `and` of `uleb` with a bitmask of the high bit in each byte.

       uleb = 0bnnnnnnnnmmmmmmmm...1zzzzzzz0yyyyyyy0...
          c = 0bn0000000m0000000...10000000000000000...

We could now bitscan to find the index of the first 1 (marking the
last ULEB byte), and then generate a mask.  However, it seems wasteful to
generate an index with a scan, only to convert it back into bitmap
space with a shift.  We'll probably still want that index to know how
far to advance the decoder's cursor, but we can hopefully update the
cursor in parallel with decoding the current ULEB value.

When we were trying to detect powers of two, we subtracted `1` from
`x`, a value kind of like `c`, in order to generate a new value
that differed from `x` in all the bits up to and including the first
set (equal to `1`) bit of `x`, and identical in the remaining bits.  We
then used the fact that `and`ing a bit with itself yields that same
bit to detect whether there was any non-zero bit in the remainder.

Here, we wish to do something else with the remaining untouched bits, we
wish to set them all to zero.  Another bitwise operator does
what we want: `xor`ing a bit with itself always yields zero, while
`xor`ing bits that differ yields `1`.  That's the plan for ULEB. We'll
subtract 1 from `c` and `xor` that back with `c`.

         uleb   = 0bnnnnnnnnmmmmmmmm...1zzzzzzz0yyyyyyy0...
    c           = 0bn0000000m0000000...10000000000000000...
         c - 1  = 0bn0000000m0000000...01111111111111111...
    c ^ (c - 1) = 0b0000000000000000...11111111111111111...

We now just have to bitwise `and` `uleb` with `c ^ (c - 1)`
to obtain the bits of the first `ULEB` value in `uleb`, while
overwriting everything else with 0.  Once we have that, we can either
[extract data bits with `PEXT`](https://www.felixcloutier.com/x86/pext)
on recent Intel chips, or otherwise dust off interesting stunts for [SWAR](https://en.wikipedia.org/wiki/SWAR) shifts by variable amounts.

Now for [damageboy](https://bits.houmus.org/2020-02-01/this-goes-to-eleven-pt4)'s problem
----------------------------------------------------------------

Let's first repeat the question that motivated this post.  We want to detect when a byte `p` is one of the following nine values:

 * `0b11111111`
 * `0b11111110`
 * `0b11111100`
 * `0b11111000`
 * `0b11110000`
 * `0b11100000`
 * `0b11000000`
 * `0b10000000`
 * `0b00000000`

These bit patterns feel similar to those for power of two bytes: if we
complement the bits, these values are all 1 less than a power of two
(or -1, one less than zero).  We already know how to detect when a
value `x` is zero or a power of two (`x & (x - 1) == 0`), so it's easy
to instead determine whether `~p` is one less than zero or a power of
two: `(~p + 1) & ~p == 0`.

This is already pretty good: bitwise `not` the byte `p`,
and check if it's one less than zero or a power of two (three simple
instructions on the critical path).  We can do better.

There's another name for `~p + 1`, i.e., for bitwise complementing a value and
adding one: that's simply `-p`, the additive inverse of `p` in two's
complement!  We can use `-p & ~p == 0`.  That's one fewer
instruction on the critical path of our dependency graph (down to two, since we can [`test` whether `and`ing yields zero](https://www.felixcloutier.com/x86/test)), and still only
uses simple instructions that are unlikely to be port constrained.

Let's check our logic by enumerating all byte-sized values.

    CL-USER> (dotimes (p 256)
               (when (zerop (logand (- p) (lognot p) 255))
                 (format t "0b~2,8,'0r~%" p)))
    0b00000000
    0b10000000
    0b11000000
    0b11100000
    0b11110000
    0b11111000
    0b11111100
    0b11111110
    0b11111111

These *are* the bytes we're looking for (in ascending rather
than descending order)!

Remember the power of borrows
-----------------------------

I hope the examples above communicated a pattern I often observe when
mangling bits: operations that are annoying (not hard, just a bit more
complex than we'd like) in the bitmap domain can be simpler in two's
complement arithmetic.  Arithmetic operations are powerful mutators
for bitmaps, but they're often hard to control.  Subtracting or adding
1 are the main exceptions: it's easy to describe their impact in terms
of the low bits of the bitmap.  In fact, we can extend that trick to
subtracting or adding powers of two: it's the same carry/borrow chain effect as for 1,
except that bits smaller than the power of two pass straight 
through...
which might be useful when we expect a known tag followed by a ULEB value that must be decoded.

If you find yourself wishing for a way to flip ranges of bits in a
data-dependent fashion, it's always worth considering the two's
complement representation of the problem for a couple minutes.  Adding
or subtracting powers of two doesn't always work, but the payoff is
pretty good when it does.

P.S., [Wojciech Muła offers a different 3-operation sequence with `-p`](http://0x80.pl/notesen/2016-10-16-detecting-bit-pattern.html)
to solve damageboy's problem.
That's another nice primitive to generate bitmasks dynamically.

<small>Thank you Ruchir for helping me clarify the notation around the ULEB section.</small>

<p><hr style="width: 50%"></p>
