---
layout: post
title: "Three-universal hashing in four instructions"
date: 2017-04-02 18:10:10 -0400
comments: true
categories: 
---

… with one caveat: the hash functions only generate one bit.

{% codeblock "hash.c" %}
bool
bit_hash(uint64_t x, uint64_t table, uint64_t bit)
{
    /* table is a random uniform uint64_t, bit is a random bit. */
	return __builtin_parityll((x & table) ^ bit);
}
{% endcodeblock %}

With hardware popcount, this compiles to something like the following.

{% codeblock "hash.s" %}
        andq    %rsi, %rdi # x & table
        xorl    %eax, %eax # work around a hardware perf bug in popcnt
        xorq    %rdi, %rdx # () ^ bit
        popcntq %rdx, %rax # get the popcount
        andl    $1, %eax   # isolate parity
{% endcodeblock %}

This should raise a few questions:

1. Why?
2. Why does it work?
3. Is it useful?

Someone with a passing familiarity with x86 would also ask why we use
`popcnt` instead of checking the parity flag after `xor`.
Unfortunately, the parity flag only considers the least significant
byte of the result (:

One-bit hash functions: but why?
--------------------------------

When implementing something like the
[hashing trick](https://arxiv.org/abs/0902.2206) or
[count sketches (PDF)](https://www.cs.rutgers.edu/~farach/pubs/FrequentStream.pdf),
you need two sets of provably strong hash functions: one to pick the
destination bucket, and another to decide whether to increment or
decrement by the sketched value.

One-bit hash functions are ideal for the latter use case.

How does that even work?
------------------------

The bitwise operations in `bit_hash` implement a degenerate form of
[tabulation hashing](https://arxiv.org/abs/1011.5200).  It considers
the 64 bit input value `x` as a vector of 64 bits, and associates a
two intermediate output values with each index.  The naïve
implementation would be something like the following.

{% codeblock "hash_slow.c" %}
bool
bit_hash_slow(uint64_t x, bool random_table[64][2])
{
    int acc = 0

    for (size_t i = 0; i < 64; i++, x >>= 1) {
        acc ^= random_table[i][x & 1];
    }

    return acc;
}
{% endcodeblock %}

Of course, the representation of `random_table` is inefficient, and we
should hand-roll a bitmap.  However, the loop itself is a problem.

The trick is to notice that we can normalise the table so that the
value for `random_table[i][0]` is always 0: in order to do so, we have
to fix the initial value for `acc` to a random bit.  That initial
value is the hash value for `0`, and the values in
`random_table[i][1]` now encode whether a non-zero bit `i` in `x`
flips the hash value or leaves it as is.

The `table` argument for `bit_hash` is simply the 64 bits in
`random_table[i][1]`, and `bit` is the hash value for `0`.  If bit `i`
in `table` is 0, bit `i` is irrelevant to the hash.  If bit `i` in
`table` is 1, the hash flips when bit `i` in `x` is 1.  Finally, the
parity counts how many times the hash was flipped.

Is it even useful?
------------------

I don't think so.  Whenever we need a hash bit, we also want a hash
bucket; we might as well steal one bit from the latter wider hash.
Worse, we usually want a few such bucket/bit pairs, so we could also
compute a wider hash and carve out individual bits.

I only thought about this trick because I've been reading a few
empirical evaluation of sketching techniques, and a few authors find
it normal that computing a hash bit doubles the CPU time spent on
hashing.  It seems to me the right way to do this is to map
columns/features to not-too-small integers (e.g., universal hashing to
`[0, n^2)` if we have `n` features), and apply strong hashing to
these integers.  Hashing machine integers is *fast*, and we can always
split strong hashes in multiple values.

In the end, this family of one-bit hash functions seems like a good
solution to a problem no one should ever have.  But it's still a cute trick!
