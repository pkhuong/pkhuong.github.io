---
layout: post
title: "One-bit twist"
date: 2022-02-28 23:02:42 -0500
draft: true
hidden: true
comments: true
categories: 
---

[Tabulation hashing](https://arxiv.org/abs/1011.5200) is one
of a few practical approaches that guarantee 
[high-independence in hash functions](https://en.wikipedia.org/wiki/K-independent_hashing).
A priori, \\(k-\\)independence seems like an obscure academic concept.
That's probably because hash tables don't care too much about higher
independence in practice: although linear probing needs high
independence to guarantee constant or logarithmic expected time per
operation, in practice, we just take the hit if we're unlucky.

However, hash functions have many more applications than just hash
tables; for example, statistical estimators.  In such applications,
knowing exactly how much independence a hash function provides and how
little an algorithm can work with is crucial.  Plugging an overly weak
independent hash function in, e.g.,
[sketching data structures](https://en.wikipedia.org/wiki/Count%E2%80%93min_sketch)
becomes a *correctness* problem, rather than one of mere performance.
I find that to be a particularly important consideration in practice
because:

1. we tend to use statistical sketches when computing the exact ground
   truth is impractical if not infeasible;
2. we know sketches can *sometimes* fail with extremely inaccurate
   estimates... an ill-suited hash function simply makes such failures
   more probable than documented.

Given these two facts, it seems hard to empirically determine whether
one is or isn't running into issues caused by a weakly independent
hash function.  That's why I think it makes sense to err on the side
of caution and use provably strong enough hash functions in
statistical sketches and similar algorithms.

[Tight analyses](https://arxiv.org/abs/1011.5200) show that tabulation
hashing punches above its 3-independence weight.
[Twisted tabulation hashing](https://epubs.siam.org/doi/pdf/10.1137/1.9781611973105.16), a small tweak on top of regular tabulation hashing,
[does even better](https://arxiv.org/abs/1505.01523).  Unfortunately,
its standard presentation is awkward to implement efficiently for
round (32 or 64 bits) output sizes.

This post shows an alternative bit-at-a-time approach that "twists"
more efficiently.

Regular tabulation hashing
--------------------------

Tabulation hashing is usually presented in terms of look-up tables
from bytes to the hash range (e.g., 64-bit integers).  It's certainly
an efficient way to implement the scheme, at least for applications
where the keys are short and the tables hot in cache.

I believe this section's bit-oriented presentation makes more sense.
The byte-driven look-up table becomes a mere instance of 
[the four Russians' trick](https://en.wikipedia.org/wiki/Method_of_Four_Russians),
where a \\(2^k\\) space blow-up buys us a \\(k\times\\) speedup.

In order to tabulation-hash an \\(n-\\)bit input value to an
\\(m-\\)bit result, we need \\(n\\) pairs of \\(m-\\)bit integers.
Each bit in the input selects one \\(m-\\)bit integer from the
corresponding pair, and tabulation hashing `xor`s all the selected
integers together.

{% codeblock tabulation.py %}
def tabhash(value, table):
    """Computes the tabulation hash for `value`, a stream of bits,
    and a `table` random uniform integers."""
    acc = 0
    for bit, pair in zip(value, table):
        acc ^= pair[bit]
    return acc
{% endcodeblock %}

We could also assume the table is normalised such that
`table[_][0] = 0`, and compensate for that normalisation with a
randomly generated uniform initial value.  This tweak is useful when
vectorising a frugal bit-oriented implementation: I've seen 34-36
cycles for a 64 bit to 64 bit hash with AVX, on a 2 GHz EPYC 7713.

{% codeblock tabulation.py %}
def tabhash_mask(value, initial, normalised_table):
    """Computes the tabulation hash for `value`, a stream of bits,
    and a `table` random uniform integers."""
    acc = initial
    for bit, hash in zip(value, normalised_table):
        if bit != 0:
            acc ^= hash
    return acc
{% endcodeblock %}

It's much more common to index the look-up table with \\(k\\) bits at a
time, which divides the number of loop iterations by \\(k.\\)

{% codeblock tabulation.py %}
def tabhash_chunked(value, k, ktable):
    """Computes the tabulation hash for `value`, a stream of bits,
    and a `table` random uniform integers indexed `k` bits at a time."""
    acc = 0
    for position, bits in enumerate(zip(*([iter(value)] * k))):
        # convert a list of `k` bits to a k-bit unsigned integer value.
        key = bits_to_int(bits)
        acc ^= ktable[position][key]
    return acc
{% endcodeblock %}

Letting \\(k = 8\\) is often a good choice, as long as one can afford
the resulting large look-up table (\\(8 \times 256 \times w = 2048w\\)
bits when hashing 64-bit inputs to \\(w-\\)bit results).  As long as
the look-up table is all cached, it's easy to hit 12-13 cycles for a
64 bit to 64 bit hash, on a 2 GHz EPYC 7713.

In the classic formulation, each run of \\(k\\) bytes is a
"character."  For example, a 64-bit value split in bytes would be a
string of 8 characters of one byte each.

{% codeblock tabulation.c %}
uint64_t
tabhash_byte_chunked(uint64_t x, const uint64_t *byte_table)
{
        uint64_t acc;

        acc = byte_table[256 * 0 + ((x >> 0) & 0xFF)];
        acc ^= byte_table[256 * 1 + ((x >> 8) & 0xFF)];
        acc ^= byte_table[256 * 2 + ((x >> 16) & 0xFF)];
        acc ^= byte_table[256 * 3 + ((x >> 24) & 0xFF)];
        acc ^= byte_table[256 * 4 + ((x >> 32) & 0xFF)];
        acc ^= byte_table[256 * 5 + ((x >> 40) & 0xFF)];
        acc ^= byte_table[256 * 6 + ((x >> 48) & 0xFF)];
        acc ^= byte_table[256 * 7 + ((x >> 56) & 0xFF)];

        return acc;
}
{% endcodeblock %}

Of course, if we're going to look up 8 bits at a time for each
`position`, we don't have to derive 256 values by `xor`-ing together
one of 8 random values (e.g., `ktable[0][3] = table[0][1] ^
table[1][1] ^ table[2][0] ...`): we might as well populate
`ktable` with independently generated integers.  As far as I
understand, this does not *provably* help, but can't hurt: it can be
seen as `xor`-combining the initial bit-driven table with an
independent random function for each byte.

Bit-twisted tabulation hashing
--------------------------

Pătrașcu and Thorup's [twisted tabulation hashing](https://epubs.siam.org/doi/pdf/10.1137/1.9781611973105.16)
provably strengthens regular tabulation hashing by deriving an
additional character from all but the last character (or any other
one) in the initial input, and `xor`ing that last character with the
newly derived one before looking up the result in a random table.

Given a character size of \\(c\\) bits, the additional character is
generated with a \\(c \times c\\)-bit look-up table for each character
in the input.  When the result size is shorter than the word size
(e.g., a 56-bit output on 64-bit machines), we can conveniently find
this additional character in unused output bits.  Otherwise, we must
perform double the lookups.  In C, the general case might look like
the following.

{% codeblock tabulation.c %}
uint64_t
twisted_hash_byte(uint64_t x, const uint64_t *byte_table,
    const uint8_t *twisting_table)
{
        uint64_t acc;
        uint8_t additional;

        acc = byte_table[256 * 0 + ((x >> 0) & 0xFF)];
        acc ^= byte_table[256 * 1 + ((x >> 8) & 0xFF)];
        acc ^= byte_table[256 * 2 + ((x >> 16) & 0xFF)];
        acc ^= byte_table[256 * 3 + ((x >> 24) & 0xFF)];
        acc ^= byte_table[256 * 4 + ((x >> 32) & 0xFF)];
        acc ^= byte_table[256 * 5 + ((x >> 40) & 0xFF)];
        acc ^= byte_table[256 * 6 + ((x >> 48) & 0xFF)];


        additional = twisting_table[256 * 0 + ((x >> 0) & 0xFF)];
        additional ^= twisting_table[256 * 1 + ((x >> 8) & 0xFF)];
        additional ^= twisting_table[256 * 2 + ((x >> 16) & 0xFF)];
        additional ^= twisting_table[256 * 3 + ((x >> 24) & 0xFF)];
        additional ^= twisting_table[256 * 4 + ((x >> 32) & 0xFF)];
        additional ^= twisting_table[256 * 5 + ((x >> 40) & 0xFF)];
        additional ^= twisting_table[256 * 6 + ((x >> 48) & 0xFF)];

        acc ^= byte_table[256 * 7 + (additional ^ ((x >> 56) & 0xFF))];

        return acc;
}
{% endcodeblock %}

Double the operations, half the speed (around 22 cycles/hash on an EPYC
7713).

Working with bits, i.e., \\(c = 1\\), will let us recover most of
plain tabulation hashing's performance by computing `additional` with
a SWAR algorithm.  We just saw that \\(c = 1\\) does not preclude an
implementation that indexes in look-up tables with full bytes, so we
will do so while preserving the speed of byte-at-a-time processing for
the rest of the hash.

When the additional character is a single bit, `tabhash_mask` shows we
can compute it with \\(n - 1\\) bits, and an initial value.  For each
input bit except the last, that bit equiprobably affects or doesn't
affect the derived bit.  The result indexes in a randomly generated
look-up table, so we don't even need a random initial value: the
impact of that initial value is equivalent to permuting the look-up
table, and a look-up table of random uniform variates permutes to
another look-up table of random uniforms.

So, given the first 63 bits, we can compute the additional character
by `and`ing it with a random uniform 63-bit integer, and taking the
parity of the result.  When we want to directly compute the result of
`xor`ing that with the 64th bit, we can `and` all 64 input bits with a
random uniform integer in \\([2^{63}, 2^{64})\\), and take the parity
of *that*.

Parity isn't a common hardware function anymore, at least not on full
64-bit integers... but population count is, and 
`parity(x) == popcount(x) % 2`.

I see two ways to use this simple tabulation-hash for bit outputs.  We
we could let regular tabulation hashing run its course, compute the
derived bit independently, and use that to conditionally `xor` in an
additional random integer.  Alternatively, We could compute the
derived bit, `xor` that with the distinguished bit, and *then* let
regular byte-indexed tabulation hashing proceed on the modified input.

The former might make sense in a vectorised bit-at-a-time
implementation.  However, as long as we're paying for full-blown table
lookups, we might as well `xor` the derived bit in before one of the
lookups.  In C, bit-twisted tabulation hashing can look like the
following, which runs in at 14 cycles per hash on my EPYC 7713, i.e.,
around one more cycle than regular tabulation hashing.

{% codeblock bit_twisted.c %}
uint64_t
hash_twisted(uint64_t x, uint64_t derived_mask, const uint64_t *tab)
{
        uint64_t acc;
        uint8_t derived;

        /* We'll mix the derived bit with the *56th* bit. */
        assert(((derived_mask >> 56) & 1) == 0);

        derived = __builtin_popcountll(x & derived_mask) & 1;

        /* This unshifted input lets us start work early. */
        acc = tab[256 * 0 + ((x >> 0) & 0xFF)];
        acc ^= tab[256 * 1 + ((x >> 8) & 0xFF)];
        acc ^= tab[256 * 2 + ((x >> 16) & 0xFF)];
        acc ^= tab[256 * 3 + ((x >> 24) & 0xFF)];
        acc ^= tab[256 * 4 + ((x >> 32) & 0xFF)];
        acc ^= tab[256 * 5 + ((x >> 40) & 0xFF)];
        acc ^= tab[256 * 6 + ((x >> 48) & 0xFF)];
        /* By the time we get here, `derived` should be available. */
        acc ^= tab[256 * 7 + (((x >> 56) & 0xFF) ^ derived)];

        return acc;
}
{% endcodeblock %}

Starting with a bit-oriented view of tabulation hashing lead us to a
much more efficient version of "twisting," without giving up any of
[twisted tabulation hashing's surprising strength](https://arxiv.org/abs/1505.01523).

Why does 64 bit \\(\rightarrow\\) 64 bit hashing matter?
-----------------------------------------

The whole exercise seems a priori pointless: we're doing a lot of work
to "hash" 64 bit values down to... 64 bits.  Of course, the same ideas
work at least as well when reducing to fewer bits, but the \\(2^w
\rightarrow 2^w\\) case actually does matter in practice.

It's important to remember that tabulation hashing doesn't just reduce
the dimensionality of its input, it also shuffles away correlation or
clumping in the inputs.  Twister tabulation hashing provably does so
well enough for, e.g., [minwise hashing](https://arxiv.org/abs/1404.6724).

Strong hash functions for fixed-size inputs are also important because
they let us construct strong and fast hash functions modularly: we can
use a fast but merely 2-independent (i.e., [universal](https://en.wikipedia.org/wiki/Universal_hashing))
hash function like [UMASH](https://github.com/backtrace-labs/umash) to
reduce large input data to 64 or 128 bits with minimal chances of
collision, and shuffle the result with a strong hash function like
twisted tabulation hashing.

The bulk of the work is performed by the first function, which only
has to avoid collisions; the strong hash functions takes the first
function's 2-independent output, and bootstraps it into something
3-independent or more.  As long as the probability of *any* collision
in the first hash function's output is negligible, we can treat the
composition of the fast-but-weak and
\\(k-\\)independent-but-fixed-size functions as
fast-and-\\(k-\\)independent hash function.

[We only know \\(2-\\)independent variable-length hash functions](https://arxiv.org/abs/1008.1715),
so this construction is pretty useful when we want to drive
statistical estimates by hashing large records.
