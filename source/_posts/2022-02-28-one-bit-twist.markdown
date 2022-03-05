---
layout: post
title: "One-bit twist"
date: 2022-02-28 23:02:43 -0500
draft: true
hidden: true
comments: true
categories: 
---

## Current status

Twisted tabulation hashing needs an actual random function for the
twisted head (the rest may use simple tabulation hashing, i.e., random
boolean affine transforms).  The twisted head must also use a large
alphabet such that \\(|\Sigma|^\eps > \log u\\).  Pătrașcu and Thorup
seem to think \\(|\Sigma| = 256\\) suffices.  This means we could
reduce the memory requirements of twisted tabulation hashing to only
need a full byte look-up table for the twisted head, and an optimally
represented affine transform.

On a Zen 3, there is enough MLP to sustain at least 1 LUT load per
cycle, even from L2.  That's a baseline of 8 input bits
consumed/cycle.  In fact, that's less than the theoretical limit: we'd
expect 2 or 3 per cycle.  However, the number of instructions gets in
the way: we observe > 4 IPC for simple LUT tabulation hashing.

The bit vector approach, when implemented as a rotation of the input
bit vector + FMA (bitwise and / xor), can achieve ~1 dot product /
lane / cycle.  We're on 256-bit AVX2, so that's only 4 input
bits/cycle.  Here as well, the bottleneck is the sheer number of
instructions.  VPTERNLOG would combine the `and / xor` FMA in one
instruction and AVX-512 would immediately double the width and offer a
native lane-wise rotation.  I believe the matrix-vector view on
AVX-512 could match the speed of tabulation hashing with byte lookups,
without the wasted space.

There's also a ~15-20c latency to roundtrip from vector to GPR to
vector, but that's easily hidden by overlapping executions.

And of course, vector gathers still suck: the throughput of loads is
one fetch per cycle, so at best we're equivalent to scalar loads.  In
practice, uops scheduling overhead seems to get us to ~half the speed
(and 2 IPC).

Given the above, unless you have AVX 512 or a very wide uarch (M1?) it
probably makes sense to implement twisted tabulation hashing with byte
lookups and a 56 bit output: the number of loads is what matters, so
stealing one byte for the twister avoids halving the speed.  Even with
128 bit inputs, we can hash at a ~16 cycles/input throughput: L2 is
more than capable of feeding 1 miss/cycle.

56 bits ought to be enough for estimators like minwise or KMV?

<hr width="50%">

[Tabulation hashing](https://arxiv.org/abs/1011.5200) is one
of a few practical approaches that guarantee 
[high-independence in hash functions](https://en.wikipedia.org/wiki/K-independent_hashing).
A priori, \\(k-\\)independence seems like an obscure academic concept.
That's probably because hash tables don't care too much about higher
independence in practice: although linear probing needs high
independence to guarantee constant or logarithmic expected time per
operation, in practice, we just take the hit if we're unlucky.

However, hash functions have many more applications than just hash
tables; for example, statistical estimators.  When implementing
estimators, knowing exactly how much independence a hash function
provides (how close it is to a true random function) and how little an
algorithm can work with is crucial.  Plugging a hash function with
overly low independence in, e.g.,
[sketching data structures](https://en.wikipedia.org/wiki/Count%E2%80%93min_sketch)
becomes a *correctness* problem, rather than one of mere performance.
I find that to be an important consideration in practice because:

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

Both simple and twisted tabulation hashing scale poorly with the input
size: splitting up 128 bits in bytes, the classic character size,
requires 16 look-up tables of 256 entries each, i.e., 32 KB for a
64-bit output.  This post shows how a bit-at-a-time approach is more
frugal in space and comparably practical for longer inputs.

Simple tabulation hashing
-------------------------

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
integers together: \\(h(x) = \bigoplus_{i\in[n]} h_i(x_i),\\) where
each \\(x_i\\) is a single bit of \\(x.\\)

{% codeblock tabulation.py %}
def tabhash(value, table):
    """Computes the tabulation hash for `value`, a stream of bits,
    and a `table` of random uniform integers."""
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
    and a `normalised_table` of random uniform integers."""
    acc = initial
    for bit, hash in zip(value, normalised_table):
        if bit != 0:
            acc ^= hash
    return acc
{% endcodeblock %}

The loop above is now clearly equivalent to an affine transform with
bit-matrices, \\(h(x) = A x \oplus b,\\) where the bit matrix \\(A\\)
and the bit vector \\(b\\) are generated uniformly at random.  This
affine transformation presentation hopefully makes it obvious that
such transformations are \\(3-\\)independent, but not
\\(4-\\)independent: given the values of \\(A x_1 \oplus b\\) and \\(A
x_2 \oplus b\\) for any \\(x_1, x_2\\), \\(A x_3 \oplus b\\) may still
take any value in the output range.  However, given three values
\\(h(x_1), h(x_2), h(x_3)\\) for
\\(x_1 = 0)\\) and \\(x_2 = \mathbf{e}_i \neq x_3 = \mathbf{e}_j\\),
it's possible to pin down \\(b\\), and as well as two columns of
\\(A\\)... enough to predict \\(A (\mathbf{e}_i \oplus \mathbf{e}_j)
\oplus b.\\)

It's much more common to index the look-up table with \\(k\\) bits at a
time in order to divide the number of iterations by \\(k,\\) a
[classic application of the Four Russians' trick](https://www.amazon.com/Design-Analysis-Computer-Algorithms/dp/0201000296).

{% codeblock tabulation.py %}
def tabhash_chunked(value, k, ktable):
    """Computes the tabulation hash for `value`, a stream of bits, and
    a `ktable` of random uniform integers indexed `k` bits at a time."""
    acc = 0
    for position, bits in enumerate(zip(*([iter(value)] * k))):
        # convert a list of `k` bits to a k-bit unsigned integer value.
        key = bits_to_int(bits)
        acc ^= ktable[position][key]
    return acc
{% endcodeblock %}

Letting \\(k = 8\\) is often a good choice, as long as one can afford
the resulting large look-up table (\\(8 \cdot 256 \cdot w = 2048w\\)
bits to hash 64-bit inputs into \\(w-\\)bit results).  When
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

Mixing a 64-bit input to a 64-bit output (or 32 to 32) is close to the
sweet spot for byte-drive tabulation hashing: longer inputs require
impractically large look-up tables that fill or exceed the L1 cache,
and shorter outputs can more easily make use of parity as a dot
product in bitwise linear algebra.

Let's first see how we can implement 128 \\(\rightarrow\\) 64 bit
tabulation hashing bit by bit.

128 \\(\rightarrow\\) 64 bit tabulation hashing
-----------------------------------------------

Byte-driven tabulation hashing scales perfectly linearly with the
number of bytes in the input, until the look-up tables spill out of
one cache level into the next.  Vectorised bit matrix-vector
multiplications instead incur a fixed latency overhead when loading
values in vector registers and when extracting a scalar value from
accumulators in vector registers.  Thus, while byte tabulation hashing
needs \\(1/3\\) as much as time as a vectorised bit matrix-vector
multiplication for \\(64\times 64\\) matrices, the 
different for \\(128\times 64\\) matrices.

A contemporary x86 can sustain 2 AVX loads per cycle, i.e., 512 bits
per cycle.  This means we can iterate through the \\(128 \cdot 64 =
8192\\) bits of a coefficient matrix in 16 cycles. The remaining
latency comes from fixed startup and teardown logic.



Bit-twisted tabulation hashing
--------------------------

Pătrașcu and Thorup's [twisted tabulation hashing](https://epubs.siam.org/doi/pdf/10.1137/1.9781611973105.16)
provably strengthens regular tabulation hashing by deriving an
additional character from all but the last character (or any other
one) in the initial input, and `xor`ing that last character with the
newly derived one before looking up the result in a random table.

Given characters of size \\(s\\) bits (alphabet \\(\Sigma\\) of
cardinality \\(2^s\\)), the additional character is generated with an
\\(s \times s\\)-bit look-up table for each original character in the
input.  When the result size is shorter than the word size (e.g., a
56-bit output on 64-bit machines), we can conveniently find this
additional character in unused output bits.  Otherwise, we must
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

Working with bits, i.e., \\(s = 1\\), will let us recover most of
plain tabulation hashing's performance by computing `additional` with
a SWAR algorithm.  We just saw that \\(s = 1\\) does not preclude an
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
random uniform integer in \\(\left[2^{63}, 2^{64}\right)\\), and take
the parity of *that*.

Parity isn't a common hardware function anymore, at least not on full
64-bit integers... but population count is, and 
`parity(x) == popcount(x) % 2`.  That's
[the one-bit hash function I blogged about in 2017](https://pvk.ca/Blog/2017/04/02/three-universal-hashing-in-four-instructions/).

I see two ways we can use this simple tabulation hash function for bit
outputs.  We we could let regular tabulation hashing run its course,
compute the derived bit independently, and use that to conditionally
`xor` in an additional random integer.  Alternatively, we could
compute the derived bit, `xor` that with the distinguished bit, and
*then* let regular byte-indexed tabulation hashing proceed on the
modified input.  This corresponds to the "less efficient" but
mathematically convenient version of twisted tabulation hashing in
[Section 1.2 of Pătrașcu and Thorup](https://epubs.siam.org/doi/pdf/10.1137/1.9781611973105.16#page=2).

The former might make sense in a vectorised bit-at-a-time
implementation.  However, while we're already paying for full-blown
table lookups, we might as well `xor` the derived bit in before
looking up.  In C, bit-twisted tabulation hashing can look like the
following, which runs in at 14 cycles per hash on my EPYC 7713, i.e.,
around one more cycle per hash than regular tabulation hashing.

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
[its surprising strength](https://arxiv.org/abs/1505.01523).

A bit of linear algebra shows we can do even better: the "twisting"
step is a linear transform on bit vectors (multiplication by a
half-arrowhead-shaped matrix), and affine transforms are closed under
composition.  Now that we view simple tabulation hashing as an affine
bitwise transformation accelerated with a precomputed data structure,
it's clear that twisting can be free!

```
[1    ... ]
[ 1   ... ]
[  1  ... ]
    ...
[ ...  1  ]
[ ...   1 ]
[???...??1]
```

Free twisting definitely sounds a bit odd to me, but feels compatible with
the "Insufficiency of simple tabulation for general Chernoff bounds"
subsection of [Pătrașcu and Thorup](https://epubs.siam.org/doi/pdf/10.1137/1.9781611973105.16#page=3).
Essentially, twisted tabulation hashing works around "weak" sets of
random parameters.  The bit matrix view shows that we can instead
avoid generating such weak parameters in the first place.

For twisted tabulation hashing, unlike simple tabulation hashing,
replacing our carefully structured affine transform with a uniformly
generated random one would hurt: it's this exact structure that avoids
particularly weak parameters.

This linear algebraic trick only works because neither simple
tabulation hashing nor twisted tabulation hashing impose any
constraint on the size of the alphabet.  It does *not* extend to
[double tabulation](https://arxiv.org/abs/1311.3121), where the
additional independence grows with the alphabet size 
\\(|\Sigma| = 2^s\\) and shrink with the number of characters in
the input.

Why does 64 bit \\(\rightarrow\\) 64 bit hashing matter?
--------------------------------------------------------

The whole exercise seems a priori pointless: we're doing a lot of work
to "hash" 64 bit values down to... 64 bits.  Of course, the same ideas
work at least as well when reducing to fewer bits, but the \\(2^w
\rightarrow 2^w\\) case actually does matter in practice.

It's important to remember that tabulation hashing doesn't just reduce
the dimensionality of its input, it also shuffles away correlation or
clumping in the inputs.  Twisted tabulation hashing provably does so
well enough for, e.g., [minwise hashing](https://arxiv.org/abs/1404.6724).

Strong hash functions for fixed-size inputs are also important because
they let us construct strong and fast hash functions modularly: we can
use a fast but merely 2-independent (i.e., [universal](https://en.wikipedia.org/wiki/Universal_hashing))
hash function like [UMASH](https://github.com/backtrace-labs/umash) to
reduce an input dataset to 64 or 128 bits with minimal chances of
collision, and shuffle the result with a strong hash function like
twisted tabulation hashing.

The bulk of the work is performed by the first function, which only
has to avoid collisions; the strong hash functions takes the first
function's 2-independent output, and bootstraps it into something
3-independent or more.  As long as the probability of *any* collision
in the first hash function's output is negligible, we can treat the
composition of the fast-but-weak with the
\\(k-\\)independent-but-fixed-size functions as a
fast-and-\\(k-\\)independent hash function.

[We only know \\(2-\\)independent variable-length hash functions](https://arxiv.org/abs/1008.1715),
so this construction is pretty useful when we want to drive
statistical estimates by hashing large records.
