---
layout: post
title: "Simpler tree hashing with EPH and ENH"
date: 2025-04-25 15:36:23 -0400
comments: true
draft: true
hidden: true
categories: 
---

In the past decade (since Intel introduced PCLMULQDQ, and ARM the similar PMULL),
we've seen [a fair](https://arxiv.org/abs/1503.03465) [number](https://arxiv.org/abs/2104.08865) 
[of throughput-oriented](https://pvk.ca/Blog/2020/08/24/umash-fast-enough-almost-universal-fingerprinting/)
universal string hash functions,
all based on the [NH](https://web.cs.ucdavis.edu/~rogaway/umac/umac_thesis.pdf#page=41)
or [PH](https://arxiv.org/pdf/1503.03465#page=6)
mixing functions.
With AVX-512, [Alder Lake can computationally sustain 64 bytes of PH per cycle, and Zen 4 around half that](https://arxiv.org/pdf/1503.03465#page=6);
that's easily memory-bound, making PH an attractive building block.

The main issue with linearly accumulating EH/PH-mixed values (and other universal vector hash functions) is that
the amount of key material (random parameters) scales linearly with the input size.
That's why PH and NH are often restricted to fixed-size block hashing,
and combined with a polynomial hash to handle larger inputs.
The downside of such constructions is that the higher-level polynomial hash is usually
significantly slower than NH/PH due to the polynomial's finite field (that's why we first compress blocks with the latter),
and, more importantly, that the collision probability of such universal string hashes scales linearly with the input size.

The [Badger MAC](https://eprint.iacr.org/2004/319) shows that NH is not only almost-universal,
but almost\\(-\Delta-\\)universal with respect to integer addition.
This means we can obtain an almost-universal ENH hash function over pairs of pairs by
mixing the first pair with NH and adding the result to the second pair.
That's obviously attractive for a tree hash, and forms the core of [Halftime Hash](https://arxiv.org/abs/2104.08865).

Unlike vector hashes that accumulate mixed values linearly, parameters for tree hashes scale logarithmically with the input size,
and unlike polynomial string hashes, the collision probability scales logarithmically with the input size.
For binary ENH and EPH, 64 distinct mixers (NH or PH parameters) suffice to hash up to \\(2^{64}\\) input blocks.
Each reduction in the tree computes one NH or PH, so we end up doing the same amount of work as a
linear accumulation (e.g., 64 bytes per cycles on Alder Lake), except with exponentially less key material.
Tree hashing with ENH or EPH addresses the main downside of EH and PH accumulation, while preserving its throughput.

However, there's another downside: the computation becomes more complex, especially for incremental hashing... or does it?

Iterative tree hashing with ENH/EPH
-----------------------------------

Let's abstract out ENH/EPH a little, and say we work with mixers \\(M\_i\\) such that \\(M\_i(x) \oplus y\\) forms
a family of almost-universal hash functions (from pairs of blocks to blocks).

- Tree hashing one block is simply the identity.
- Tree hashing two blocks is \\(x_1 \oplus M_0(x_0).\\)
- Tree hashing four blocks is \\(x_3 \oplus M_0(x_2) \oplus M_1(x_1 \oplus M_0(x_0)).\\)

The last example shows how tree hashing reuses parameters:
\\(M_0(x_2)\\) doesn't actually interact with \\(M_0(x_0)\\), with \\(M_1\\) in the way.

I'm not sure how to implement this pattern without explicit divide and conquer, or at least a reduction stack,
and those are hard to micro-optimise (you can inline the leaves, but after that?).

The trick is that we don't *have* to implement tree hashing that way, at least when using almost\\(-\Delta-\\)universal mixers.
The only thing that prevents us from reusing parameters (e.g., iteratively mixing with the same \\(M_0\\)) is the assumption, key to universal hashing, that the inputs are generated independently from the parameters.
That assumption would obviously be violated if we fed the output of \\(M_0\\) back to \\(M_0\\), or, in the case of almost\\(-\Delta-\\)universality, directly combined \\(M_0(x) \oplus M_0(y).\\)

We can avoid such direct contact by applying a fresh mixer \\(M_n\\) before reusing \\(M_i, 0 \leq i < n.\\)

Concretely, start with an accumulator \\(a_0 = 0.\\)

- Hash in the first block \\(x_0\\) with \\(a_1 = x_0 \oplus a_0 = x_0.\\)
- Hash in the second block \\(x_1\\) with \\(a_2 = x_1 \oplus M_0(a_1).\\)
- Hash in \\(x_2\\) with \\(a_3 = x_2 \oplus M_1(a_2).\\)
- Hash in \\(x_3\\) with \\(a_4 = x_3 \oplus M_0(a_3).\\)

Substituting all that yields \\(a_4 = x_3 \oplus M_0(x_2 \oplus M_1(x_1 \oplus M_0(x_0)));\\)
similar to tree hashing's \\(x_3 \oplus M_0(x_2) \oplus M_1(x_1 \oplus M_0(x_0))\\),
but definitely not identical.

With ENH and EPH, the danger with respect to the universal hashing model is when we reuse functions.
The only mixer \\(M_i\\) used multiple times in \\(a_4\\) is \\(M_0\\).
Why is it safe to do so?

I claim this works because the second time we use \\(M_0,\\) 
in \\(a_4 = x_3 \oplus M_0(a_3),\\)
the distribution of \\(a_3\\) is independent from \\(M_0,\\)
for the same reason regular tree hashing works:
\\(M_1\\)'s independent random parameters broke any dependence on \\(M_0.\\)

Assume the output of \\(M_0\\) contains a pattern such that collisions are more likely with
\\(M_0(x_2) \oplus M_\emptyset(\cdot \oplus M_0(\cdot)),\\)
where \\(M_\emptyset\\) is NH or PH with parameters fixed (without loss of generality) to 0.
Any such pattern is destroyed when we use \\(M_1\\) instead of \\(M_\emptyset:\\)
the distribution retains all clumpiness from \\(M_0,\\)
but the clumps are shifted by a random independently generated parameter,
so there's no reason for a set of values that's more (or less) likely to be generated by \\(M_\emptyset\\) to also be more (resp. less) probable with \\(M_1.\\)

Generating indices for iterative tree hashing
---------------------------------------------

The new formulation is useful because we always work with one accumulator,
much simpler than a reduction stack of accumulators.
It's not necessarily obvious how to quickly figure out which parameter index to use.

That's not an issue when everything is unrolled, but does matter if we
want to handle large inputs, particularly for incremental hashing.
There's a simple pattern to the indices:
when hashing in block \\(x_1,\\) we first mix the accumulator with \\(M_0;\\)
block \\(x_2\\) mixes the accumulator with \\(M_1;\\)
block \\(x_3\\) mixes with \\(M_0\\) again.

The mixer index is simply a count of trailing zero bits in the new block's index!
The cascading pattern from the carry when we increment by 1 exactly ensures that we use a fresh mixer before reusing lower-indexed mixers,
with the recursive pattern we need to let the number of mixers (parameters) scale logarithmically with the number of blocks.

In short, the pseudocode for this alternative tree hash looks like:

```
acc = None
for idx, block in enumerate(input):
    if idx == 0:
        acc = block
    else:
        param = params[trailing_zeros(idx)]
        acc = block ^ mix(param, acc)  # mix is NH or PH
```

We'll still want to unroll the inner loop a couple times for performance (mostly to cache frequently used parameters in registers), but this is still pretty sweet!
