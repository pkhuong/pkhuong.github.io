---
layout: post
title: "1.5x the PH bits for one more CLMUL"
date: 2020-10-31 18:30:19 -0400
comments: true
categories: 
---

<small>Turns out the part where I simply asserted a property was slightly off 😉. I had to go back to an older proof with weaker bounds, but that's not catastrophic: we still collide way more rarely than \\(2^{-70}\\).</small>

The core of [UMASH](https://github.com/backtrace-labs/umash) is a
hybrid [PH](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.105.9929&rep=rep1&type=pdf#page=3)/[(E)NH](https://eprint.iacr.org/2004/319.pdf#page=4)
block compression function.
That function is fast
(it needs one multiplication for each 16-byte "chunk" in a block),
but relatively weak:
despite a 128-bit output, the worst-case probability of collision is
\\(2^{-64}\\).

For a [fingerprinting](https://en.wikipedia.org/wiki/Fingerprint_(computing))
application, we want collision probability less than \\(\approx 2^{-70},\\)
so that's already too weak,
before we even consider merging a variable-length string of compressed
block values.

The [initial UMASH proposal](https://pvk.ca/Blog/2020/08/24/umash-fast-enough-almost-universal-fingerprinting/)
compresses each block with two independent compression functions.
Krovetz showed that we could do so while reusing most of the key material
(random parameters), with a [Toeplitz extension](http://krovetz.net/csus/papers/thesis.pdf#page=50),
and I simply recycled the proof for UMASH's hybrid compressor.

That's good for the memory footprint of the random parameters, but
doesn't help performance: we still have to do double the work to get
double the hash bits.

Earlier this month, [Jim Apple](https://github.com/jbapple) pointed me at
a [promising alternative that doubles the hash bit with only one more multiplication](https://link.springer.com/chapter/10.1007/978-3-662-46706-0_25).
The construction adds finite field operations that aren't particularly
efficient in software, on top of the additional 64x64 -> 128
(carryless) multiplication, so isn't a slam dunk win over a
straightforward Toeplitz extension.
However, Jim felt like we could "spend" some of the bits we don't need
for fingerprinting (\\(2^{-128}\\) collision probability is overkill
when we only need \\(2^{-70}\\)) in order to make do with faster operations.

Turns out he was right! We can use carryless multiplications by sparse
constants (concretely, xor-shift and one more shift) without any
reducing polynomial, *on independent 64-bit halves*... and still
collide with probability at most <s>2^{-126}</s> \\(2^{-98}\\).

The proof is fairly simple, but relies on a bit of notation for clarity.
Let's start by re-stating UMASH's hybrid PH/ENH block compressor in
that notation.

How does UMASH currently work?
------------------------------

The current block compressor in UMASH splits a 256-byte block \\(m\\)
in 16 chunks \\(m_i,\, i\in [0, 15]\\) of 128 bits each, and processes
all but the last chunk with a PH loop,

\\[ \bigoplus_{i=0}^{14} \mathtt{PH}(k_i, m_i), \\]

where

\\[ \mathtt{PH}(k_i, m_i) = (k_i \bmod 2^{64}) \oplus (m_i \bmod 2^{64})) \odot (\lfloor k_i / 2^{64} \rfloor \oplus \lfloor m_i / 2^{64} \rfloor) \\]

and each \\(k_i\\) is a randomly generated 128-bit parameter.

The compression loop in UMASH handles the last chunk, along with a
size tag (to protect against extension attacks), with
[ENH](https://eprint.iacr.org/2004/319.pdf#page=4):

\\[ \mathtt{ENH}(k, x, y) = ((k + x) \bmod 2^{64}) \cdot (\lfloor k / 2^{64}\rfloor + \lfloor x / 2^{64} \rfloor \bmod 2^{64}) + y \mod 2^{128}. \\]

The core operation in ENH is a full (64x64 -> 128) integer
multiplication, which has lower latency than PH's carryless
multiplication on x86-64.  That's why UMASH switches to ENH for the
last chunk.  We use ENH for only one chunk because combining multiple
NH values calls for 128-bit additions, and that's slower than PH's
xors.  Once we have mixed the last chunk and the size tag with ENH,
the result is simply xored in with the previous chunks' PH values:

\\[ \left(\bigoplus_{i=0}^{14} \mathtt{PH}(k_i, m_i)\right)  \oplus \mathtt{ENH}(m_{15}, k_{15}, \mathit{tag}). \\]

This function is annoying to analyse directly, because we end up
having to manipulate different proofs of almost-universality.  Let's
abstract things a bit, and reduce the ENH/PH to the bare minimum we need
to find our collision bounds.

Let's split our message blocks in \\(n\\) (\\(n = 16\\) for
UMASH) "chunks", and apply an independently sampled mixing function
to each chunk.  Let's say we have two messages \\(m\\) and
\\(m^\prime\\) with chunks \\(m_i\\) and \\(m^\prime_i\\), for \\(i\in
[0, n)\\), and let \\(h_i\\) be the result of mixing chunk \\(m_i,\\)
and \\(h^\prime_i\\) that of mixing \\(m^\prime_i.\\)

We'll assume that the first chunk is mixed with a
\\(2^{-w}\\)-almost-universal (\\(2^{-64}\\) for UMASH) hash function:
if \\(m_0 \neq m^\prime_0,\\) \\(\mathrm{P}[h_0 = h^\prime_0] \leq 2^{-w},\\)
(where the probability is taken over the set of randomly chosen
parameters for the mixer).
Otherwise, \\(m_0 = m^\prime_0 \Rightarrow h_i = h^\prime_i\\).

This first chunks stands for the ENH iteration in UMASH.

Every remaining chunk will instead be mixed with a
\\(2^{-w}\\)-XOR-almost-universal hash function:
if \\(m_i \neq m^\prime_i\\) (\\(0 < i < n\\)),
\\(\mathrm{P}[h_i \oplus h^\prime_i = y] \leq 2^{-w}\\)
for any \\(y,\\)
where the probability is taken over the randomly generated
parameter for the mixer.

This stronger condition represents the PH iterations in UMASH.

We hash a full block by xoring all the mixed chunks together:

\\[ H = \bigoplus_{i = 0}^{n - 1} h_i, \\]

and

\\[ H^\prime = \bigoplus_{i = 0}^{n - 1} h^\prime_i. \\]

We want to bound the probability that \\(H = H^\prime \Leftrightarrow
H \oplus H^\prime = 0,\\) assuming that the messages differ
(i.e., there is at least one index \\(i\\) such that
\\(m_i \neq m^\prime_i\\)).

If the two messages only differ in \\(m_0 \neq n^\prime_0\\) (and thus
\\(m_i = m^\prime_i,\,\forall i \in [1, n)\\)),

\\[ \bigoplus_{i = 1}^{n - 1} h_i = \bigoplus_{i = 1}^{n - 1} h^\prime_i, \\]

and thus \\(H = H^\prime \Leftrightarrow h_0 = h^\prime_0\\).

By hypothesis, the 0th chunks are mixed with a
\\(2^{-w}\\)-almost-universal hash, so this happens with probability
at most \\(2^{-w}\\).

Otherwise, assume that \\(m_j \neq m^\prime_j\\), for some \\(j \in
[1, n)\\).
We will rearrange the expression

\\[ H \oplus H^\prime = h_j \oplus h^\prime_j \oplus \left(\bigoplus_{i\in [0, n) \setminus \\{ j \\}} h_i \oplus h^\prime_i\right). \\]

Let's conservatively replace that unwieldly sum with an adversarially
chosen value \\(y\\):

\\[ H \oplus H^\prime = h_j \oplus h^\prime_j \oplus y, \\]

and thus \\(H = H^\prime\\) iff \\(h_j \oplus h^\prime_j = y.\\)
By hypothesis, the \\(j\\)th chunk (every chunk but the 0th),
is mixed with a \\(2^{-w}\\)-almost-XOR-universal hash, and
this thus happens with probability at most \\(2^{-w}\\).

In both cases, we find a collision probability at most \\(2^{-w}\\)
with a simple analysis, despite combining mixing functions from
different families over different rings.

Wringing more bits out of the same mixers
-----------------------------------------

We combined strong mixers (each is \\(2^{-w}\\)-almost-universal),
and only got a \\(2^{-w}\\)-almost-universal output.
It seems like we should be able to do better when two or
more chunks differ.

As [Nandi](https://link.springer.com/chapter/10.1007/978-3-662-46706-0_25)
points outs, we can apply erasure codes to derive additional
chunks from the original messages' contents.  We only need
one more chunk, so we can simply xor together all the original
chunks:

\\[m_n = \bigoplus_{i=0}^{n - 1} m_i,\\]

and similarly for \\(m^\prime_n\\).
If \\(m\\) and \\(m^\prime\\) differ in only one chunk, \\(m_n \neq
m^\prime_n\\).  It's definitely possible for \\(m_n = m^\prime_n\\)
when \\(m \neq m^\prime\\), but only if two or more chunks differ.

We will again mix \\(m_n\\) and \\(m^\prime_n\\) with a fresh
\\(2^{-w}\\)-almost-XOR-universal hash function to yield \\(h_n\\) and
\\(h^\prime_n\\).

We want to xor the result \\(h_n\\) and \\(h^\prime_n\\) with the second
(still undefined) hash values \\(H_2\\) and \\(H^\prime_2\\); if
\\(m_n \neq m^\prime_n\\), the final xored values are equal with
probability at most \\(2^{-w}\\), regardless of \\(H_2\\) and
\\(H^\prime_2\ldots\\) and, crucially, independently of \\(H \neq
H^\prime\\).

When the two messages \\(m\\) and \\(m^\prime\\) only differ in a
single (initial) chunk, mixing a [LRC checksum](https://en.wikipedia.org/wiki/Longitudinal_redundancy_check)
gives us an independent hash function, which
squares the collision probability to \\(2^{-2w}\\).

Now to the interesting bit: we must define a second hash function
that combines \\(h_0,h_1,\ldots, h_{n - 1}\\) and \\(h^\prime_0, h^\prime_1, \ldots, h^\prime_{n - 1}\\)
such that the resulting hash values \\(H_2\\) and \\(H^\prime_2\\) collide
independently enough of \\(H\\) and \\(H^\prime\\).
That's a tall order, but we do have one additional assumption to work
with: we only care about collisions in this second hash function if the
additional checksum chunks are equal, which means that the two messages
differ in two or more chunks (or they're identical).

For each index \\(0 < i < n\\), we'll fix a *public* linear (with xor
as the addition) function \\(\overline{xs}_i(x)\\).  This family of
function must have two properties:

1. \\(f(x) = x \oplus \overline{xs}_i(x)\\) is invertible for all \\(0 < i < n\\).
2. For any two distinct \\(0 < i < j < n\\), xoring the two functions
   together into \\(g(x) = \overline{xs}_i(x) \oplus
   \overline{xs}_j(x)\\) yields a function with a small (low rank)
   null space.  In other words, while \\(g\\) may not be invertible,
   it must be "pretty close."

For regularity, we will also define \\(\overline{xs}_0(x) = x\\).

Concretely, let \\(\overline{xs}_1(x) = x \mathtt{<<} 1\\), where the
bitshift is computed for the two 64-bit halves independently, and
\\(\overline{xs}_i(x) = (x  \mathtt{<<} 1) \oplus (x \mathtt{<<} i)\\)
for \\(i > 1\\), again with all the bitshifts computed independently
over the two 64-bit halves.

To see that these satisfy our requirements, we can represent the
functions as carryless multiplication by distinct "even" constants
(the least significant bit is 0) on each 64-bit half:

1. Once we xor in \\(x\\), we get a multiplication by an odd constant,
   and that's invertible.
2. Combining \\(\overline{xs}_i\\) and \\(\overline{xs}_j\\) with xor
   yields either \\(x \mathtt{<<} j\\) or \\((x \mathtt{<<} i) \oplus (x \mathtt{<<} j)\\).  In the worst case, we lose \\(j\\) bits in each 64-bit half,
   and there are thus \\(2^{2j}\\) values in \\(g^{-1}(0)\\).

To recapitulate, we defined the first hash function as

\\[ H = \bigoplus_{i = 0}^{n - 1} h_i, \\]

the (xor) sum of the mixed value \\(h_i\\) for each chunk \\(m_i\\) in
the message block \\(m\\), and similarly for \\(H^\prime\\) and
\\(h^\prime_i\\).

We'll let the second hash function be

\\[ H_2 \oplus h_n = \left(\bigoplus_{i = 0}^{n - 1} \overline{xs}_i(h_i)\right) \oplus h_n, \\]

and 

\\[ H^\prime_2 \oplus h^\prime_n = \left(\bigoplus_{i = 0}^{n - 1} \overline{xs}_i(h^\prime_i)\right) \oplus h^\prime_n. \\]


We can finally get down to business and find some collision bounds.
We've already shown that both \\(H = H^\prime\\) *and* \\(H_2 \oplus h_n =
H^\prime_2 \oplus h^\prime_n\\) collide simultaneously with probability at most \\(2^{-2w}\\)
when the checksum chunks differ, i.e., when \\(m_n \neq m^\prime_n\\).

Let's now focus on the case when \\(m \neq m^\prime\\), but \\(m_n =
m^\prime_n\\).
In that case, we know that at least two chunks \\(0 \leq i < j < n\\)
differ: \\(m_i \neq m^\prime_i\\) and \\(m_j \neq m^\prime_j\\).

If only two chunks \\(i\\) and \\(j\\) differ, and one of them is the
\\(i = 0\\)th chunk, we want to bound the probability that

\\[ h_0 \oplus h_j = h^\prime_0 \oplus h^\prime_j \\]

and

\\[ h_0 \oplus \overline{xs}_j(h_j) = h^\prime_0 \oplus \overline{xs}_j(h^\prime_j) = 0, \\]

both at the same time.

Letting \\(\Delta_i = h_i \oplus h^\prime_i\\), we can reformulate the
two conditions as

\\[ \Delta_0 = \Delta_j \\]
and
\\[ \Delta_0 = \overline{xs}_j(\Delta_j). \\]

Taking the xor of the two conditions yields

\\[ \Delta_j \oplus \overline{xs}_j(\Delta_j) = 0, \\]

which is only satisfied for \\(\Delta_j = 0\\), since \\(f(x) = x
\oplus \overline{xs}_j(x)\\) is an invertible linear function.
This also forces \\(\Delta_0 = 0\\).

By hypothesis, \\(\mathrm{P}[\Delta_j = 0] \leq 2^{-w}\\), and
\\(\mathrm{P}[\Delta_0 = 0] \leq 2^{-w}\\) as well.  These two
probabilities are independent, so we get a probability that both
hash collide less than or equal to \\(2^{-2w}\\) (\\(2^{-128}\\)).

In the other case, we have messages that differ in at least two chunks
\\(0 < i < j < n\\): \\(m_i \neq m^\prime_i\\) and \\(m_j \neq
m^\prime_j\\).

We can simplify the collision conditions to

\\[ h_i \oplus h_j = h^\prime_i \oplus h^\prime_j \oplus y \\]

and 

\\[ \overline{xs}_i(h_i) \oplus \overline{xs}_j(h_j) = \overline{xs}_i(h^\prime_i) \oplus \overline{xs}_j(h^\prime_j) \oplus z, \\]

for \\(y\\) and \\(z\\) generated arbitrarily (adversarially), but
without knowledge of the parameters that generated \\(h_i, h_j, h^\prime_i,
h^\prime_j\\).

Again, let \\(\Delta_i = h_i \oplus h^\prime_i\\) and \\(\Delta_j = h_j \oplus h^\prime_j\\), and reformulate the conditions into

\\[ \Delta_i \oplus \Delta_j = y \\]
and
\\[ \overline{xs}_i(\Delta_i) \oplus \overline{xs}_j(\Delta_j) = z. \\]

Let's apply the linear function \\(\overline{xs}_i\\) to the first
condition

\\[ \overline{xs}_i(\Delta_i) \oplus \overline{xs}_i(\Delta_j) = \overline{xs}_i(y); \\]

since \\(\overline{xs}_i\\) isn't invertible, the result
isn't equivalent, but is a weaker (necessary, not sufficient)
version of the initial condiion.

After xoring that with the second condition

\\[ \overline{xs}_i(\Delta_i) \oplus \overline{xs}_j(\Delta_j) = z, \\]

we find

\\[ \overline{xs}_i(\Delta_j) \oplus \overline{xs}_j(\Delta_j) = \overline{xs}_i(y) \oplus z. \\]

By hypothesis, the null space of
\\(g(x) = \overline{xs}_i(x) \oplus \overline{xs}_j(x)\\)
is "small."
For our concrete definition of \\(\overline{xs}\\), there
are \\(2^{2j}\\) values in that null space, which means that \\(\Delta_j\\)
can only satisfy the combined xored condition by taking one of at most
\\(2^{2j}\\) values; otherwise, the two hashes definitely can't both
collide.

Since \\(j < n\\), this happens with probability at most \\(2^{2(n -
1) - w} \leq 2^{-34}\\) since UMASH has \\(w = 64\\) and \\(n =
16\\).

Finally, for any given \\(\Delta_j\\), there is at most one
\\(\Delta_i\\) that satisfies

\\[ \Delta_i \oplus \Delta_j = y,\\]

and so *both* hashes collide with probability at most \\(2^{-98}\\),
with \\(w = 64\\) and \\(n = 16\\).

Astute readers will notice that we could let
\\(\overline{xs}_i(x) = x \mathtt{<<} i\\),
and find the same combined collision probability.
However, this results in a much weaker secondary hash, since a chunk
could lose up to \\(2n - 2\\) bits (\\(n - 1\\) in each 64-bit half)
of hash information in a plain shift.

The shifted xor-shifts might be a bit slower to compute, but
guarantees that we only lose at most 2 bits of information per chunk.
This feels like an interface that's harder to misuse.

If one were to change the \\(\overline{xs}_i\\) family of functions, I
think it would make more sense to look at a more diverse form of
(still sparse) multipliers, which would likely let us preserve a
couple more bits of independence.  Jim has constructed such a family
of multipliers, in arithmetic modulo \\(2^{64}\\); I'm sure we could
find something similar in carryless multiplication.  The hard part is
implementing these multipliers: in order to exploit the multipliers'
sparsity, we'd probably have to fully unroll the block hashing loop,
and that's not something I like to force on implementations.

What does this look like in code?
---------------------------------

The base [UMASH block compressor](https://github.com/backtrace-labs/umash/blob/8fd6287617f41e236bfb679e8d29a8b32f82c0e9/umash.c#L336)
mixes all but the last of the message block's 16-byte chunks with
PH: xor the chunk with the corresponding bytes in the parameter
array, computes a carryless multiplication of the xored chunks' half
with the other half.  The last chunk goes through a variant of
[ENH](https://github.com/backtrace-labs/umash/blob/8fd6287617f41e236bfb679e8d29a8b32f82c0e9/umash.c#L358)
with an invertible finaliser (safe because we only rely on
\\(\varepsilon\\)-almost-universality),
and everything is xored in the accumulator.

The collision proofs above preserved the same structure for the first hash.

The second hash reuses so much work from the first that it mostly
makes sense to consider a combined loop that computes both (regular
UMASH and this new xor-shifted variant) block compression functions at
the same time.

The first change for this combined loop is that we need to xor
together all 16-bytes chunk in the message, and mix the resulting
checksum with a fresh PH function.  That's equivalent to xoring
everything in a new accumulator (or two accumulators when working with
256-bit vectors) initialised with the PH parameters, and `CLMUL`ing
together the accumulator's two 64-bit halves at the end.

We also have to apply the \\(\overline{xs}_i\\) quasi-xor-shift
functions to each \\(h_i\\).  The trick is to accumulate the shifted
values in two variables: one is the regular UMASH accumulator without
\\(h_0\\) (i.e., \\(h_1 \oplus h_2 \ldots\\)), and the other shifts
the current accumulator before xoring in a new value, i.e.,
\\(\mathtt{acc}^\prime = (\mathtt{acc} \mathtt{<<} 1) \oplus h_i\\),
where the left shift on parallel 64-bit halves simply adds `acc`
to itself.

This additional shifted accumulator includes another special case to
skip \\(\overline{xs}_1(x) = x \mathtt{<<} 1\\); that's not a
big deal for the code, since we already have to special case the last
iteration for the ENH mixer.

Armed with \\(\mathtt{UMASH} = \bigoplus_{i=1}^{n - 1} h_i\\) and
\\(\mathtt{acc} = \bigoplus_{i=2}^{n - 1} h_i \mathtt{<<} (i - 1),\\)
we have
\\[\bigoplus_{i=1}^{n - 1} \overline{xs}_i(h_i) = (\mathtt{UMASH} \oplus \mathtt{acc}) \mathtt{<<} 1.\\]

We just have to xor in the `PH`-mixed checksum \\(h_n\\), and finally
\\(h_0\\) (which naturally goes in GPRs, so can be computed while we
extract values out of vector registers).

We added two vector xors and one addition for each chunk in a block,
and, at the end, one `CLMUL` plus a couple more xors and adds again.

This should most definitely be faster than computing two UMASH at the
same time, which incurred two vector xors and a `CLMUL` (or full
integer multiplication) for each chunk: even when `CLMUL` can pipeline
one instruction per cycle, vector additions can dispatch to more
execution units, so the combined throughput is still higher.

One last thing: what if we have blocks of different length?
-----------------------------------------------------------

It's easy to show that UMASH is relatively safe when one block is
shorter than the other, and we simply xor together fewer mixed chunks.
Without loss of generality, we can assume the longer block has \\(n\\)
chunks; that block's final ENH is independent of the shorter block's
UMASH, and any specific value occurs with probability at most
\\(2^{-63}\\) (the probability of a multiplication by zero).

A similar argument seems more complex to defend for the shifted UMASH.

Luckily, we can tweak the [LRC checksum](https://en.wikipedia.org/wiki/Longitudinal_redundancy_check)
we use to generate an additional chunk in the block: rather than xoring
together the raw message chunks, we'll xor them *after* xoring them
with the PH key, i.e.,

\\[m_n = \bigoplus_{i=0}^{n - 1} m_i \oplus k_i, \\]

where \\(k_i\\) are the PH parameters for each chunk.

When checksumming blocks of the same size, this is a no-op with respect
to collision probabilities.  Implementations might however benefit
from the ability to use a fused `xor` with load from memory[^latency]
to compute \\(m_i \oplus k_i\\), and feed that both into the checksum
and into `CLMUL` for PH.

[^latency]: This might also come with a small latency hit, which is unfortunate since PH-ing \\(m_n\\) is likely to be on the critical path... but one cycle doesn't seem that bad.

Unless we're extremely unlucky (\\(m_{n - 1} = k_{n - 1}\\), with
probability \\(2^{-2w}\\)), the long block's LRC will differ from the
shorter block's.  As long as we always xor in the same PH parameters
when mixing the artificial LRC, the secondary hashes collide with
probability at most \\(2^{-64}\\).

With a small tweak to the checksum function, we can easily guarantee
that blocks with a different number of chunks collide with probability
less than \\(2^{-126}\\).[^tag]

[^tag]: The algorithm to expand any input message to a sequence of full 16-byte chunks is fixed.  That's why we incorporate a size tag in ENH; that makes it impossible for two messages of different lengths to collide when they are otherwise identical after expansion.

<small>Thank you Joonas for helping me rubber duck the presentation, and Jim for pointing me in the right direction, and for the fruitful discussion!</small>

<p><hr style="width: 50%"></p>

