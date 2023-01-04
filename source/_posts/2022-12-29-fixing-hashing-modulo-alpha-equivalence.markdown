---
layout: post
title: "Fixing the hashing in \"Hashing modulo α-equivalence\""
date: 2022-12-29 15:12:06 -0500
comments: true
categories: 
---

[Per Vognsen](https://mastodon.social/@pervognsen) sent me a link to [Maziarz _et al's_ Hashing Modulo Alpha-Equivalence](https://simon.peytonjones.org/assets/pdfs/hashing-modulo-alpha.pdf)
because its Lemma 6.6 claims to solve a thorny problem we have both
encountered several times.

Essentially, the lemma says that computing the natural recursive
combination of hash values over \\(2^b\\) bits for two distinct trees
(ADT instances) \\(a\\) and \\(b\\) yields a collision probability at most
\\(\frac{\|a\| + \|b\|}{2^b}\\) if we use a random hash function (sure),
and Section 6.2 claims *without proof* that the result can be safely
extended to the unspecified "seeded" hash function they use.

That's a minor result, and the paper's most interesting contribution
(to me) is an algorithmically efficient alternative to 
[the locally nameless representation](https://chargueraud.org/research/2009/ln/main.pdf): 
rather than representing bindings with simple binders and complex
references, as in de Bruijn indices (lambda is literally just a
`lambda` literal, but references must count how many lambdas to
go up in order to find the correct bindings), Maziarz and his
coauthors use simple references (holes, all identical), and complex
binders (each lambda tracks the set of paths from the lambda binding
to the relevant holes).

The rest all flows naturally from this powerful idea.

Part of the naturally flowing rest are collision probability
analyses for a few hashing-based data structures.  Of course it's not what
[PLDI](https://en.wikipedia.org/wiki/Programming_Language_Design_and_Implementation) is about, but that aspect of the paper makes it look like the
authors are unaware of analysis and design tools for hashing based
algorithms introduced in the 1970s (a quick Ctrl-F for "universal,"
"Wegman," or "Carter" yields nothing).  That probably explains the
reckless generalisation from truly random hash functions to
practically realisable ones.

There are two core responsibilities for the hashing logic:

1. incrementally hash trees bottom up (leaf to root)
2. maintain the hash for a map of variable name to (hash of) trees (that may grow bottom-up as well)

As Per saliently put it, there are two options for formal analysis
of collision probabilities here:
we can either assume a cryptographic hash function like [SHA-3](https://en.wikipedia.org/wiki/SHA-3) or [BLAKE3](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE3), in which case *any collision* is world-breaking news,
so all that matters is serialising data unambiguously when feeding bytes to the hash function,
or we can work in the [universal hashing framework](https://www.cs.princeton.edu/courses/archive/fall09/cos521/Handouts/universalclasses.pdf).

Collision probability analysis for the former is trivial, so let's assume we want the latter, pinpoint where the paper is overly optimistic, and figure out how to fix it.

Incremental bottom-up hashing, without novelty
----------------------------------------------

Let's tackle the first responsibility: incrementally hashing
trees bottom up.

The paper essentially says the following in [Appendix A](https://simon.peytonjones.org/assets/pdfs/hashing-modulo-alpha.pdf#page=15).
Assume we have one *truly random variable-arity* hash function ("hash combiner") \\(f\\), and a tag for each constructor (e.g., \\(s_{\texttt{Plus}}\\) for `(Plus a b)`);
we can simply feed the constructor's arity, its tag, and the subtrees' hash values to \\(f\\), e.g., \\(f(2, s_{\texttt{Plus}}, hv_a, hv_b)\\)...
and goes on to show a surprisingly weak collision bound
(the collision rate for two distinct trees grows with the
*sum* of the size of both trees).[^union-bound]

[^union-bound]: Perhaps not that surprising given the straightforward union bound.

A non-intuitive fact in hash-based algorithms is that results for truly
random hash functions often fail to generalise for the weaker "salted"
hash functions we can implement in practice.  For example, 
[linear probing hash tables need *5*](https://arxiv.org/abs/cs/0612055)-[universal hash functions](https://en.wikipedia.org/wiki/Universal_hashing)[^tabular] in
order to match the performance we expect from a naïve analysis with
truly random hash functions.  A [5-universal family of hash functions](https://www.cs.princeton.edu/courses/archive/fall09/cos521/Handouts/universalclasses.pdf)
isn't the kind of thing we use or come up with by accident (such
families are parameterised by at least 5 words for word-sized outputs, and that's a lot of salt).

[^tabular]: [Twisted tabular hashing](https://dl.acm.org/doi/10.5555/2627817.2627833) also works despite not being quite 5-universal, and is already at the edge of practicality.

The paper's assumption that the collision bound it gets for a truly
random function \\(h\\) holds for practical salted/seeded hash
functions is thus unwarranted (see, for examples, 
[these counter examples for linear probing](https://www.cs.utexas.edu/~yzhang/papers/hash-alenex10.pdf), or [the seed-independent collisions that motivated the development of SipHash](https://en.wikipedia.org/wiki/SipHash));
strong cryptographic hash functions could work (find a collision, break Bitcoin),
but we otherwise need a more careful analysis.

It so happens that we can easily improve on the collision bound with a
classic [incremental hashing](https://en.wikipedia.org/wiki/Rolling_hash) approach: [polynomial string hashing](https://arxiv.org/abs/1008.1715).

Polynomial string hash functions are computed over a fixed finite
field \\(\mathbb{F}\\) (e.g., arithmetic modulo a prime number
\\(p\\)), and parameterised by a single point \\(x \in \mathbb{F}\\).

Assuming a string of "characters" \\(v_i \in \mathbb{F}\\) (e.g., we could
hash strings of atomic bytes in arithmetic modulo a prime \\(p \geq 256\\)
by mapping each byte to the corresponding binary-encoded integer), the
hash value is simply

\\[v_0 + v_1 x + v_2 x^2 \ldots + v_{n - 1} x^{n - 1},\\]

evaluated in the field \\(\mathbb{F}\\), e.g., \\(\mathbb{Z}/p\mathbb{Z}\\).

For more structured atomic (leaf) values, we can serialise to bits and
make sure the field is large enough, or split longer bit serialised
values into multiple characters.  And of course, we can linearise trees
to strings by encoding them in binary S-expressions, with dedicated
characters for open `(` and close `)` parentheses.[^rpn]

[^rpn]: It's often easier to update a hash value when appending a string, so reverse Polish notation could be a bit more efficient.

The only remaining problem is to commute hashing and string
concatenation: given two subtrees `a`, `b`, we want to compute the
hash value of `(Plus a b)`, i.e., hash `"(Plus " + a + " " + b + ")"`
in constant time, given something of constant size, like hash values
for `a` and `b`.

Polynomials offer a lot of algebraic structure, so it shouldn't be
a surprise that there exists a solution.

In addition to computing `h(a)`, i.e., \\(\sum_{i=1}^{\|a\|} a_i x^i,\\)
we will remember \\(x^{\|a\|}\\), i.e., the product of `x` repeated for each
"character" we fed to the hash function while hashing the subtree `a`.
We can obviously compute that power in time linear in the size of `a`,
although in practice we might prefer to first compute that size, and later
exponentiate in logarithmic time with [repeated squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring).

Equipped with this additional power of \\(x\in\mathbb{F}\\), we can now compute the hash for the concatenation of two strings \\(h(a \mathtt{++} b)\\)
in constant time, given the hash and power of `x` for the constituent strings \\(a\\) and \\(b\\).

Expanding \\(h(a \mathtt{++} b)\\) and letting \\(m = \|a\|, \\) \\(n = \|b\| \\) yields:

\\[a_0 + a_1 x + \ldots + a_{m - 1} x^{m - 1} + b_0 x^n + b_1 x^{n + 1} + \ldots + b_{n - 1} x^{m + n - 1},\\]

which we can rearrange as

\\[a_0 + a_1 x + \ldots + a_{m - 1} x^{m - 1} + x^m (b_0 + b_1 x + \ldots b_{n-1} x^{n-1},\\]

i.e.,

\\[h(a \mathtt{++} b) = h(a) + x^{\|a\|} h(b),\\]


and we already have all right-hand side three terms \\(h(a),\\) \\(x^{\|a\|},\\) and \\(h(b).\\)

Similarly, \\(x^{\|a \mathtt{++} b\|} = x^{\|a\| + \|b\|} = x^a \cdot x^b,\\)
computable in constant time as well.

This gives us an explicit representation for the hash summary of each
substring, so it's easy to handle, e.g., commutative and associative
operators by sorting the pairs of \\((h(\cdot), x^\{|\cdot\|})\\) that
correspond to each argument before hashing their concatenation.

TL;DR: a small extension of classic polynomial string hashing commutes
efficiently with string concatenation.

And the collision rate?  We compute the same [polynomial string hash](https://arxiv.org/abs/1008.1715),
so two distinct strings of length at most \\(n\\) collide with
probability at most \\(n/\|\mathbb{F}\|\\) (with the expectation over
the generation of the random point \\(x \in \mathbb{F}\\);[^sketch] never worse than Lemma 6.6 of Maziarz _et al_, and up to twice as good.

[^sketch]: Two distincts inputs `a` and `b` define polynomials \\(p_a\\) and `\\(p_b\\) of respective degree \\(\|a\|\\) and \\(\|b\|\\).  They only collide for a seed \\(x\in\mathbb{F}\\) when \\(p_a(x) = p_b(x),\\) i.e., \\(p_a(x) - p_b(x) = 0\\).  This difference is a non-zero polynomial of degree at most \\(\max(\|a\|, \|b\|),\\) so at most that many of the \\(\|\mathbb{F}\|\\) potential values for \\(x\\) will lead to a collision.

Practical implementations of polynomial string hashing tend to
evaluate the polynomial with Horner's method rather than maintaining
\\(x^i\\).  The result computes a different hash function, since it reverses
the order of the terms in the polynomial, but that's irrelevant for
collision analysis.  The concatenation trick is similarly little affected:
we now want \\(h(a \mathtt{++} b) = x^{\|b\|} h(a) + h(b)\\).

Hashing unordered maps and sets 
-------------------------------

The term representation introduced in "Hashing Module
Alpha-Equivalence" contains a map from variable name to a tree
representation of the holes where the variable goes (like a DAWG
representation for a set of words where each word is a path, except
the paths only share as they get closer to the root of the tree...  so
maybe more like `snoc` lists with sharing).

We already know how to hash trees incrementally; the new challenge
is in maintaining the hash value for a map.

Typically, one hashes unordered sets or maps by storing them in
balanced trees sorted primarily on the key's hash value, and secondarily on the key.[^rhh] We can also easily tweak arbitrary
balanced trees to maintain the tree's hash value as we add or remove
entries: [augment each node](https://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf#page=9)
with the hash and power of `x` for the serialised representation of
subtree rooted at the node.[^crypto]

[^rhh]: A more efficient option in practice, if maybe idiosyncratic, is to use Robin Hood hashing with linear probing to maintain the key-value pairs sorted by `hash(key)` (and breaking improbable ties by comparing the keys themselves), but that doesn't lend itself well to incremental hash maintenance.

[^crypto]: Cryptographically-minded readers might find [Incremental Multiset Hashes and their Application to Integrity Checking](http://csg.csail.mit.edu/pubs/memos/Memo-464/memo-464.pdf) interesting.

The paper instead takes the treacherously attractive approach of
hashing individual key-value pairs, and combining them with an abelian group
operator (commutative and associative, and where each element has an
inverse)... in their case, bitwise `xor` over fixed-size words.

Of course, for truly random hash functions, this works well enough,
and the proof is simple.  Unfortunately, just because a practical
hash function is well distributed for individial value does not mean
pairs or triplets of values  won't show any "clumping" or pattern.
That's what [\\(k-\\)universality is all about](https://en.wikipedia.org/wiki/K-independent_hashing).

For key-value pairs, we can do something simple: associate one hash
function from a (almost-`xor`)-universal family to each value, and
use it to mix the associated value before `xor`ing everything together.

It's not always practical to associate one hash function with each
key, but it does work for the data structure introduced in "Hashing
modulo Alpha-Equivalence:" the keys are variable names, and these
were regenerated arbitrarily to ensure uniqueness in a prior linear
traversal of the expression tree.  The "variable names" could thus
include (or *be*) randomly generated parameters for a
(almost-`xor`)-universal family.

[Multiply-shift is universal](https://arxiv.org/pdf/1504.06804.pdf#page=12),
so that would work; other approaches [modulo a Mersenne prime should also be safe to `xor`](https://thomasahle.com/papers/mersenne.pdf).

For compilers where hashing speed is more important than compact
hash values, almost-universal families could make sense.

The [simplest almost-`xor`-universal family of hash functions on contemporary hardware is probably `PH`](http://cr.yp.to/antiforgery/pema-20071022.pdf#page=6), a 1-universal family that maps
a pair of words \\((x_1, x_2)\\) to a pair of output words, and is
parameterised on a pair of words \\((a_1, a_2)\\):

\\[\texttt{PH}_a(x) = (x_1 \oplus a_1) \odot (x_2 \oplus a_2),\\]

where \\(\oplus\\) is the bitwise `xor`, and \\(\odot\\) an unreduced
carryless multiplication (e.g., x86 `CLMUL`).

Each instance of `PH` accepts a pair of \\(w-\\)bit words and returns
a \\(2w-\\)bit result; that's not really a useful hash function.

However, not only does `PH` guarantee a somewhat disappointing
collision rate at most \\(w^{-1}\\) for distinct inputs (expectation
taken over the \\(2w-\\)bit parameter \\((a_1, a_2)\\)), but, 
crucially, the results from any number of independently parameterised
`PH` can be combined with `xor` and maintain that collision rate!

For compilers that may not want to rely on cryptographic extensions,
[the `NH` family also works](https://fastcrypto.org/umac/umac_thesis.pdf#page=41), with \\(\oplus\\) mapping to addition modulo
\\(2^w\\), and \\(\odot\\) to full multiplication of two \\(w-\\)bit
multiplicands into a single \\(2w-\\)bit product.  The products have the
similar property of colliding with probability \\(w^{-1}\\) even once
combined with addition modulo \\(w^2\\).

Regardless of the hash function, it's cute. Useful? Maybe not, when
we could use purely functional balanced trees, and time complexity is
already in linearithmic land.

Unknown unknowns and walking across the campus
----------------------------------------------

None of this takes away from the paper, which I found both interesting
and useful (I intend to soon apply its insights), and it's all fixable
with a minimal amount of elbow grease... but the paper does make
claims it can't back, and that's unfortunate when reaching out to
people working on hash-based data structures would have easily
prevented the issues.

I find cross-disciplinary collaboration most effective for problems
we're not even aware of, unknown unknowns for some, unknown knowns for
the others.  Corollary: we should *especially* ask experts for
pointers and quick gut checks when we think it's all trivial because
*we* don't see anything to worry about.

<small>Thank you Per for linking to [Maziarz _et al's_ nice paper](https://simon.peytonjones.org/assets/pdfs/hashing-modulo-alpha.pdf) and for quick feedback as I iterated on this post.</small>

<p><hr style="width: 50%"></p>
