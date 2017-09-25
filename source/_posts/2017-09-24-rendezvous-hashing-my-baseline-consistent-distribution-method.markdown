---
layout: post
title: 'Rendezvous hashing: my baseline "consistent" distribution method'
date: 2017-09-24 22:41:00 -0400
comments: true
categories: 
---

Whenever I mention a data or work distribution problem where I
ideally want everything related to a given key to hit the same
machine, everyone jumps to [consistent hashing](http://courses.cse.tamu.edu/caverlee/csce438/readings/consistent-hashing.pdf).  I don't know how this technique achieved the mindshare it has,
although I suspect [Amazon's 2007 Dynamo DB paper](http://www.read.seas.harvard.edu/~kohler/class/cs239-w08/decandia07dynamo.pdf)
is to blame (by introducing the problem to many of us, and mentioning
exactly one decentralised solution)… or maybe some Google interview
prep package.

<small>Karger et al's paper doesn't help, since they introduce the
generic concept of a consistent hash function and call their
specific solution… "consistent hashing."  I'm not sure where I first
encountered rendezvous hashing, but I vaguely remember a technical
report by Karger,  so it's probably not some MIT vs UMich thing.</small>

Regardless of the reason for consistent hashing's popularity, I feel
the go-to technique should instead be [rendezvous hashing](http://www.eecs.umich.edu/techreports/cse/96/CSE-TR-316-96.pdf).  Its basic
form is simple enough to remember without really trying (one of those [desert island](https://www.pvk.ca/Blog/2015/04/26/pointer-less-scapegoat-trees/)
algorithms), it is more memory efficient than consistent hashing in
practice, and its downside--a simple implementation assigns a location
in time linear in the number of hosts--is not a problem for small
deployments, or even medium (a couple racks) scale ones if you
actually think about failure domains.

<small>Side question: why did rendez-vous have to lose its hyphen to cross the Channel?</small>

Basic rendezvous hashing takes a distribution key (e.g., a filename),
and a set of destinations (e.g., hostnames).  It then uses a hash function
to pseudorandomly map each `(distribution_key, destination)` pair to a
value in `[0, 1)` or `[0, 2^64 - 1)`, and picks the destination that
gives the minimal hash value.  If it needs `k` destinations for
redundancy, it can pick the destinations that yield the least `k` hash
values.  If there are ties (unlikely with a good hash function), it
breaks them arbitrarily but consistently, e.g., by imposing a total
order on hostnames.

A Python implementation could look like the following.

{% codeblock basic rendezvous hashing lang:py %}
Destination = namedtuple('Destination', ['host', 'hash'])


def merge_hashes(x, y):
    return ((x * 3) ^ y) % 2**64


def pick_destinations(key, destinations, k=1):
    key_hash = hash_key(key)  # hash the key once, instead of hash(key + host)
    annotated = [(merge_hashes(key_hash, dest.hash), dest.host)
                 for dest in destinations]
    ordered = sorted(annotated)  # lexicographic sort on merged hash, host.
    return [host for _, host in ordered[:k]]  # grab host from the first k
{% endcodeblock %}

We only need to store the list of destinations, and we can convince
ourselves that data distribution is pretty good (close to uniform) and
that small changes in the set of destinations only affects a small
fraction of keys (those going to destinations added/removed), either
with pen and paper or with a few simulations.  That compares
positively with consistent hashing, where a practical implementation
has to create a lot (sometimes hundreds) of pseudo-nodes for each real
destination in order to mitigate clumping in the hash ring.

The downside is that we must iterate over all the nodes, while
consistent hashing is easily \\(\mathcal{O}(\log n)\\) time, or even
\\(\mathcal{O}(\log \log n)\\), with respect to the number of (pseudo-)nodes.
However, that's only a problem if you have a lot of nodes, and
rendezvous hashing, unlike consistent hashing, does not inflate the
number of nodes.

Another thing I like about rendezvous hashing is that it naturally
handles weights.  With consistent hashing, if I want a node to receive
ten times as much load as another, I create ten times more
pseudo-nodes.  As the greatest common divisor of weights
shrinks, the number of pseudo-node per node grows, which makes
distribution a bit slower, and, more importantly, increases memory
usage (linear in the number of pseudo-nodes).  Worse, if you hit the
[fundamental theorem of arithmetic](https://en.wikipedia.org/wiki/Fundamental_theorem_of_arithmetic)
(as a coworker once snarked out in a commit message), you may have
to rescale *everything*, potentially causing massive data movement.

Rendezvous hashing generates pseudorandom scores by hashing, and ranks
them to find the right node(s).  Intuitively, we want to use weights
so that the distribution of pseudorandom scores generated for a node A
with twice the weight as another node B has the same shape as that of
node B, but is linearly stretched so that the average hash value for A is
twice that for B.  We also want the distribution to cover `[0, infty)`, 
otherwise a proportion of hashes will always go to the heavier node,
regardless of what the lighter node hashes to, and that seems wrong.

The [trick](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.414.9353&rep=rep1&type=pdf),
as explained by [Jason Resch](http://www.snia.org/sites/default/files/SDC15_presentations/dist_sys/Jason_Resch_New_Consistent_Hashings_Rev.pdf)
at Cleversafe, is to map our hashes from uniform in `[0, 1)` to
`[0, infty)` not as an exponential, but with `-weight / log(h)`.  If you
simulate just using an exponential, you can quickly observe that it
doesn't reweigh things correctly: while the mean is correctly scaled,
the mass of the probability density function isn't shifted quite right.
Resch's proof of correctness for this tweaked exponential fits on a
[single page](https://twitter.com/pkhuong/status/799759031896309760).

The Python code becomes something like:

{% codeblock weighted rendezvous hashing lang:py %}
Destination = namedtuple('Destination', ['host', 'hash', 'weight'])


def score(hash_value, weight):
    return -weight / math.log(hash_value / HASH_MAX)


def pick_destinations(key, destinations, k=1):
    key_hash = hash_key(key)
    annotated = [(score(merge_hashes(key_hash, dest.hash), dest.weight), dest.host)
                 for dest in destinations]
    ordered = sorted(annotated)
    return [host for _, host in ordered[:k]]
{% endcodeblock %}

There are obvious micro-optimisations here (for example, computing
the inverse of the score lets us precompute the reciprocal of each
destination's weight), but that's all details.  The salient part to me
is that space and time is still linear in the number of nodes,
regardless of the weights; consistent hashing instead needs space
pseudolinear(!) in the weights, and is thus a bit slower than its
\\(\mathcal{O}(\log n)\\) runtime would have us believe.

The linear-time computation for weighted rendezvous hashing is also
CPU friendly.  The memory accesses are all linear and easily
prefetchable (load all metadata from an array of nodes), and the
computational kernel is standard vectorisable floating point
arithmetic.

In practice, I'm also not sure I ever really want to distribute
between hundreds of machines: what kind of failure/resource allocation
domain encompasses that many equivalent nodes?  For example, when
distributing data, I would likely want a hierarchical
consistent distribution scheme, like [Ceph's CRUSH](https://www.crss.ucsc.edu/media/papers/weil-sc06.pdf):
something that first assigns data to sections of a datacenter, then to
racks, and only then to individual machines.  I should never blindly
distribute data across hundreds of machines; I need to distribute
between a handful of sections of the network, then one of a dozen
racks, and finally to one of twenty machines.  The difference between
linear and logarithmic time at each level of this "failure trie" is
marginal and is easily compensated by a bit of programming.

The simplicity of basic rendezvous hashing, combined with its minimal
space usage and the existence of a weighted extension, makes me
believe it's a better initial/default implementation of consistent
hash functions than consistent hashing.  Moreover, consistent
hashing's main advantage, sublinear-time distribution, isn't
necessarily compelling when you think about the whole datacenter (or
even many datacenters) as a resilient system of failure-prone domains.
Maybe rendezvous hashing deserves a rebranding campaign (:
