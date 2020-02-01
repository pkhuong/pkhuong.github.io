---
layout: post
title: "Lazy Linear Knapsack"
date: 2020-01-20 23:10:19 -0500
comments: true
categories: 
---

The [continuous knapsack problem](https://en.wikipedia.org/wiki/Continuous_knapsack_problem) may be the simplest non-trivial linear
programming problem:

\\[\max_{x \in [0, 1]^n} p'x\\]
subject to
\\[w'x \leq b.\\]

It has a linear objective, one constraint, and each decision variable
is bounded to ensure the optimum exists.  Note the key difference from
the [binary knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem#0/1_knapsack_problem): decision variables are allowed to take any
value between 0 and 1.  In other words, we can, e.g., stick half of
a profitable but large item in the knapsack. That's why this knapsack
problem can be solved in linear time.

Dual to primal is reasonable
----------------------------

Duality also lets us determine the shape of all optimal solutions to
this problem.  For each item \\(i\\) with weight \\(w_i\\) and profit
\\(p_i\\), let its profit ratio be \\(r_i = p_i / w_i,\\)
and let \\(\lambda^\star\\) be the optimal dual (Lagrange or linear)
multiplier associated with the capacity constraint \\(w'x \leq b.\\)
If \\\(\lambda^\star = 0,\\) we simply take all items with a positive
profit ratio (\\(r_i > 0\\)) and a non-negative weight \\(w_i \geq 0.\\)
Otherwise, every item with a profit ratio \\(r_i > \lambda^\star\\)
will be at its weight upper bound (1 if \\(w_i \geq 0\\), 0
otherwise), and items with \\(r_i < \lambda^\star\\) will instead be
at their lower bound (0 of \\(w_i \leq 0\\), and 1 otherwise).

Critical items, items with \\(r_i = \lambda^\star,\\) will take any value
that results in \\(w'x = b.\\) Given \\(\lambda^\star,\\) we can
derive the sum of weights for non-critical items; divide the
remaining capacity for critical items by the total weight of critical
items, and let that be the value for every critical item (with the
appropriate sign for the weight).

For example, if we have capacity \\(b = 10,\\) and the sum of weights
for non-critical items in the knsapsack is \\(8,\\) we're left with another
two units of capacity to distribute however we want among
critical items (they all have the same profit ratio \\(r_i =
\lambda^\star,\\) so it doesn't matter where that capacity goes).  Say
critical items with a positive weight have a collective weight of 4;
we could then assign a value of \\(2 / 4 = 0.5\\) to the corresponding
decision variable (and 0 for critical items with a non-positive
weight).

We could instead have \\(b = 10,\\) and the sum of weights for
non-critical items in the knapsack \\(12\\): we must find two units of
capacity among critical items (they all cost \\(r_i = \lambda^\star\\)
per unit, so it doesn't matter which).  If critical items with a
negative weight have a collective weight of \\(-3,\\) we could assign
a value of \\(-2 / -3 = 0.6\overline{6}\\) to the corresponding decision
variables, and 0 for critical items with a non-negative weight.

The last case highlights something important about the knapsack: in
general, we can't assume that the weights *or profits* are positive.
We could have an item with a non-positive weight and non-negative
profit (that's always worth taking), an item with positive weight and
negative profit (never interesting), or weights and profits of the
same sign.  The last case is the only one that calls for actual
decision making.  Classically, items with negative weight and profit
are rewritten away, by assuming they're taken in the knapsack, and
replacing them with a decision variable for the complementary decision
of removing that item from the knapsack (i.e., removing the additional
capacity in order to improve the profit).  I'll try to treat them
directly as much as possible, because that reduction can be a
significant fraction of solve times in practice.

The characterisation of optimal solutions above makes it easy to
directly handle elements with a negative weight: just find the optimal
multiplier, compute the contribution of non-critical elements (with
decision variables at a bound) to the left-hand side of the capacity
constraint, separately sums the negative and positive weights for
critical elements, then do a final pass to distribute the remaining
capacity to critical elements (and 0-weight / 0-value elements if one
wishes).

Solving the dual looks like selection
-------------------------------------

Finding the optimal multiplier \\(\lambda^\star\\) is similar to a
selection problem: the value is either 0 (the capacity constraint is
redundant), or one of the profit ratios \\(r_i,\\) and, given a
multiplier value \\(\lambda,\\) we can determine if it's too high or
too low in linear time.  If the non-critical elements yield a left-hand
side such that critical elements
can't add enough capacity (i.e., no solution with the optimal form can
be feasible), \\(\lambda\\) is too low.  If the maximum weight of
potentially optimal solutions is too low, \\(\lambda\\) is too high.

We can thus sort the items by profit ratio \\(r_i\\), compute the
total weight corresponding to each ratio with a prefix sum (with a
pre-pass to sum all negative weights), and perform a linear (or
binary) search to find the critical profit ratio.
Moreover, the status of non-critical items is monotonic as
\\(\lambda\\) grows: if an item with positive weight is taken at
\\(\lambda_0\\), it is also taken for every \\(\lambda \leq
\lambda_0\\), and a negative-weight item that's taken at
\\(\lambda_0\\) is also taken for every \\(\lambda \geq \lambda_0.\\)
This means we can adapt selection algorithms like [Quickselect](https://en.wikipedia.org/wiki/Quickselect) to solve
the continuous knapsack problem in linear time.

I'm looking at large instances, so I would like to run these
algorithms in parallel or even distributed on multiple machines, and
ideally use GPUs or SIMD extensions.  Unfortunately, selection doesn't
parallelise very well: we can run a distributed quickselect where
every processor partitions the data in its local RAM, but that still
requires a logarithmic number of iterations.

Selection looks like quantile estimation; does the dual?
--------------------------------------------------------

[Lazy Select](https://cs.stackexchange.com/questions/27685/can-someone-explain-lazyselect) offers a completely different angle for the selection
problem.  Selecting the \\(k\\)th smallest element from a list of
\\(n\\) elements is the same as finding the \\(k / n\\)th quantile[^abuse-of-language] in
that list of \\(n\\) elements.  We can use [concentration bounds](https://en.wikipedia.org/wiki/Hoeffding%27s_inequality)[^binomial] to estimate quantiles from a sample of, e.g.,
\\(m = n^{3/4}\\) elements: the population quantile value is very
probably between the \\(qm - \frac{\log m}{\sqrt{m}}\\)th and \\(qm +
\frac{\log m}{\sqrt{m}}\\)th values of the sample.  Moreover, this
range very probably includes at most \\(\mathcal{O}(n^{3/4})\\)
elements[^same-bounds], so a second pass suffices to buffer all the
elements around the quantile, and find the exact quantile.  Even with
a much smaller sample size \\(m = \sqrt{n},\\) we would only need four
passes.

[^abuse-of-language]: It's more like a fractional percentile, but you know what I mean: the value such that the distribution function at that point equals \\(k / n\\).

[^binomial]: Binomial bounds offer even stronger confidence intervals when the estimate is close to 0 or 1 (where Hoeffding's bound would yield a confidence interval that juts outside \\([0, 1]\\)), but don't impact worst-case performance.

[^same-bounds]: Thanks to Hoeffding's inequality, again.

Unfortunately, we can't directly use that correspondence between
selection and quantile estimation for the continuous knapsack.

I tried to apply a similar idea by sampling the knapsack elements
equiprobably, and extrapolating from a solution to the sample.  For
every \\(\lambda,\\) we can derive a selection function 
\\(f_\lambda (i) = I[r_i \geq \lambda]w_i\\) 
(invert the condition if the weight is negative),
and scale up \\(\sum_i f(i)\\) from the sample to the population).
As long as we sample independently of \\(f\\), we can reuse the
same sample for all \\(f_\lambda.\\)
The difficulty here is that, while the error for Lazy Select
scales as a function of \\(n,\\) the equivalent bounds with
variable weights are a function of \\(n(\|\max_i w_i\| + \|\min_i w_i\|)^2.\\)
That doesn't seem necessarily practical; scaling with \\(\sum_i \|w_i\|\\)
would be more reasonable.

Good news: we can hit that, thanks to linearity.

Let's assume weights are all integers.  Any item with weight \\(w_i\\)
is equivalent to \\(w_i\\) subitems with unit weight (or \\(-w_i\\)
elements with negative unit weight), and the same profit ratio \\(r_i\\),
i.e., profit \\(p_i / \|w_i\|\\).  The range of *subitem* weights is now a constant.

We could sample uniformly from the subitems with a [Bernoulli](https://en.wikipedia.org/wiki/Bernoulli_distribution) for each
subitem, but that's clearly linear time in the sum of weights, rather
than the number of elements.  If we wish to sample roughly \\(m\\) elements
from a total weight \\(W = \sum_i \|w_i\|,\\) we can instead determine how
many subitems (units of weight) to skip before sampling with a
[Geometric](https://en.wikipedia.org/wiki/Geometric_distribution) of
success probability \\(m / W.\\) This shows us how to lift the
integrality constraint on weights: sample from an [Exponential](https://en.wikipedia.org/wiki/Exponential_distribution)
with the same parameter \\(m / W!\\)

That helps, but we could still end up spending much more than constant
time on very heavy elements.  The trick is to deterministically
special-case these elements: stash any element with large weight
\\(w_i \geq W / m\\) to the side, exactly once.  By [Markov's inequality](https://en.wikipedia.org/wiki/Markov%27s_inequality),[^pigeonhole]
we know there aren't too many heavy elements: at most \\(m.\\)

[^pigeonhole]: That's a troll. I think any self-respecting computer person would rather see it as a sort of pigeonhole argument.

Let's test this out
-------------------

The heart of the estimation problem can be formalised as follows:
given a list of elements \\(i \in [n]\\) with weight \\(w_i \geq 0\\),
generate a sample of \\(m \leq n\\) elements ahead of time. After the
sample has been generated, we want to accept an arbitrary predicate
\\(p \in \\{0,1\\}^n\\) and estimate \\(\sum_{i\in [n]} p(i) w_i.\\)

We just had a sketch of an algorithm for this problem.  Let's see
what it looks like in Python.  The initial sample logic has to
determine the total weight, and sample items with probability
proportional to their weight.  Items heavier than the cutoff are not
considered in the sample and instead saved to an auxiliary list.

{% codeblock sample.py %}
def total_weight(items):
    return sum(weight for (_, weight) in items)


def sample_by_weight(items, rate, cutoff):
    """Samples from a list of (index, weight), with weight >= 0.

    Items with weight >= cutoff are taken with probability one.
    Others are sampled with rate `rate` / unit of weight.
    """
    sample = []
    large = []
    next_sample = random.expovariate(rate)
    for item in items:
        index, weight = item
        if weight >= cutoff:
            large.append(item)
        else:
            next_sample -= weight
            while next_sample <= 0:
                sample.append(index)
                next_sample += random.expovariate(rate)
    return sample, large
{% endcodeblock %}

We can assemble the resulting sample (and list of "large" elements) to
compute a lower bound on the weight of items that satisfy any
predicate that's independent of the sampling decisions.  The value for
large elements is trivial: we have a list of all large elements.
We can subtract the weight of all large elements from the total item
weight, and determine how much we have to extrapolate up.

{% codeblock extrapolate.py %}
def hoeffding(n, alpha):
    """Determines how much we can expect a sample of n i.i.d. values
    sampled from a Bernouli to differ, given an error rate of alpha.

    Given a sample X of n i.i.d. values from a Bernoulli distribution,
    let delta be \bar{X} - E[\bar{X}], the one-sided difference
    between the sample average value and the expected sample average.

    Hoeffding's upper bound (see below) is conservative when the
    empirical probability is close to 0 or 1 (trivially, it can yield
    confidence bounds that are outside [0, 1]!), but simple, and in
    general not much worse than tighter confidence interval.

    P(delta >= eps) <= exp(-2 eps^2 n) = alpha
      -> -2 eps^2 n = ln alpha
     <->        eps = sqrt[-(ln alpha) / 2n ]

    """
    return math.sqrt(- math.log(alpha) / (2 * n))


def eval_weight(total_weight, sample, large, predicate, alpha):
    """Given a population's total weight, a memoryless sample (by weight)
    from the population's items, and large items that were
    deterministically picked, evaluates a lower bound for the sum of
    weights for items in the population that satisfy predicate.
    
    The lower bound is taken with error rate <= alpha.
    """
    large_sum = sum(weight for (index, weight) in large if predicate(index))
    # The remainder was up for sampling, unit of weight at a time.
    sampled_weight = total_weight - sum(weight for (_, weight) in large)
    if sampled_weight <= 0 or not sample:
        return large_sum
    # Estimate the Binomial success rate with a Beta
    successes = sum(1 if predicate(x) else 0 for x in sample)
    failures = len(sample) - successes
    # We want a lower bound, and the uniform prior can result in a
    # (valid) bound that's higher than the empirical rate, so take the
    # min of the two.
    empirical_rate = successes / sampled_weight
    delta = hoeffding(len(sample), alpha)
    return large_sum + sampled_weight * max(0, empirical_rate - delta)
{% endcodeblock %}

And finally, here's how we can sample from an arbitrary list of items,
compure a lower bound on the weight of items that satisfy a predicate,
and compare that with the real lower bound.

{% codeblock lower_bound.py %}
def compare_bounds(items, rate, alpha, predicate):
    total = total_weight(items)
    # We expect a sample size of roughly rate * len(items), and
    # at most rate * len(items) large items.
    sample, large = sample_by_weight(items, rate, rate * total)
    lower_bound = eval_weight(total, sample, large, predicate, alpha)
    # Check if the lower bound is valid.
    actual = sum(weight for (index, weight) in items if predicate(index))
    return lower_bound <= actual + 1e-8, lower_bound, actual
{% endcodeblock %}

How do we test that? Far too often, I see tests for randomised
algorithms where the success rate is computed over randomly generated
inputs.  That's too weak!  For example, this approach could lead us to accept
that the identity function is a randomised sort function, with success
probability \\(\frac{1}{n!}.\\)

The property we're looking for is that, for any input, the success
rate (with the expectation over the pseudorandom sampling decisions)
is as high as requested.

For a given input (list of items and predicate), we can use the [Confidence sequence method (CSM)](http://pvk.ca/Blog/2018/07/06/testing-slo-type-properties-with-the-confidence-sequence-method/)
to confirm that the lower bound is valid at least
\\(1 - \alpha\\) of the time.

{% codeblock csm_test.py %}
def compare_bounds_generator(test_case, rate, alpha):
    items = [(i, w) for (i, (w, _)) in enumerate(test_case)]
    chosen = set(i for (i, (_, p)) in enumerate(test_case) if p)
    while True:
        yield compare_bounds(items, rate, alpha, lambda x: x in chosen)[0]


def check_bounds(test_case, rate, alpha):
    """Test case is a list of pairs of weight and predicate value
       rate is the sample rate
       alpha is the confidence parameter for the lower bound.
    """
    wanted = 1 - alpha  # The Hoeffding bound is conservative, so
                        # this should let csm_driver stop quickly.
    result = csm.csm_driver(compare_bounds_generator(test_case,
                                                     rate,
                                                     alpha),
                            wanted,
                            1e-6,  # Wrong conclusion with p < 1e-6.
                            file=sys.stderr
                            )
    stop, actual, *_ = result
    assert actual >= wanted, "Result: %s" % str(result)
{% endcodeblock %}

With a false positive rate of at most one in a million,[^lotta-errors] we can
run automated tests against `check_bounds`.  I'll use
[Hypothesis](https://hypothesis.works/) to generate list of pairs of weight and predicate value:

[^lotta-errors]: We're juggling a handful of error rates here. We're checking whether the success rate for the Lazy Knapsack sampling subroutine is at least as high as \\(1 - \alpha,\\) as requested in the test parameters, and we're doing so with another randomised procedure that will give an incorrect conclusion at most once every one million invocation.

{% codeblock test_bounds.py %}
from hypothesis import given, settings, Verbosity
import hypothesis.strategies as st

@given(test_case=st.lists(st.tuples(st.floats(min_value=0, max_value=1),
                                    st.booleans())),
       rate=st.floats(min_value=1e-6, max_value=0.5),
       alpha=st.floats(min_value=0.05, max_value=0.25))
def test_bounds(test_case, rate, alpha):
    check_bounds(test_case, rate, alpha)
{% endcodeblock %}

Bimodal inputs tend to be harder, so we can add a specialised test
generator.

{% codeblock test_bimodal_bounds.py %}
@given(test_case=st.lists(st.tuples(st.one_of(st.just(0.1), st.just(1)),
                                    st.booleans())),
       rate=st.floats(min_value=1e-6, max_value=0.5),
       alpha=st.floats(min_value=0.05, max_value=0.25))
def test_bimodal_bounds(test_case, rate, alpha):
    check_bounds(test_case, rate, alpha)
{% endcodeblock %}

Again, we use [Hypothesis](https://hypothesis.readthedocs.io/en/latest/data.html) to generate inputs, and
the [Confidence sequence method (available in C, Common Lisp, and Python)](https://github.com/pkhuong/csm) to check that the lower bound is
valid with probability at least \\(1 - \alpha\\).  The CSM tests
for this statistical property with power 1 and adjustable error rate
(in our case, one in a million): we only provide a generator
for success values, and the driver adaptively determines when it makes
sense to make a call and stop generating more data, while accounting
for multiple hypothesis testing.

TL;DR: the estimation algorithm for individual sampling passes works,
and the combination of [Hypothesis](https://hypothesis.works/) and [Confidence Sequence Method](https://github.com/pkhuong/csm)
lets us painlessly test for a statistical property.

We can iteratively use this sampling procedure to derive lower and
(symmetrically) upper bounds for the optimal Lagrange multiplier
\\(\lambda^\star,\\) and Hoeffding's inequality lets us control the
probability that the lower and upper bounds are valid.  Typically,
we'd use a tolerance of \\(\sqrt{\log(n) / n},\\) for an error
rate of \\(1 / n^2.\\) I prefer to simply use something like \\(7 /
\sqrt{n}:\\) the error rate is then less than \\(10^{-42},\\)
orders of manitude smaller than the probability of hardware failure in any given
nanosecond.[^memory-error]
We can still check for failure of our Las Vegas algorithm,
but if something went wrong, it's much more likely that we detected
a hardware failure than anything else.  It's like running [SuperPi](https://en.wikipedia.org/wiki/Super_PI)
to stress test a computer, except the work is useful. 😉

[^memory-error]: [This classic Google study](http://www.cs.toronto.edu/~bianca/papers/sigmetrics09.pdf) found 8% of DIMMs hit at least one error per year; that's more than one single-bit error every \\(10^9\\) DIMM-second, and they're mostly hard errors.  [More recently, Facebook](https://users.ece.cmu.edu/~omutlu/pub/memory-errors-at-facebook_dsn15.pdf) reported that uncorrectable errors affect 0.03% of servers each month; that's more than one uncorrectable error every \\(10^{10}\\) server-second.  If we performed one statistical test every nanosecond, the probability of memory failure alone would still dominate statistical errors by \\(10^{20}!\\)

Repeat as necessary to solve a knapsack
---------------------------------------

How many sampling passes do we need? Our bounds are in terms of the
sum of item weight: if we let our sample size be in
\\(\Theta(\sqrt{n}),\\) the sum of weights \\(\sum_i \|w_i\|\\) for
unfathomed items (that may or may not be chosen depending on the exact
optimal multiplier \\(\lambda^\star\\) in the current range) will very
probably shrink by a factor of \\(\Omega(n^{1/4}).\\) The initial sum can, in
the worst case, be exponentially larger than the bitlength of the
input, so even a division by \\(n^{1/4}\\) isn't necessarily that
great.

I intend to apply this Lazy Linear Knapsack algorithm on subproblems in
a more interesting solver, and I know that the sum of weights is
bounded by the size of the initial problem, so that's good enough for
me!  After a constant (\\(\approx 4\\)) number of passes, the
difference in item weight between the lower and upper bound on
\\(\lambda^\star\\) should also be at most 1.  One or two additional
passes will get me near optimality (e.g., within \\(10^{-4}\\)),
and the lower bound on \\(\lambda^\star\\) should thus yield
a super-optimal solution that's infeasible by at most \\(10^{-4},\\)
which is, for my intended usage (again), good enough.

Given an optimal enough \\(\lambda^\star,\\) we can construct an
explicit solution in one pass, plus a simple fixup for critical items.
This Lazy Knapsack seems pretty reasonable for parallel or GPU
computing: each sampling pass only needs to read the items (i.e., no
partitioning-like shuffling) before writing a fraction of the data to
a sample buffer, and we only need a constant number of passes (around
6 or 7) in the worst case.

<p><hr style="width: 50%"></p>
