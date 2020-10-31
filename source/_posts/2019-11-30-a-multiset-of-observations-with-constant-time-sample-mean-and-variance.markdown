---
layout: post
title: "A multiset of observations with constant-time sample mean and variance"
date: 2019-11-30 23:51:41 -0500
comments: true
categories: LazyKnapsack
---

<small>_Fixed notation issues in the "Faster multiset updates" section. Thank you Joonas._</small>

Let's say you have a [multiset (bag)](https://en.wikipedia.org/wiki/Multiset) of "reals" (floats or rationals),
where each value is a sampled observations.
It's easy to augment any implementation of the multiset ADT
to also return the sample mean of the values in the multiset in constant time:
track the sum of values in the multiset, as they are individually added and removed.
This requires one accumulator
and a counter for the number of observations in the multiset (i.e., constant space),
and adds a constant time overhead to each update.

It's not as simple when you also need the sample variance of the multiset \\(X\\), i.e.,

\\[\frac{1}{n - 1} \sum\sb{x \in X} (x - \hat{x})\sp{2},\\]

where \\(n = |X|\\) is the sample size and
\\(\hat{x}\\) is the sample mean \\(\sum\sb{x\in X} x/n,\\)
ideally with constant query time, 
and constant and update time overhead.

One could try to apply the textbook equality

\\[s\sp{2} = \frac{1}{n(n-1)}\left[n\sum\sb{x\in X} x\sp{2} - \left(\sum\sb{x\in X} x\right)\sp{2}\right].\\]

However, as [Knuth notes in TAoCP volume 2](https://books.google.com/books?id=Zu-HAwAAQBAJ&printsec=frontcover&dq=the+art+of+computer+programming+volume+2&hl=en&newbks=1&newbks_redir=0&sa=X&ved=2ahUKEwja5aGCzpPmAhWjY98KHYCGBksQuwUwAXoECAQQBw#v=onepage&q=welford%20technometrics&f=false),
this expression loses a lot of precision to round-off in floating point:
in extreme cases, the difference might be negative
(and we know the variance is never negative).
More commonly, we'll lose precision
when the sampled values are clustered around a large mean.
For example, the sample standard deviation of `1e8` and `1e8 - 1`
is `1`, same as for `0` and `1`.
However, the expression above would evaluate that to `0.0`, even in double precision:
while `1e8` is comfortably within range for double floats,
its square `1e16` is outside the range where all integers are represented exactly.

Knuth refers to a [better behaved recurrence by Welford](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.7503&rep=rep1&type=pdf), where
a running sample mean is subtracted from each new observation
before squaring.
[John Cook has a `C++` implementation](https://www.johndcook.com/blog/standard_deviation/)
of the recurrence that adds observations to a sample variance in constant time.
In Python, this streaming algorithm looks like this.

{% codeblock streaming_variance.py %}
class StreamingVariance:
    def __init__(self):
        self.n = 0
        self.mean = 0
        self.var_sum = 0  # centered 2nd moment (~variance of the sum of observations)

    def observe(self, v):
        self.n += 1
        if self.n == 1:
            self.mean = v
            return
        old_mean = self.mean
        self.mean += (v - old_mean) / self.n
        self.var_sum += (v - old_mean) * (v - self.mean)

        def get_mean(self):
        return self.mean

    def get_variance(self):
        return self.var_sum / (self.n - 1) if self.n > 1 else 0
{% endcodeblock %}

That's all we need for insert-only multisets,
but does not handle removals;
if only we had removals,
we could always implement updates (replacement)
as a removal and an insertion.

Luckily, `StreamingVariance.observe` looks invertible.
It's shouldn't be hard to recover the previous sample mean, given `v`,
and, given the current and previous sample means,
we can re-evaluate `(v - old_mean) * (v - self.mean)` and
subtract it from `self.var_sum`.

Let \\(\hat{x}\sp{\prime}\\) be the sample mean after `observe(v)`.
We can derive the previous sample mean \\(\hat{x}\\) from \\(v\\):

\\[(n - 1)\hat{x} = n\hat{x}\sp{\prime} - v \Leftrightarrow \hat{x} = \hat{x}\sp{\prime} + \frac{\hat{x}\sp{\prime} - v}{n-1}.\\]

This invertibility means that we can undo calls to `observe` in
LIFO order.  We can't handle arbitrary multiset updates, only a
stack of observation.  That's still better than nothing.

{% codeblock variance_stack.py %}
class VarianceStack:
    def __init__(self):
        self.n = 0
        self.mean = 0
        self.var_sum = 0  # variance of the sum

    def push(self, v):
        self.n += 1
        if self.n == 1:
            self.mean = v
            return
        old_mean = self.mean
        self.mean += (v - old_mean) / self.n
        self.var_sum += (v - old_mean) * (v - self.mean)

    def pop(self, v):
        assert self.n > 0
        if self.n == 1:
            self.n = 0
            self.mean = 0
            self.var_sum = 0
            return
        next_n = self.n - 1
        old_mean = self.mean
        self.mean = old_mean + (old_mean - v) / next_n
        # var_sum should never be negative, clamp it so.
        self.var_sum = max(0, self.var_sum - (v - self.mean) * (v - old_mean))
        self.n -= 1

    def get_mean(self):
        return self.mean

    def get_variance(self):
        return self.var_sum / (self.n - 1) if self.n > 1 else 0
{% endcodeblock %}

Before going any further, let's test this.

Testing the `VarianceStack`
---------------------------

The best way to test the `VarianceStack` is to execute a series of
`push` and `pop` calls, and compare the results of `get_mean` and
`get_variance` with batch reference implementations.

I could hardcode calls in unit tests.
However, that quickly hits diminishing returns in terms of
marginal coverage VS developer time.
Instead, I'll be lazy, completely skip unit tests,
and rely on [Hypothesis](https://hypothesis.works/),
its [high level "stateful" testing API](https://hypothesis.readthedocs.io/en/latest/stateful.html)
in particular.

We'll keep track of the values pushed and popped off the observation stack
in the driver: we must make sure they're matched in LIFO order,
and we need the stack's contents to compute the reference mean and variance.
We'll also want to compare the results with reference implementations,
modulo some numerical noise.  Let's try to be aggressive and bound
the number of float values between the reference and the actual results.

{% codeblock variance_stack_driver.py %}
import struct
import unittest
import hypothesis.strategies as st
from hypothesis.stateful import RuleBasedStateMachine, invariant, precondition, rule


def float_bits(x: float) -> int:
    bits = struct.unpack('=q', struct.pack('=d', x))[0]
    significand = bits % (1 << 63)
    # ~significand = -1 - significand. We need that instead of just
    # -significand to handle signed zeros.
    return significand if bits >= 0 else ~significand


FLOAT_DISTANCE = 2**10


def assert_almost_equal(x, y, max_delta=FLOAT_DISTANCE):
    assert abs(float_bits(x) - float_bits(y)) <= max_delta


class VarianceStackDriver(RuleBasedStateMachine):
    def __init__(self):
        super(VarianceStackDriver, self).__init__()
        self.values = []

    @rule(v=st.floats(allow_nan=False, allow_infinity=False))
    def push(self, v):
        self.values.append(v)

    # Don't generate `pop()` calls when the stack is empty.
    @precondition(lambda self: self.values)
    @rule()
    def pop(self):
        self.values.pop()

    def reference_mean(self):
        if self.values:
            return sum(self.values) / len(self.values)
        return 0

    def reference_variance(self):
        n = len(self.values)
        if n <= 1:
            return 0
        mean = self.reference_mean()
        return sum(pow(x - mean, 2) for x in self.values) / n

    @invariant()
    def mean_matches(self):
        assert_almost_equal(self.reference_mean(), self.reference_mean())

    @invariant()
    def variance_matches(self):
        assert_almost_equal(self.reference_variance(),
                            self.reference_variance())


StackTest = VarianceStackDriver.TestCase

if __name__ == '__main__':
    unittest.main()
{% endcodeblock %}

This initial driver does not even use the `VarianceStack` yet.
All it does is push values to the reference stack,
pop values when the stack has something to pop,
and check that the reference implementations match themselves after each call:
I want to first shake out any bug in the test harness itself.

[Not surprisingly](https://twitter.com/DRMacIver/status/1095662615223848960),
Hypothesis does find an issue in the reference implementation:

    Falsifying example:
    state = VarianceStackDriver()
    state.push(v=0.0)
    state.push(v=2.6815615859885194e+154)
    state.teardown()

We get a numerical `OverflowError` in `reference_variance`: `2.68...e154 / 2`
is slightly greater than `sqrt(sys.float_info.max) = 1.3407807929942596e+154`,
so taking the square of that value errors out instead of returning infinity.

Let's start by clamping the range of the generated floats.

{% codeblock variance_stack_driver.py %}
import math
import sys
...


MAX_RANGE = math.sqrt(sys.float_info.max) / 2

FLOAT_STRATEGY = st.floats(min_value=-MAX_RANGE, max_value=MAX_RANGE)

class VarianceStackDriver(RuleBasedStateMachine):
    ...
    
    @rule(v=FLOAT_STRATEGY)
    def push(self, v):
        self.variance_stack.push(v)
        self.values.append(v)

    ...
{% endcodeblock %}

Now that the test harness doesn't find fault in itself,
let's hook in the `VarianceStack`, and see what happens
when only `push` calls are generated (i.e., first test
only the standard streaming variance algorithm).

{% codeblock variance_stack_driver.py %}
def assert_almost_equal(x, y, max_delta=FLOAT_DISTANCE):
    distance = abs(float_bits(x) - float_bits(y))
    # Print out some useful information on failure.
    assert distance <= max_delta, '%.18g != %.18g (%f)' % (x, y, math.log(distance, 2))

class VarianceStackDriver(RuleBasedStateMachine):
    def __init__(self):
        super(VarianceStackDriver, self).__init__()
        self.values = []
        self.variance_stack = VarianceStack()

    @rule(v=FLOAT_STRATEGY)
    def push(self, v):
        self.variance_stack.push(v)
        self.values.append(v)

    # Never generate `pop()`
    @precondition(lambda self: self.values and False)
    @rule()
    def pop(self):
        self.values.pop()

    def reference_mean(self):
        if self.values:
            return sum(self.values) / len(self.values)
        return 0

    def reference_variance(self):
        n = len(self.values)
        if n <= 1:
            return 0
        mean = self.reference_mean()
        return sum(pow(x - mean, 2) for x in self.values) / n

    @invariant()
    def mean_matches(self):
        assert_almost_equal(self.reference_mean(),
                            self.variance_stack.get_mean())

    @invariant()
    def variance_matches(self):
        assert_almost_equal(self.reference_variance(),
                            self.variance_stack.get_variance())
{% endcodeblock %}

This already fails horribly.

    Falsifying example:
    state = VarianceStackDriver()
    state.push(v=1.0)
    state.push(v=1.488565707357403e+138)
    state.teardown()
    F

The reference finds a variance of `5.54e275`,
which is very much not the streaming computation's `1.108e276`.
We can manually check that the reference is wrong:
it's missing the `n - 1` correction term in the denominator.

We should use this updated reference.

{% codeblock variance_stack_driver.py %}
class VarianceStackDriver(RuleBasedStateMachine):
    ...

    def reference_variance(self):
        n = len(self.values)
        if n <= 1:
            return 0
        mean = self.reference_mean()
        return sum(pow(x - mean, 2) for x in self.values) / (n - 1)
{% endcodeblock %}

Let's now re-enable calls to `pop()`.

{% codeblock variance_stack_driver.py %}
class VarianceStackDriver(RuleBasedStateMachine):
    ...

    @precondition(lambda self: self.values)
    @rule()
    def pop(self):
        self.variance_stack.pop(self.values[-1])
        self.values.pop()
{% endcodeblock %}

And now things fail in new and excitingly numerical ways.

    Falsifying example:
    state = VarianceStackDriver()
    state.push(v=0.0)
    state.push(v=0.00014142319560050964)
    state.push(v=14188.9609375)
    state.pop()
    state.teardown()
    F

This counter-example fails with the online variance returning `0.0` instead of `1e-8`.
That's not unexpected:
removing (the square of) a large value from a running sum
spells catastrophic cancellation.
It's also not *that* bad for my use case,
where I don't expect to observe very large values.

Another problem for our test harness is that
floats are very dense around `0.0`, and 
I'm ok with small (around `1e-8`) absolute error
because the input and output will be single floats.

Let's relax `assert_almost_equal`, and
restrict generated observations to fall
in \\([-2\sp{-12}, 2\sp{12}].\\)

{% codeblock variance_stack_driver.py %}
# Let values be off by ~1 single float ULP
FLOAT_DISTANCE = 2**32

# or by 1e-8
ABSOLUTE_EPS = 1e-8


def assert_almost_equal(x, y, max_delta=FLOAT_DISTANCE, abs_eps=ABSOLUTE_EPS):
    delta = abs(x - y)
    distance = abs(float_bits(x) - float_bits(y))
    assert distance <= max_delta or delta <= abs_eps, '%.18g != %.18g (%f)' % (
        x, y, math.log(distance, 2))


# Avoid generating very large observations.
MAX_RANGE = 2**12

FLOAT_STRATEGY = st.floats(width=32, min_value=-MAX_RANGE, max_value=MAX_RANGE)
{% endcodeblock %}

With all these tweaks to make sure we generate easy (i.e., [interesting](https://twitter.com/alexwlchan/status/1095663620422332416))
test cases, Hypothesis fails to find a failure after its default time budget.

I'm willing to call that a victory.

From stack to full multiset
---------------------------

We have tested code to undo updates in Welford's classic streaming variance algorithm.
Unfortunately, inverting `push`es away only works for LIFO edits,
and we're looking for arbitrary inserts and removals (and updates) to a multiset
of observations.

However, both the mean \\(\hat{x} = \sum\sb{x\in X} x/n\\) and 
the centered second moment \\(\sum\sb{x\in X}(x - \hat{x})\sp{2}\\)
are order-independent:
they're just sums over all observations.
Disregarding round-off, we'll find the same mean and second moment regardless
of the order in which the observations were pushed in.
Thus, whenever we wish to remove an observation from the multiset,
we can assume it was the last one added to the estimates,
and pop it off.

We think we know how to implement running mean and variance for a multiset of observations.
How do we test that with Hypothesis?

The hardest part about testing dictionary (map)-like interfaces
is making sure to generate valid identifiers when removing values.
As it turns out, Hypothesis has built-in support for this important use case,
with its [Bundles](https://hypothesis.readthedocs.io/en/latest/stateful.html#rule-based-state-machines).
We'll use that to test a dictionary from observation name to observation value,
augmented to keep track of the current mean and variance of all values.

{% codeblock variance_multiset_driver.py %}
class VarianceBag(VarianceStack):
    def update(self, old, new):
        # Replace one instance of `old` with `new` by
        # removing `old` and inserting `new`.
        self.pop(old)
        self.push(new)


class VarianceBagDriver(RuleBasedStateMachine):
    keys = Bundle("keys")

    def __init__(self):
        super(VarianceBagDriver, self).__init__()
        self.entries = dict()
        self.variance_bag = VarianceBag()

    @rule(target=keys, k=st.binary(), v=FLOAT_STRATEGY)
    def add_entry(self, k, v):
        if k in self.entries:
            self.update_entry(k, v)
            return multiple()

        self.entries[k] = v
        self.variance_bag.push(v)
        return k

    @rule(k=consumes(keys))
    def del_entry(self, k):
        self.variance_bag.pop(self.entries[k])
        del self.entries[k]

    @rule(k=keys, v=FLOAT_STRATEGY)
    def update_entry(self, k, v):
        self.variance_bag.update(self.entries[k], v)
        self.entries[k] = v

    def reference_mean(self):
        if self.entries:
            return sum(self.entries.values()) / len(self.entries)
        return 0

    def reference_variance(self):
        n = len(self.entries)
        if n <= 1:
            return 0
        mean = self.reference_mean()
        return sum(pow(x - mean, 2) for x in self.entries.values()) / (n - 1)

    @invariant()
    def mean_matches(self):
        assert_almost_equal(self.reference_mean(),
                            self.variance_bag.get_mean())

    @invariant()
    def variance_matches(self):
        assert_almost_equal(self.reference_variance(),
                            self.variance_bag.get_variance())


BagTest = VarianceBagDriver.TestCase
{% endcodeblock %}


Each call to `add_entry` will either go to `update_entry` if
the key already exists, or add an observation to the dictionary
and streaming estimator.  If we have a new key, it is added
to the `keys` Bundle; calls to `del_entry` and `update_entry`
draw keys from this Bundle.  When we remove an entry, it's
also consumed from the `keys` Bundle.

Hypothesis finds no fault with our new implementation of dictionary-with-variance,
but `update` seems like it could be much faster and numerically stable,
and I intend to mostly use this data structure for calls to `update`.

Faster multiset updates
-----------------------

The key operation for my use-case is to update one observation
by replacing its `old` value with a `new` one.
We can maintain the estimator by popping `old` away and pushing `new` in,
but this business with updating the number of observation `n` and
rescaling everything seems like a lot of numerical trouble.

We should be able to do better.

We're replacing the multiset of sampled observations \\(X\\) with
\\(X\sp{\prime} = X \setminus \\{\textrm{old}\\} \cup \\{\textrm{new}\\}.\\)
It's easy to maintain the mean after this update: \\(\hat{x}\sp{\prime} = \hat{x} + (\textrm{new} - \textrm{old})/n.\\)

The update to `self.var_sum`, the sum of squared differences from the mean, is trickier.
We start with \\(v = \sum\sb{x\in X} (x - \hat{x})\sp{2},\\)
and we wish to find \\(v\sp{\prime} = \sum\sb{x\sp{\prime}\in X\sp{\prime}} (x\sp{\prime} - \hat{x}\sp{\prime})\sp{2}.\\)

Let \\(\delta = \textrm{new} - \textrm{old}\\) and \\(\delta\sb{\hat{x}} = \delta/n.\\)
We have
\\[\sum\sb{x\in X} (x - \hat{x}\sp{\prime})\sp{2} = \sum\sb{x\in X} [(x - \hat{x}) - \delta\sb{\hat{x}}]\sp{2},\\]
and
\\[[(x - \hat{x}) - \delta\sb{\hat{x}}]\sp{2} = (x - \hat{x})\sp{2} - 2\delta\sb{\hat{x}} (x - \hat{x}) + \delta\sb{\hat{x}}\sp{2}.\\]

We can reassociate the sum, and find 

\\[\sum\sb{x\in X} (x - \hat{x}\sp{\prime})\sp{2} = \sum\sb{x\in X} (x - \hat{x})\sp{2} - 2\delta\sb{\hat{x}} \left(\sum\sb{x \in X} x - \hat{x}\right) + n \delta\sb{\hat{x}}\sp{2}\\]

Once we notice that \\(\hat{x} = \sum\sb{x\in X} x/n,\\)
it's clear that the middle term sums to zero, and we find
the very reasonable

\\[v\sb{\hat{x}\sp{\prime}} = \sum\sb{x\in X} (x - \hat{x})\sp{2} + n \delta\sb{\hat{x}}\sp{2} = v + \delta \delta\sb{\hat{x}}.\\]

This new accumulator \\(v\sb{\hat{x}\sp{\prime}}\\) corresponds to the sum of the
squared differences between the old observations \\(X\\) and the new mean \\(\hat{x}\sp{\prime}\\).
We still have to update one observation from `old` to `new`.
The remaining adjustment to \\(v\\) (`self.var_sum`) corresponds to
going from \\((\textrm{old} - \hat{x}\sp{\prime})\sp{2}\\)
to \\((\textrm{new} - \hat{x}\sp{\prime})\sp{2},\\)
where \\(\textrm{new} = \textrm{old} + \delta.\\)

After a bit of algebra, we get
\\[(\textrm{new} - \hat{x}\sp{\prime})\sp{2} = [(\textrm{old} - \hat{x}\sp{\prime}) + \delta]\sp{2} = (\textrm{old} - \hat{x}\sp{\prime})\sp{2} + \delta (\textrm{old} - \hat{x} + \textrm{new} - \hat{x}\sp{\prime}).\\]

The adjusted \\(v\sb{\hat{x}\sp{\prime}}\\) already includes
\\((\textrm{old} - \hat{x}\sp{\prime})\sp{2}\\)
in its sum, so we only have to add the last term
to obtain the final updated `self.var_sum`

\\[v\sp{\prime} = v\sb{\hat{x}\sp{\prime}} + \delta (\textrm{old} - \hat{x} + \textrm{new} - \hat{x}\sp{\prime}) = v + \delta [2 (\textrm{old} - \hat{x}) + \textrm{new} - \hat{x}\sp{\prime}].\\]

That's our final implementation for `VarianceBag.update`,
for which Hypothesis also fails to find failures.

{% codeblock VarianceBag.py %}
class VarianceBag(VarianceStack):
    def update(self, old, new):
        assert self.n > 0
        if self.n == 1:
            self.mean = new
            self.var_sum = 0
            return
        delta = new - old
        old_mean = self.mean
        delta_mean = delta / self.n
        self.mean += delta_mean

        adjustment = delta * (2 * (old - old_mean) + (delta - delta_mean))
        self.var_sum = max(0, self.var_sum + adjustment)
{% endcodeblock %}

How much do you trust testing?
------------------------------

We have automated property-based tests and some human-checked proofs.
Ship it?

I was initially going to ask a [CAS](https://en.wikipedia.org/wiki/Computer_algebra_system)
to check my reformulations,
but the implicit \\(\forall\\) looked messy.
Instead, I decided to check the induction hypothesis implicit in
`VarianceBag.update`, and enumerate all cases up to a certain number
of values with [Z3](https://github.com/Z3Prover/z3/wiki) in IPython.

    In [1]: from z3 import *
    In [2]: x, y, z, new_x = Reals("x y z new_x")
    In [3]: mean = (x + y + z) / 3
    In [4]: var_sum = sum((v - mean) * (v - mean) for v in (x, y, z))
    In [5]: delta = new_x - x
    In [6]: new_mean = mean + delta / 3
    In [7]: delta_mean = delta / 3
    In [8]: adjustment = delta * (2 * (x - mean) + (delta - delta_mean))
    In [9]: new_var_sum = var_sum + adjustment
    
    # We have our expressions. Let's check equivalence for mean, then var_sum
    In [10]: s = Solver() 
    In [11]: s.push()
    In [12]: s.add(new_mean != (new_x + y + z) / 3)
    In [13]: s.check()
    Out[13]: unsat  # No counter example of size 3 for the updated mean
    In [14]: s.pop()
    
    In [15]: s.push()
    In [16]: s.add(new_mean == (new_x + y + z) / 3)  # We know the mean matches
    In [17]: s.add(new_var_sum != sum((v - new_mean) * (v - new_mean) for v in (new_x, y, z)))
    In [18]: s.check()
    Out[18]: unsat  # No counter example of size 3 for the updated variance

Given this script, it's a small matter of programming to generalise
from 3 values (`x`, `y`, and `z`) to any fixed number of values, and
generate all small cases up to, e.g., 10 values.

{% codeblock z3-check.py %}
def updated_expressions(vars, new_x):
    x = vars[0]
    num_var = len(vars)
    mean = sum(vars) / num_var
    var_sum = sum((v - mean) * (v - mean) for v in vars)
    delta = new_x - x
    delta_mean = delta / num_var
    new_mean = mean + delta_mean
    adjustment = delta * (2 * (x - mean) + (delta - delta_mean))
    new_var_sum = var_sum + adjustment
    return new_mean, new_var_sum


def test_num_var(num_var):
    assert num_var > 0
    vars = [Real('x_%i' % i) for i in range(0, num_var)]
    new_x = Real('new_x')

    new_mean, new_var_sum = updated_expressions(vars, new_x)
    new_vars = [new_x] + vars[1:]
    s = Solver()
    s.push()
    s.add(new_mean != sum(new_vars) / num_var)
    result = s.check()
    print('updated mean %s' % result)
    if result != unsat:
        print(s.model())
        return False
    s.pop()

    s.push()
    s.add(new_mean == sum(new_vars) / num_var)
    s.add(new_var_sum != sum(
        (v - new_mean) * (v - new_mean) for v in new_vars))
    result = s.check()
    print('updated variance %s' % result)
    if result != unsat:
        print(s.model())
        return False
    return True


for i in range(1, 11):
    print('testing n=%i' % i)
    if test_num_var(i):
        print('OK')
    else:
        print('FAIL %i' % i)
        break
{% endcodeblock %}

I find the most important thing when it comes to using automated proofs
is to insert errors and confirm we can find the bugs we're looking for.

I did that by manually mutating the expressions for `new_mean` and `new_var_sum`
in `updated_expressions`.  This let me find a simple bug in the initial
implementation of `test_num_var`: I used `if not result` instead of `result != unsat`,
and both `sat` and `unsat` are truthy.  The code initially failed to flag a failure
when `z3` found a counter-example for our correctness condition!

And now I'm satisfied
---------------------

I have code to augment an arbitrary multiset or dictionary with
a running estimate of the mean and variance;
that code is based on a classic recurrence,
with some new math checked by hand,
with automated tests,
and with some exhaustive checking of small inputs (to which I claim most bugs can be reduced).

I'm now pretty sure the code works, but there's another more obviously correct way to solve that update problem.
This [2008 report by Philippe Pébay](https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/2008/086212.pdf)[^pebay2016]
presents formulas to compute the mean, variance, and arbitrary moments
in one pass,
and shows how to combine accumulators,
a useful operation in parallel computing.

[^pebay2016]: There's also a [2016 journal article by Pébay and others](https://www.osti.gov/biblio/1426900) with numerical experiments, but I failed to implement their simpler-looking scalar update…

We could use these formulas to [augment an arbitrary \\(k\\)-ary tree](http://blog.sigfpe.com/2010/11/statistical-fingertrees.html)
and re-combine the merged accumulator as we go back up the (search)
tree from the modified leaf to the root.
The update would be much more stable (we only add and merge observations),
and incur logarithmic time overhead (with linear space overhead).
However, given the same time budget, and a *logarithmic* space overhead,
we could also implement the constant-time update with arbitrary precision
software floats, and probably guarantee even better precision.

The constant-time update I described in this post demanded more effort to convince myself
of its correctness, but I think it's always a better option than
an augmented tree for serial code, especially if initial values
are available to populate the accumulators with batch-computed
mean and variance.
I'm pretty sure the code works, and [it's up in this gist](https://gist.github.com/pkhuong/549106fc8194c0d1fce85b00c9e192d5).
I'll be re-implementing it in C++
because that's the language used by the project that lead me to this problem;
feel free to steal that gist.

<p><hr style="width: 50%"></p>
