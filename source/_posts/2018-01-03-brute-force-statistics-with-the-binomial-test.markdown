---
layout: post
title: "Brute force statistics with the Binomial test"
date: 2018-01-03 16:08:00 -0500
comments: true
published: false
categories: 
---

As computers have gotten faster, and parallel computing more boring,
testing properties of code has become easier: it usually makes more
sense to code up a stupid "overkill" testing loop and waste CPU cycles
than to try and be smart about things and slowly design a complex (and
potentially buggy) test driver.  For example, I now routinely test
functions on 32 bit integers or floats by enumerating all possible
values.

However, this lazy approach breaks down for statistical properties,
like SLAs.  How should we test that our service times out less than
0.1% of the time; or that, between two pieces of code A and B, A is
faster than B 99% of the time; or that some approximation function C
is within 10% of its reference implementation, 95% of the time?

The parametric tests we're usually taught in school, like 
[Student's t-test](), tend to be concerned with summary statistics:
average difference, ratio, etc.  Even when we do want to look at
averages (e.g., is A faster than B *on average*?), we often expect
distributions to be nasty (skewed, multimodal, etc.), and it's not
clear that the parametric assumptions hold.

{% img center /images/2018-01-03-brute-force-statistics-with-the-binomial-test/clt.jpg %}

What I tend to see for SLA-type properties is that we run an
experiment and track the percentage of times the SLA threshold is met
(response time less than the timeout limit), or some other predicate
(A is faster than B) is satisfied.  We then generate a report that
tells us:

1. We executed \\(n\\) calls or trials.
2. \\(s\\) of them met the SLA threshold or satisfied some condition.
3. \\(\frac{s}{n}\\) is more than our target success rate (or not).

What the report doesn't say is whether the result in 3. is
statistically significant.  If I make one call to a service, and 100%
of them times out, I probably shouldn't rush to make any decision.
How many calls do I need to make to be confident that we satisfy the
SLA?  Usually, we just shrug and let \\(n = 10\sp{6}\\): that ought to
be enough for anyone ;)

The end result is that such tests are either so slow that everyone
complains about them, or so flaky that everyone complains about them.
In either case, we are incentivised *not* to test statistical
properties at all, or to do so with thresholds so lax that we get
[smoke tests]().

What if we could compute "normal" [\\(p\\)-values](), with confidence bounds?
For example, what if the last part of the report could tell us: "I'm
pretty sure that, for 99.9% of requests, the response time is less than
\\(t\sb{\textrm{max}}\\), because the odds of observing our data if
that were false are tiny (less than \\(\varepsilon = 10\sp{-5}\\))."
The report could also tell us that the SLA is not satisfied frequently
enough, or that we need more data.  What if, even better, the
statistical testing framework could end tests early when the property
is obviously satisfied or violated?

I'll describe small, dependency-free, functions to generate reports
that tell us:

1. The empirical success rate \\(\hat{p}\\).
2. Whether we may conclude that the underlying (unknown) success rate
\\(p\\) differs from some threshold \\(\alpha\\), with a tunable false
positive rate \\(\varepsilon\\) (e.g., one in a billion).
3. A confidence interval for \\(p\\).

The functions implement an adaptive scheme that requires less data
when the empirical success rate is far from the threshold \\(\alpha\\).

For example, [flip a coin, is fair?] [is unfair?]

%% Insert example *CSM* report here.

We can do most of the above with a [binomial test]().  Our data
distribution (e.g., the distribution of response times) may be really
weird, but the result of our predicate--was the response fast enough?
was A actually faster than B? was the result of C close enough to the
reference?--is either true or false.  We "only" have to make sure that
the truth values are independent and identically distributed.

There is however a mismatch between the question answered by the
binomial test--given this data, can we conclude that the actual
"success" ratio is different than, e.g., 5%?--and the one we usually
ask when testing computer programs.

The straight Binomial test makes sense in, e.g., biology.  If we want
to [look at the distribution of puffins on certain islands](), we only
have so many islands to look at.  Once we've gathered as much data as
we can, we must make the best out of what we have: either there's
enough to make a reasonable inference, or we throw our hands in the
air and say we don't have a statistically significant result.  When we
test computer programs, we can often generate an arbitrary amount of
data.  That's a boon for the persistent.  For the rest of us, the new
challenge is to determine when we have enough to make a call.  The
sooner we can stop, the faster we report results, and the more likely
people are to use our statistical tests.

This post is long, because I want to both show how to use the tools I
describe here, and how to implement them without any statistical
computing library.  There are six more sections; sections 2 and 5 are
concerned with numerical computation details, and aren't necessary to
understand how to use the code.

* [Section 1]() introduces the confidence sequence method, the
  statistical test that lets us generate statistically sound reports
  like the above, while terminating quickly when the system under
  test is obviously broken or working.
* [Section 2]() describes a practical implementation of the test,
  without non-standard dependency.
* [Section 3]() shows how to use the confidence sequence method (CSM)
  for SLA-like properties, and how to estimate power.
* [Section 4]() builds on top of the CSM to implement an Exact test.
* [Section 5]() implements confidence intervals for the binomial
  distribution with a Beta posterior, without special dependencies.
* [Section 6]() winds down with a summary of tricks to really use the
  tools presented here, especially within continuous integration
  pipelines.

A simple stopping criterion for Monte Carlo tests
-------------------------------------------------

The binomial test requires us to set a fixed number of trials ahead of
time, before testing for statistical significance.  For computer
generated data, it's usually preferable to assume we have an unbounded
amount of observations, and dynamically determine when we can
stop. That's where Ding, Gandy, and Hahn's 
[confidence sequence method (CSM)](https://arxiv.org/abs/1611.01675)
comes in.  This simple dynamic criterion tells us when we have enough
data to reliably determine that the unknown but constant success
probability \\(p\\) for our experiment (e.g., the timeout percentage)
is different than our threshold \\(\alpha\\) (e.g., 99.9%).  We stop
when the number of successes and trials is such that, if 
\\(p = \alpha\\), the probability of *ever* observing such an unlikely 
number of successes and trials would be less than the target false 
positive rate \\(\varepsilon\\) (e.g., \\(10\sp{-5}\\)).

This method is actually sound, unlike the usual "p-hacking" trick of
extending an experiment until \\(p < \alpha\\).  The stopping criterion
is a direct application of
[this beautiful result by Robbins](https://projecteuclid.org/euclid.aoms/1177696786).
The CSM criterion does not instruct us to stop merely when, if the
null hypothesis were true, the probability of observing the results
(the current number of successes and failures) would be less than
\\(\varepsilon\\).  It instead check for such a stringent condition at
each iteration that the probability of satisfying that condition *even
once in an infinite stream of values* would be less than
\\(\varepsilon\\) if the null hypothesis (\\(p = \alpha\\)) held.

The criterion is also easy to implement, and does not rely on anything
fancier than logarithms.  Given \\(n\\) trials (executions) and
\\(s\\) successes, we may conclude that the unknown probability of
success \\(p\\) (with maximum likelihood estimator \\(\hat{p} = \frac{s}{n}\\))
differs from our threshold \\(\alpha\\), with
a false positive rate less than \\(\varepsilon\\), if

\\[{n \choose s} \alpha\sp{s} (1-\alpha)\sp{n - s} < \frac{\varepsilon}{n + 1}.\\]

Intuitively, the criterion checks whether the probability of observing
exactly that \\(s\\) successes after \\(n\\) trials, given a
[binomial]() with success probability \\(\alpha\\), is very low.  As
we perform more checks, the right-hand side becomes more and more
demanding, almost like a [Bonferroni correction]().  This kind of
makes sense intuitively, but is far from a real proof.

We can find a formal proof by directly applying [this inequality of Robbins's](https://projecteuclid.org/euclid.aoms/1177696786),
which guarantees that the odds of the inequality being satisfied *even
once* in an unbounded stream of values is less than \\(\varepsilon\\),
if \\(p = \alpha\\).  Ding and others additionally show that, if 
\\(p \neq \alpha\\), the inequality will eventually be satisfied, with
probability 1.

There are more sophisticated stopping methods that are tighter (stop
earlier), especially when the number of trials has an upper limit
(e.g., we'll never want to run more than a thousand trials).  However,
the difference is often not that large, especially when the trial
limit is high, and the more sophisticated criteria are harder to code.

The implementation is simple enough that I already have 
[ready-to-run code for SBCL, Python 3, and C on github](),
available under the 3-clause BSD license.  If you don't care about
implementing the criterion,
[skip to the section on *using* the confidence sequence method.]()

Implementing CSM on a real computer
-----------------------------------

There are two problems when it comes to implementing
[CSM](https://arxiv.org/abs/1611.01675) on real computers.  The first
one is that we don't have reals, and must make do with floating
point (fixed point is also an option, for the highly motivated).  The
second is that computing \\({n \choose s} = \frac{n!}{s!(n - s)!}\\)
can be slow and quickly involves large numbers: 
\\(171! > 10\sp{309}\\), which is already too large for double floats,
and we want to handle \\(n\\) and \\(s\\) in the thousands or millions.

This tells me we want to work in the log domain, and directly bound
	\\(\log {n \choose s}\\) or \\(\log k!\\), instead of slowly computing
factorials, only to realise that they are too big for double floats
when we attempt to take their logarithm.  Since we're only interested
in conservatively testing for an inequality, we should also compensate
for rounding errors by nudging intermediate values up or down after
rounding.

There are many ways to bound \\(\log k!\\).  Another 
[result of Robbins's](http://www.jstor.org/stable/2308012)
is tight enough, and simple to implement:

\\[\sqrt{2\pi} k\sp{k + 1/2} \exp\left(\frac{1}{12k + 1} - k\right)
\leq k! \leq
\sqrt{2\pi} k\sp{k + 1/2} \exp\left(\frac{1}{12k} - k\right),\\]

or, in \\(\log\\):

\\[\log\sqrt{2\pi} + \left(k + \frac{1}{2}\right) \log k + \frac{1}{12k + 1} - k
\leq \log k! \leq
\log\sqrt{2\pi} + \left(k + \frac{1}{2}\right)\log k + \frac{1}{12k} - k.\\]

Given these simple upper and lower bounds on \\(\log k!\\), we can
upper bound \\(\log{n \choose s} = \log n! - \log s! - \log (n - s)!\\):

\\[\log{n \choose s} \leq 
-\log\sqrt{2\pi} +
\left(n + \frac{1}{2}\right)\log n -
  \left(s + \frac{1}{2}\right)\log s -
  \left(n - s + \frac{1}{2}\right)\log (n - s) +
\frac{1}{12n} - \frac{1}{12s + 1} - \frac{1}{12(n - s) + 1}.\\]

We can precompute a floating (or fixed) point value that
overapproximates \\(-\log\sqrt{2\pi}\\) offline.  Except for the
\\(\log\\)s, all the operations should be performed with at most half
an ULP of error.  We can easily compensate for these rounding errors by
incrementing intermediate values to the next higher floating point
value (in practice, rounding is probably dominated by Robbins's
approximation, but it doesn't hurt).  The 
[documentation for GNU libm](https://www.gnu.org/software/libc/manual/html_node/Errors-in-Math-Functions.html) claims their implementation of
\\(\log\\) is never off by more than 3 ULP; we can round up to the
fourth next floating point value to obtain a conservative upper bound on
\\(\log x\\).

I precomputed a floating point overapproximation of
\\(-\log\sqrt{2\pi}\\) with [computable-reals](), a continued
fraction package for Common Lisp.

    CL-USER> (require 'computable-reals)
    NIL
    CL-USER> (computable-reals:-r
              (computable-reals:log-r
               (computable-reals:sqrt-r computable-reals:+2pi-r+)))
    -0.91893853320467274178...
    CL-USER> (computable-reals:ceiling-r 
              (computable-reals:*r *
                                   (ash 1 53)))
    -8277062471433908
    -0.65067431749790398594...
    CL-USER> (* -8277062471433908 (expt 2d0 -53))
    -0.9189385332046727d0
    CL-USER> (computable-reals:-r (rational *)
                                  ***)
    +0.00000000000000007224...
    
We can safely replace \\(-\log\sqrt{2\pi}\\) with 
`-0.9189385332046727`, or, equivalently, `scalbn(-8277062471433908.0, -53)`,
for an upper bound.  If we wanted a lower bound, we could decrement
the integer significand by one.

Next, we need to easily increment floating point values by one (or
four) ULPs.  The easiest way to do that is to directly manipulate the
binary representation of FP values.  On all the architectures I care
about, double precision FP values can also be bitcast as native endian
64-bit integers with a sign-magnitude representation (32 bits for
single floats).  SBCL exposes that representation with
`sb-kernel:double-float-{low,high}-bits`; in C, I would `memcpy`.

    CL-USER> (sb-kernel:double-float-high-bits pi)
    1074340347
    CL-USER> (sb-kernel:double-float-low-bits pi)
    1413754136
    CL-USER> (sb-kernel:double-float-high-bits (- pi))
    -1073143301
    CL-USER> (sb-kernel:double-float-low-bits (- pi))
    1413754136

SBCL returns two 32-bit values because CMUCL originally only ran on 32
bit machines.  We can combine the pair of 32-bit values into a single
signed 64-bit integer with some bitwise arithmetic.  However, while
it's clear that the low half isn't affected by the sign of the float
value (as expected in sign-magnitude), the high half doesn't look
right: `sb-kernel:double-float-high-bits` directly takes the high half
of the sign-magnitude representation and sign-extends it.  We want to
correctly convert the sign-magnitude integer to two's complement.

{% codeblock double-float-bits lang:lisp %}
(defun double-float-bits (x)
  (let* ((fx (float x 1d0))
         (hi (sb-kernel:double-float-high-bits fx))
         (lo (sb-kernel:double-float-low-bits fx))
         (word (+ (ash (ldb (byte 31 0) hi) 32) lo)))
    ;; if hi is negative, we want to negate `word`.  However, we also
    ;; want to differentiate between 0.0 and -0.0.  Return -1 - word
    ;; instead of -word.
    ;;
    ;; -1 - word = (lognot word) = (logxor word -1).
    (logxor word (ash hi -32))))
{% endcodeblock %}

    CL-USER> (double-float-bits pi)
    4614256656552045848
    CL-USER> (double-float-bits (- pi))
    -4614256656552045849

The two's complement value for `pi` is one less than 
`(- (double-float-bits pi))` because two's complement does not support
signed zeros.

    CL-USER> (eql 0 (- 0))
    T
    CL-USER> (eql 0d0 (- 0d0))
    NIL
    CL-USER> (double-float-bits 0d0)
    0
    CL-USER> (double-float-bits -0d0)
    -1

We also want to go the other way: we have to convert from two's
complement back to (sign-extended) sign-magnitude.

{% codeblock bits-double-float lang:lisp %}
(defun bits-double-float (x)
  ;; convert back to sign-magnitude: if x is negative, all but the
  ;; sign bit must be flipped again.
  (let ((x (logxor x (ldb (byte 63 0) (ash x -64)))))
    (sb-kernel:make-double-float (ash x -32)
                                 (ldb (byte 32 0) x))))
{% endcodeblock %}

We can quickly check that the round trip from float to integer and back
is an identity.

    CL-USER> (eql pi (bits-double-float (double-float-bits pi)))
    T
    CL-USER> (eql (- pi) (bits-double-float (double-float-bits (- pi))))
    T
    CL-USER> (eql 0d0 (bits-double-float (double-float-bits 0d0)))
    T
    CL-USER> (eql -0d0 (bits-double-float (double-float-bits -0d0)))
    T

We can also check that incrementing or decrementing the integer
representation does increase or decrease the floating point value.

    CL-USER> (< (bits-double-float (1- (double-float-bits pi))) pi)
    T
    CL-USER> (< (bits-double-float (1- (double-float-bits (- pi)))) (- pi))
    T
    CL-USER> (bits-double-float (1- (double-float-bits 0d0)))
    -0.0d0
    CL-USER> (bits-double-float (1+ (double-float-bits -0d0)))
    0.0d0
    CL-USER> (bits-double-float (1+ (double-float-bits 0d0)))
    4.9406564584124654d-324
    CL-USER> (bits-double-float (1- (double-float-bits -0d0)))
    -4.9406564584124654d-324

The code doesn't handle special values like infinities or NaNs, but
that's out of scope for the CSM test anyway.  That's all we need to
take Robbins's bound on factorial and implement a conservatively
rounded upper bound for \\(\log {n \choose s}\\).

{% codeblock robbins-log-choose lang:lisp %}
(defvar *minus-log-sqrt-2pi* -0.9189385332046727d0
  "Smallest double precision value > -log sqrt(2pi).")

(defun next (x &optional (delta 1))
  "Next double float."
  (bits-double-float (+ (double-float-bits x) delta)))

(defun prev (x &optional (delta 1))
  "Previous double float"
  (bits-double-float (- (double-float-bits x) delta)))

(defvar *log-ulp* 4
  "Assume log(x) is always off by fewer than 4 ULPs.")

(defun log-up (x)
  "Conservative upper bound on log(x)."
  (next (log x) *log-ulp*))

(defun log-down (x)
  "Conservative lower bound on log(x)."
  (prev (log x) *log-ulp*))

(defun sum-up (&rest xs)
  "Conservative upper bound on the sum of xs."
  (reduce (lambda (x y)
            (next (+ x y)))
          xs
          :initial-value 0d0))

(defun robbins-log-choose (n s)
  "Compute a conservative upper bound on log c(n, s) based on
   Robbins's bounds for k!."
  (assert (<= 0 s n))
  (assert (< n (ash 1 45))) ;; should be plenty of headroom for 53 bits.
  (when (or (= n s)
            (zerop s))
    (return-from robbins-log-choose 0d0))
  (let* ((n (float n 1d0))
         (s (float s 1d0))
         (n-s (float (- n s) 1d0))
         (l1 (next (* (+ n .5d0) (log-up n)))) ; (+ n .5d0) is exact.
         (l2 (next (- (* (+ s .5d0) (log-down s)))))
         (l3 (next (- (* (+ n-s .5d0) (log-down n-s)))))
         (r1 (next (/ (* 12d0 n)))) ; (* 12d0 n) is exact.
         (r2 (next (- (/ (1+ (* 12d0 s)))))) ; also exact.
         (r3 (next (- (/ (1+ (* 12d0 n-s)))))))
    (sum-up *minus-log-sqrt-2pi*
            (sum-up r3 r2 r1)
            (sum-up l3 l2 l1))))
{% endcodeblock %}

Let's quickly test that against an exact implementation with
`computable-reals` and a brute force factorial.

    CL-USER> (require 'alexandria)
    NIL
    CL-USER> (defun cr-log-choose (n s)
               (computable-reals:-r
                (computable-reals:log-r (alexandria:factorial n))
                (computable-reals:log-r (alexandria:factorial s))
                (computable-reals:log-r (alexandria:factorial (- n s)))))
    CR-LOG-CHOOSE
    CL-USER> (computable-reals:-r (rational (robbins-log-choose 10 5))
                                  (cr-log-choose 10 5))
    +0.00050526703375914436...
    CL-USER> (computable-reals:-r (rational (robbins-log-choose 1000 500))
                                  (cr-log-choose 1000 500))
    +0.00000005551513197557...
    CL-USER> (computable-reals:-r (rational (robbins-log-choose 1000 5))
                                  (cr-log-choose 1000 5))
    +0.00025125559085509706...

That's not obviously broken, and the error is pretty small.  The only
thing left is to implement the CSM test in the log domain;

\\[\log\left[{n \choose s} \alpha\sp{s} (1-\alpha)\sp{n - s}\right] < \log\frac{\varepsilon}{n + 1}\\]

becomes

\\[\log(n + 1) + \log {n \choose s} + s \log\alpha + (n - s)\log(1 - \alpha) < \\log \varepsilon.\\]

{% codeblock confidence sequence method lang:lisp %}
(defun csm (n alpha s eps)
  "Given n trials and s sucesses, are we reasonably sure that the
  success rate is *not* alpha (with a false positive rate < eps)?
  
  Answer that question with Ding, Gandy, and Hahn's confidence
  sequence method (CSM). The second return value is an estimate of the
  false positive target rate we would need to stop here.  This value
  should only be used for reporting; the target rate eps should always
  be fixed before starting the experiment."
  (assert (<= 0 s n (1- (ash 1 45))))
  (assert (< 0d0 alpha 1d0))
  (assert (> eps 0d0))
  (let* ((n (float n 1d0))
         (alpha (float alpha 1d0))
         (s (float s 1d0))
         (eps (float eps 1d0))
         (log-level (sum-up (log-up (1+ n))
                            (robbins-log-choose n s)
                            (next (* s (log-up alpha)))
                            (next (* (- n s) (log-up (- 1d0 alpha)))))))
    (values (< log-level (log-down eps))
            (next (exp log-level) *log-ulp*))))
{% endcodeblock %}

The [full SBCL implementation is on github](), as well as a
[C99 implementation](), and a [Python implementation]().  They are all
available under the 3-clause BSD license.

How to use the CSM
------------------

Let's start with a synthetic test case: we'll generate successes 99.5%
of the time, and test whether the success rate is more than 99% or
less than 99.95%.

{% codeblock csm driver lang:lisp %}
(defun csm-driver (generator alpha eps &optional max-count)
  (loop
     with s = 0
     for i upfrom 1 do
       (incf s (if (funcall generator (1- i)) 1 0))
       (multiple-value-bind (stop level)
           (csm i alpha s eps)
         (when (or stop
                   (and max-count
                        (> i max-count)))
           (return (values stop (float (/ s i) 1d0))))
         (when (or (<= i 10)
                   (and (<= i 100)
                        (zerop (mod i 100)))
                   (zerop (mod i 1000)))
           (format t "~A ~A ~A~%" i (float (/ s i) 1d0) level)))))
{% endcodeblock %}

    CL-USER> (csm-driver (lambda (i)
	                       (declare (ignore i))
                           (< (random 1d0) 0.995))
                         0.99 1d-5)
    1 1.0d0 1.980000019073489d0
    2 1.0d0 2.940300056648262d0
    3 1.0d0 3.881196112163553d0
    4 1.0d0 4.802980235069862d0
    5 1.0d0 5.705940574228742d0
    6 1.0d0 6.590361426719634d0
    7 1.0d0 7.4565232860606105d0
    8 1.0d0 8.304702889849827d0
    9 1.0d0 9.135173266834606d0
    10 1.0d0 9.948203783414664d0
    100 0.99d0 37.497578782087075d0
    1000 0.994d0 62.811242025207704d0
    2000 0.996d0 2.5306713308957063d0
    3000 0.996d0 0.29566946029567487d0
    4000 0.99575d0 0.07697210706532162d0
    5000 0.9958d0 0.008288394263355908d0
    6000 0.9958333333333333d0 8.708071016212414d-4
    7000 0.9957142857142857d0 2.1122303933505983d-4
    T
    0.9959645925540224d0
    CL-USER> (csm-driver (lambda (i)
	                       (declare (ignore i))
                           (< (random 1d0) 0.995))
                         0.999 1d-5)
    1 1.0d0 1.9980000257492092d0
    2 1.0d0 2.99400307717038d0
    3 1.0d0 3.988012150186414d0
    4 1.0d0 4.980030236725378d0
    5 1.0d0 5.9700603247254795d0
    6 1.0d0 6.958105398140063d0
    7 1.0d0 7.944168436942608d0
    8 1.0d0 8.928252417131645d0
    9 1.0d0 9.910360310735804d0
    10 1.0d0 10.890495085818728d0
    100 0.98d0 0.45385419613795464d0
    1000 0.991d0 9.871457014693956d-4
    2000 0.9935d0 3.4670855404987606d-4
    T
    0.9927404718693285d0

We find that, although the estimated success probabilities (\\(0.99596\\) and
\\(0.99274\\)) are far from the known value, \\(0.995\\), they are
on the correct side of the threshold: \\(0.99596 > 0.99\\), just like
\\(0.995 > 0.99\\), and \\(0.99274 < 0.999\\), just like \\(0.995 < 0.999\\).

We can also test whether the success probability differs from
\\(0.995\\), for one million iterations (many more than the 7000 and
2000 needed earlier).

    CL-USER> (csm-driver (lambda (i)
	                       (declare (ignore i))
                           (< (random 1d0) 0.995))
                         0.995 1d-5 1000000)
    1 1.0d0 1.9900000095367458d0
    2 1.0d0 2.9700750284671855d0
    ...
	999000 0.9949629629629629d0 4909.220461659301d0
    1000000 0.994965d0 4984.715723217456d0
    NIL
    0.994965005034995d0

Even after a million tests, we still don't have a significant
difference: remember, the odds of false positives are *for each
unbounded stream of random values*, not merely per iteration.  In fact, we
can see that, as we run more experiments, the "inconfidence level"
increases.  In other words, the stopping criterion is more strongly
violated as we add more data… so we are more sure that we don't know
whether the success probability differs from \\(\alpha = 0.995\\) (:

I also re-ran the first two synthetic tests 1000 times, with
\\(\varepsilon = 0.05\\) and 10000 iterations.

The first test (\\(p \neq \alpha = 0.99\\)) found a statistically
significant result in 955 cases (95.5% of the time), and, of those 955
cases, only 4 (0.4%) were misclassified, with an estimated success
probability \\(\hat{p} = \frac{s}{n} \leq 0.99\\).  The false positive
rate, 0.4%, is much lower than \\(\varepsilon = 0.05\\); that's
typical of CSM, which is extra conservative when \\(\alpha\\) is close
to 0 or 1.

The second test (\\(\hat{p} < 0.999\\)) found a statistically
significant result in all 1000 cases, and classified them all
correctly.  There is some asymmetry in the stopping intervals, so the
test tends to be easier (more quickly converge to a correctly
significant result) when the actual probability \\(p\\) is less
extreme than the threshold \\(\alpha\\).  That's why the test for
\\(\hat{p} < 0.999\\) didn't fail even once in 1000 independent runs.

Let's use real data now.  I'll load raw data from 
[the `an_itoa` post]() and process it in Python.  Let's look at the
cycle counts for `fb_ltoa` and `an_ltoa`, when printing 3-digit
integers.  We have 10000 rows for each implementation.  I'll shuffle
the values, then use the first 1000 for exploration, and the remaining
9000 for the analysis.

    In [72]: %paste
	import random
    import re
    
    def extract_cycles(count, function):
        ret = []
        with open('itoa/one_digits_filtered.tab', 'rt') as f:
            for line in f:
                groups = re.match(r'^([0-9]+)\t([a-z_]+)\t([0-9.]+)', line)
                if not groups:
                    continue
                if int(groups.group(1)) != count or groups.group(2) != function:
                    continue
                ret.append(float(groups.group(3)))
        random.shuffle(ret)
        return ret
    ## -- End pasted text --
    
    In [73]: fb_3 = extract_cycles(3, 'fb_ltoa')
    
    In [74]: an_3 = extract_cycles(3, 'an_ltoa')
    
    In [75]: fb_3_prefix, fb_3_analysis = fb_3[:1000], fb_3[1000:]
    
    In [76]: an_3_prefix, an_3_analysis = an_3[:1000], an_3[1000:]
    
    In [77]: sum(an < fb for an, fb in zip(an_3_prefix, fb_3_prefix))
    Out[77]: 996
    
    In [78]: csm_driver((an < fb for an, fb in zip(an_3_analysis, fb_3_analysis)), 0.99, 1e-9)
    1 1.000 1.98
    2 1.000 2.9403
    3 1.000 3.8812
    4 1.000 4.80298
    5 1.000 5.70594
    6 1.000 6.59036
    7 1.000 7.45652
    8 1.000 8.3047
    9 1.000 9.13517
    10 1.000 9.9482
    20 1.000 17.176
    30 1.000 22.9307
    40 1.000 27.4278
    50 1.000 30.8553
    60 1.000 33.3766
    70 1.000 35.1335
    80 1.000 36.2494
    90 1.000 36.8306
    100 1.000 36.9693
    1000 0.999 0.438319
    2000 0.999 0.00076166
    3000 0.999 8.46993e-06
    4000 0.999 1.24426e-08
    Out[78]: (True, 0.9988355845365626)

The initial subset of a thousand values showed that `an_ltoa` was
faster than `fb_ltoa` around 99.6% of the time.  Let's hope that's enough
margin to easily find that `an_ltoa` is faster than `fb_ltoa` at least
99% of the time, even with a false positive rate of \\(10\sp{-9}\\)…
and that is indeed what we see here.

We could also have been unlucky.  When we run out of data without a
statistically significant difference, we must either keep `eps` the
same and generate more values, or we can try with a higher (more
permissive) `eps` and a new, independent, stream of data.

Generating more data can be painful.  What if we could estimate the
probability of succcessful termination, given an actual success rate
\\(p\\), a threshold \\(\alpha\\), a false positive rate
\\(\varepsilon\\), and a max iteration count \\(N\\)?  We can, by
recursively applying the CSM test!  The success or failure of
`csm_driver`, for fixed parameters, is itself a Bernoulli process.

{% codeblock cms_power lang:py %}
def csm_power(p: float, alpha: float, max_count: int,
              eps: float=1e-5, success_rate: float=0.99) -> Tuple[bool, float]:
    """
    Estimate the probability of finding statistically significant
    differences when generating i.i.d. success values with probability
    = p, comparing against alpha, with false positive rate eps, and
    generating at most max_count values.

    Stop when the estimated probability of finding a significant
    result differs from success_rate, with a false positive rate of
    1e-9.
    """
    def bernoulli() -> Iterable[bool]:
        while True:
            yield random.random() < p
    def fn() -> Iterable[Iterable[bool]]:
        while True:
            yield csm_driver(bernoulli(), alpha, eps, max_count, log=False)[0]
    return csm_driver(fn(), success_rate, 1e-9)[1]
{% endcodeblock %}

    In [79]: csm_power(0.996, 0.99, 9000, 1e-9)
    1 0.000 0.02
    2 0.000 0.0003
    3 0.333 0.00119465
    4 0.250 1.98959e-05
    5 0.200 2.98352e-07
    6 0.167 4.17635e-09
    7 0.286 1.6492e-08
    Out[79]: 0.25

If we assume that `an_ltoa` is faster than `fb_ltoa` 99.6% of the time
(our MLE given the initial prefix of 1000 values), the test we ran had
a success probablity lower than 99%.  In fact, it would have
terminated only around 25% of the time.  Either we were lucky, or
`an_ltoa` is faster than `fb_ltoa` more than 99.6% of the time.  In
practice, we should probably set our expectations for \\(\alpha\\) and
\\(\epsilon\\) with `csm_power` before running the analysis.  For
example, `csm_power(0.996, 0.95, 9000, 1e-9)` shows that we could have
run the test against \\(\alpha = 0.95\\) and reasonably expected it to
terminate successfully, if `an_ltoa` was faster than `fb_ltoa` 99.6%
of the time.

The "Exact" test with CSM
-------------------------

The confidence sequence method lets us test for the success rate of
binary properties with a dynamic bound on the number of iterations
needed to get a significant result.  That's great for classic
SLA-style statistical properties like "X happens less than Y% of the
time": X is the binary property/predicate, and Y is our probability
threshold \\(\alpha\\).

What if we actually want to compare aggregate statistics, like "the
average runtime for A is less than that for B," or "the 99th
percentile response time for A is less than that for B?"

We could use the usual statistical hypothesis tests, like a
[t-test]().  However, our data tend to be far from normal, so it's not
clear that the assumptions are satisfied enough for valid results.

Maybe we should try to abuse (parallel) computing power instead of
imposing far-fetched assumptions on our data.

That's exactly the trade-off that the marvelously named [exact test]()
offers.  When we look at our data and find that the average runtime or
the 99th percentile response time for A is 10 ms less than for B, the
next question should be something like: "what are the odds of
observing such an extreme gap, if there was no difference between A
and B?"

The better known way to estimate these odds is to assume that the
underlying distributions for the averages, percentiles, etc., are
"nice," and use a closed form estimate.  With the exact test, we can
instead generate values for the gap (or any other statistic), assuming
the null hypothesis holds, and use a binomial test to tell us if the
odds of observing such an extreme gap are lower or higher than some
threshold \\(\alpha\\).

With the same `fb_ltoa` and `an_ltoa` data sets, we observe that, on
average, `an_ltoa` is more than 54 cycles faster than `fb_ltoa` for
3-digit integers:

    In [80]: sum(fb_3) / len(fb_3)
    Out[80]: 96.9946
    
    In [81]: sum(an_3) / len(an_3)
    Out[81]: 42.8598
    
    In [82]: Out[80] - Out[81]
    Out[82]: 54.134800000000006


We can generate data for the null hypothesis by shuffling `an_3` and
`fb_3`, and assigning labels (`an_ltoa` or `fb_ltoa`) randomly, while
preserving the size of each sample (i.e., 10000 for each label).
That's enough to count the number of times we find a difference as
extreme as `54.13`, and let the CSM tell us if we can expect to find
such a difference less than, e.g., 1% of the time.

    In [83]: %paste
    def avg(x):
        return sum(x) / len(x)
    
    def gap_avg(x, y):
        return abs(avg(x) - avg(y))
    
    def resampled_stat(fn, x, y):
        combined = x + y
        random.shuffle(combined)
        random_x = combined[:len(x)]
        random_y = combined[len(x):]
        return fn(random_x, random_y)
    
    def resampled_confidence(fn, x, y, alpha=0.05, p=1e-9):
        gap = fn(x, y)
        def generate():
            while True:
                yield resampled_stat(fn, x, y) >= gap
        return csm_driver(generate(), alpha, p)[1]
    ## -- End pasted text --
    
    In [84]: resampled_confidence(gap_avg, an_3, fb_3, 0.01)
    1 0.000 1.98
    2 0.000 2.9403
    3 0.000 3.8812
    4 0.000 4.80298
    5 0.000 5.70594
    6 0.000 6.59036
    7 0.000 7.45652
    8 0.000 8.3047
    9 0.000 9.13517
    10 0.000 9.9482
    20 0.000 17.176
    30 0.000 22.9307
    40 0.000 27.4278
    50 0.000 30.8553
    60 0.000 33.3766
    70 0.000 35.1335
    80 0.000 36.2494
    90 0.000 36.8306
    100 0.000 36.9693
    1000 0.000 0.0432144
    2000 0.000 3.72938e-06
    Out[84]: 0.0

It takes a while, but we finally find that, unless we're extremely
unlucky (or have a bad PRNG), the odds of observing a difference in
averages as extreme as 54.13 cycles would be less than 1% if the
printing algorithm (`fb_ltoa` VS `an_ltoa`) had no impact.  Unlike
traditional significance tests, once we have more than ~one hundred
values to sample from, the Exact method will *eventually* terminate
(unless the actual success probability is equal to the threshold
\\(\alpha\\)); however, termination could take a very long time.

We can use similar logic to determine if we can be confident that the
average runtime for `an_ltoa` is at least 30 cycles better than that
for `fb_ltoa`.

    In [85]: %paste
    def resampled_predicate(pred, x, y, alpha=0.05, p=1e-9):
        assert pred(x, y)
        def generate():
            while True:
                yield resampled_stat(pred, x, y)
        return csm_driver(generate(), alpha, p)[1]
    ## -- End pasted text --
    
    In [86]: resampled_predicate(lambda x, y: avg(x) - avg(y) > 30, fb_3, an_3, 0.01)
    1 0.000 1.98
    2 0.000 2.9403
    3 0.000 3.8812
    4 0.000 4.80298
    5 0.000 5.70594
    6 0.000 6.59036
    7 0.000 7.45652
    8 0.000 8.3047
    9 0.000 9.13517
    10 0.000 9.9482
    20 0.000 17.176
    30 0.000 22.9307
    40 0.000 27.4278
    50 0.000 30.8553
    60 0.000 33.3766
    70 0.000 35.1335
    80 0.000 36.2494
    90 0.000 36.8306
    100 0.000 36.9693
    1000 0.000 0.0432144
    2000 0.000 3.72938e-06
    Out[86]: 0.0

Not only can we say that the difference in average runtime between
`an_ltoa` and `fb_ltoa` for 3-digit integers is unlikely to be caused
by pure luck, but we can also be confident that that difference is
greater than 30 cycles.  If the observations were paired (e.g.,
count cycles when converting the same integer to string with `an_ltoa`
and `fb_ltoa`), we could reduce the variance by randomly flipping the
label assignment for each pair, instead of blindly shuffling values.

What if we run out of data/patience?
------------------------------------

Running Monte Carlo binomial tests with the confidence sequence method
has the interesting guarantee that, unless \\(p\\), the unknown
success probability for our i.i.d. Bernoulli stream is exactly equal
to the threshold \\(\alpha\\), CSM will *eventually* have enough data
to determine that the difference between \\(\hat{p} \approx p\\) and
\\(\alpha\\) is likely significative, i.e., that the unknown success
probability \\(p\\) and \\(\hat{p}\\), our estimate of \\(p\\), are on
the same side of \\(\alpha\\).  Reaching that state will sometimes
take a long time, or require more data than we actually have.  It
would be nice to have something more to report than only \\(\hat{p} =
\frac{s}{n}\\), the MLE for \\(p\\)… along with a big warning that,
although \\(\hat{p} < \alpha\\) (for example), we're not confident
that \\(p < \alpha\\).

Updating our belief of the parameters for a distribution, given a set
of observations, is a classic Bayesian problem.  For a Binomial
distribution with unknown success parameter \\(p\\), we can update a
conjugate Beta distribution for \\(p\\).

We could start with a minimal assumption for \\(p\\): a priori assume
that any value between 0 and 1 for \\(p\\) is equally likely.  That's
Bayes's prior, with \\(\mathrm{Beta}(1, 1)\\); there are many
arguments why that's theoretically flawed, but it mostly doesn't
matter given that we expect many observations.  After \\(n\\) trials,
and \\(s\\) success, our updated belief for the value of \\(p\\)
should follow a \\(\mathrm{Beta}(1 + s, 1 + n - s)\\).  We can use
that distribution to compute confidence intervals for the value of
\\(p\\), by evaluating the inverse CDF of the Beta posterior, at
\\(\varepsilon\\) and \\(1-\varepsilon\\).

There's only one minor problem: the Beta's inverse CDF has no closed
form.  The only way to evaluate it is to use an iterative algorithm,
like [Newton's method]()… but that depends on evaluating the Beta
CDF, and that function too is annoying.

The Beta CDF, \\(I\sb{x}(a, b)\\), has three parameters (\\(a\\), \\(b\\),
\\(x\\)), and multiple methods are necessary to cover the full
parameter space.  [This technical report](http://www.dtic.mil/dtic/tr/fulltext/u2/a210118.pdf) 
detailing [Algorithm 708](https://dl.acm.org/citation.cfm?id=131776)
deploys _five_ different evaluation strategies (double that if we
count symmetries).

Hopefully we can do simpler by specialising for our use case of
computing confidence intervals.  Already, we know that the parameters
\\(a\\) and \\(b\\) are positive integers.  We can also focus on
evaluating lower bounds for the left (lower) bound of confidence
intervals: we can handle upper bounds with the symmetry 
\\(I\sb{x}(a, b) = 1 - I\sb{1 - x}(b, a)\\).  We're left with
maximising \\(x\\) such that

\\[I\sb{x}(a,b) \leq \tilde{I}\sb{x}(a,b) < \alpha < \frac{1}{2},\\]

where \\(\tilde{I}\sb{x}(a, b)\\) is an arbitrary conservative
over-approximation of \\(I\sb{x}(a, b)\\).  We can also assume
we only support \\(90\%\\) or higher confidence intervals, and thus
\\(\alpha < 0.05\\).

If we locate \\(x\\) with [Newton's]() (or the [secant]()) method, we get
superlinear convergence, but need accurate evaluations of
\\(\tilde{I}\\).  The [bisection method]() only needs to know if
\\(\tilde{I}\sb{x}(a, b) < \alpha\\) or 
\\(\tilde{I}\sb{x}(a, b) > \alpha\\), and still guarantees a
respectable linear rate of convergence.

Given all we know about the confidence interval workload, we can use
an approach that is usually pointless, the 
[second hypergeometric representation listed in the DLMF](http://dlmf.nist.gov/8.17#ii):

\\[I\sb{x}(a, b) = 
\frac{\Gamma(a + b)}{\Gamma(a)\Gamma(b)}\frac{x\sp{a}(1-x)\sp{b}}{a}F(a + b, 1; a + 1; x) =
{ {a + b - 1} \choose a}x\sp{a}(1 - x)\sp{b}
  \sum\sb{s=0}\sp{\infty}\frac{(a+b)\sb{s}}{(a + 1)\sb{s}}x\sp{s},\\]
where 

\\[(a+b)\sb{s} = \prod\sb{i=1}\sp{s} a + b + i - 1,\\]

i.e., \\((a + b)\sb{0} = 0\\), and

\\[(a+1)\sb{s} = \prod\sb{i=1}\sp{s} a + i.\\]

We can reformulate the summands 

\\[\frac{(a + b)\sb{s}}{(a + 1)\sb{s}}x\sp{s} = 
\prod\sb{i=1}\sp{s} \frac{a + b + i - 1}{a + i}x =
\prod\sb{i=1}\sp{s} \left(1 + \frac{b - 1}{a + i}\right)x
\equiv \prod\sb{i=1}\sp{s} \pi\sb{i}(a, b, x)\\]

to see that, in the limit,

\\[\lim\sb{i\rightarrow\infty} \left(1 + \frac{b - 1}{a + i}\right)x = x,\\]

monotonically and from above.  Since \\(x < 1,\\) we eventually get
something that decreases like a [geometric series]().  We can further
impose \\(x < \frac{a}{a + b} = \hat{p}\\) (we are looking for
\\(\alpha < 0.5\\)), and guarantee that
\\(\forall i\geq 1,\,\pi\sb{i}(a, b, x) < 1.\\)

When \\(x \approx \frac{a}{a + b},\\) the convergence rate could still
be very slow for the first \\(b\\) (i.e., potentially millions)
iterations.  Luckily, we don't care.  We only need to know if
\\(\tilde{I}\sb{x}(a,b) < \alpha\\), or 
\\(\tilde{I}\sb{x}(a, b) > \alpha,\\) with 
\\(\alpha < 0.05.\\) If 
\\(x \approx \frac{a}{a + b},\\)
\\(I\sb{x}(a,b)\approx 0.5 > \alpha,\\) and the partial
sums should quickly grow past \\(\alpha.\\) Moreover, if \\(b\\) is
much larger than \\(a\\), \\(\frac{a}{a + b}\\) is small,
and \\(x < \frac{a}{a + b}\\).

In fact, we can do even better and tighten upper and lower bounds on
the series as we evaluate more terms.  The multiplicands
\\(\pi\sb{i}(a, b, x)\\) converge to \\(x\\) monotonically and from 
above.  We thus have, for any \\(j\in\mathbb{N},\\)
\\[0\leq
\sum\sb{s=j}\sp{\infty} \prod\sb{i=1}\sp{j}\pi\sb{i}(a, b, x) \prod\sb{i=j}\sp{s}x \leq
\sum\sb{s=j}\sp{\infty} \prod\sb{i=1}\sp{s} \pi\sb{i}(a, b, x) \leq
\sum\sb{s=j}\sp{\infty} \prod\sb{i=1}\sp{j}\pi\sb{i}(a, b, x)
\prod\sb{i=j}\sp{s}\pi\sb{j}(a, b, x).\\]

We can evaluate the [geometric series]()' limits and find
\\[0\leq
\frac{\prod\sb{i=1}\sp{j}\pi\sb{i}(a, b, x)}{1 - x} \leq
\sum\sb{s=j}\sp{\infty} \prod\sb{i=1}\sp{s} \pi\sb{i}(a, b, x) \leq
\frac{\prod\sb{i=1}\sp{j}\pi\sb{i}(a, b, x)}{1-\pi\sb{j}(a, b, x)}.\\]

This gives us one quick test to determine when
\\(\tilde{I}\sb{x}(a,b)>\alpha\\): simply compare the partial sum
against \\(\alpha\\), and exit if the former is already greater than
the threshold \\(\alpha\\).  If that doesn't work, we can evaluate the
two limits and check if either the lower bound is greater than
\\(\alpha\\), or the upper bound less than \\(\alpha\\).  The latter
two tests are slower, so I wouldn't execute them on every iteration.
The tests may also be slightly incorrect due to floating point
rounding, so I only let them trigger when the bounds are far away from
\\(\alpha\\); for example, if
\\(\frac{\prod\sb{i=1}\sp{j}\pi\sb{i}(a, b, x)}{1 - x} > 2\left[\alpha
-\sum\sb{s=1}\sp{j} \prod\sb{i=1}\sp{s} \pi\sb{i}(a, b, x) \right],\\)
when the lower bound clears twice the gap to \\(\alpha\\).

Our algorithm to approximate the incomplete beta function
\\(I\sb{x}(a,b)\\) multiplies a bunch of small values, so it seems
natural to work in the log domain.  However, we then *add* the
products together.  If we used the log domain, it would only be for
\\(\prod\sb{i=1}\sp{s}\pi\sb{i}(a, b, x).\\) This could be useful if
we sometimes had very small terms (that would round to 0 if it weren't
for the \\(\log\\)) that are eventually multiplied back to large
values.  That's not what happens here: the partial products are
monotonically decreasing.  If a partial product ever rounds off into
nothingness, that will also be the case for every further product.

We should thus use the log domain to compute the initial product

\\[\exp\left[\log{ {a + b - 1} \choose a } + a\log x + b\log(1 - x)\right],\\]

and then work with normal additions, multiplications, and divisions
(modulo forcibly rounding temporary values up).  We also took
advantage of the fact that we know we're looking for a lower
confidence bound to let us evaluate an arbitrary
\\(\tilde{I}\sb{x}(a,b) > I\sb{x}(a,b)\\).  We'll use that freedom to
approximate \\(\log{ {a + b - 1} \choose a }\\) in the initial product
with the same conservative Robbins bound we used for the CSM
criterion.

{% codeblock inverse Beta CDF lang:lisp %}
(defun %iter-beta (a b x threshold &optional limit)
  "Iteratively evaluate a hypergeometric representation for the
   regularised incomplete Beta."
  (let* ((log-initial (sum-up (robbins-log-choose (+ a b -1) a)
                              (* a (log-up x))
                              (* b (log-up (- 1 x)))))
         (product (next (exp log-initial) *log-ulp*))
         (acc product)
         (limit (or limit (+ a b 1000))))
    (loop for i from 1 upto limit do
         (let ((multiplicand
                (next (* x (sum-up 1
                                   (next (/ (float (1- b) 1d0)
                                            (+ a i 1))))))))
           (assert (< multiplicand 1))
           (setf product (next (* product multiplicand))
                 acc (sum-up acc product))
           (when (>= acc threshold)
             (return acc))
           (when (or (zerop (mod i 128))
                     (= i limit))
             ;; Compute upper and lower bounds for the total
             ;; contribution of the remaining summands.  If either is
             ;; away from the threshold, return immediately.
             (let ((tail-hi (/ product (- 1 multiplicand)))
                   (tail-lo (/ product (- 1 x)))
                   (delta (- threshold acc)))
               (when (> tail-lo (* 2 delta))
                 (return (+ acc tail-lo)))
               (when (< (* 2 tail-hi) delta)
                 (return acc))))))))

(defun %beta-icdf-lo (a b alpha)
  "Find x s.t. I_x(a, b) ~ alpha < 0.5.  Assume x in (0, a / (a + b)].
   We also know that our algorithms converge much faster for small x,
   so start a reverse exponential search there."
  (let* ((alpha (float alpha 1d0))
         (p (float (/ a (+ a b)) 1d0))
         (lo 0d0)
         (hi p))
    (when (zerop alpha)
      (return-from %beta-icdf-lo 0d0))
    (flet ((update (x)
             (assert (< 0 x 1))
             (let* ((limit (and (< hi (+ lo (* lo 1d-3)))
                                1000))
                    (px (%iter-beta a b x alpha limit)))
               (cond ((not px)
                      (return-from %beta-icdf-lo (values lo hi)))
                     ((< px alpha)
                      (setf lo x))
                     (t
                      (setf hi x))))))
      (loop for i upfrom 1
         while (and (> hi (+ lo (* lo 1d-10)))
                    (> hi 1d-10))
         do (update (* .5 (+ hi lo)))
         finally (return (values lo hi))))))

(defun beta-icdf (a b alpha)
  (assert (or (<= alpha 0.05d0) (<= 0.95d0 alpha)))
  (if (< alpha 0.5)
      (multiple-value-bind (lo hi)
          (%beta-icdf-lo a b alpha)
        (values lo (- hi lo)))
      (multiple-value-bind (lo hi)
          (%beta-icdf-lo b a (- 1 alpha))
        (values (- 1 lo) (- hi lo)))))
{% endcodeblock %}

That's finally enough to also report approximative confidence
intervals on the success probability \\(p\\), even when the CSM
criterion is not satisfied.

{% codeblock beta_ci lang:py %}
def beta_ci(n: int, s: int, eps: float=1e-5) -> Tuple[float, float, float]:
    """
    Return the maximum likelihood estimate for the success probability
    p, given n trials and s successes, as well as an eps-level
    confidence interval for p.
    """
    assert 0 <= s <= n < (1 << 45)
    assert eps > 0
    mle = 1.0 * s / n
    if eps >= 0.5:
        return mle, mle, mle
    a = s + 1
    b = n - s + 1
    return 1.0 * s / n, beta_icdf(a, b, eps), beta_icdf(a, b, 1 - eps)


def csm_driver(generator: Iterable[bool],
               alpha: float,
               eps: float=1e-5,
               max_count: Optional[int]=None,
               log: bool=True) -> Tuple[bool, float, float, float]:
    """
    Extract up to max_count (default to unbounded) success values from
    generator. Stop with success when the success rate likely differs
    from alpha (the procedure will have a false positive rate < eps),
    and with failure if max_count iterations is reached.

    The second return value is the maximum likelihood estimate for the
    success rate of generator.  The third and fourth value are conservative 
    bounds for an (1 - 2eps)-level confidence interval on the success rate.
    """
    s = 0
    n = 0
    for success in generator:
        s += 1 if success else 0
        n += 1
        stop, level = csm(n, alpha, s, eps)
        if stop or (max_count is not None and n > max_count):
            return (stop,) + beta_ci(n, s, eps)
        if n <= 10 or (n <= 100 and n % 10 == 0) or n % 1000 == 0:
            if log:
                print('%d %.3f %g' % (n, 1.0 * s / n, level))
    return (False,) + beta_ci(n, s, eps)
{% endcodeblock %}

The CSM driver routine is now able to report a confidence interval on
\\(p\\), whether it successfully finds a significant difference from
\\(\alpha\\) or not.

    In [87]: csm_driver((an < fb for an, fb in zip(an_3_analysis, fb_3_analysis)), 0.99, 1e-9)
    1 1.000 1.98
    ...
    3000 0.999 8.46993e-06
    4000 0.999 1.24426e-08
    Out[87]: (True, 0.9988355845365626, 0.9921857020817697, 0.9999776408076286)

This tells us that we can (probably) conclude that `an_ltoa` is faster
than `fb_ltoa` in at least 99% of cases for 3-digit integers, that the
actual percentage of cases is roughly around 99.88%, *and* that we can
probably assume that the actual percentage is between 99.22% and
99.998%.  The confidence interval for the Beta would likely have let
us use fewer data points; however, using that threshold after every
observation increases the risk of false positive.  It could still make
sense to use the confidence interval once we reach the iteration limit
(the CSM is conservative because it assumes an infinite horizon), but
I'm not sure quite sure how to deal with multiple hypothesis testing.

Brute force sounds great, how do I use this?
--------------------------------------------

The first step is to take the question we really want to answer (is
this service fast enough? is the new code faster?) and rephrase it as
a number of SLA-type properties: « at least `x`% of `$SOMETHING` are
"good enough", » or « at most `x`% of `$SOMETHING` are "very bad". »

More formally, we want:

Given \\(x\\) sampled independently and identically from some
distribution \\(X\\), \\(\mathrm{pred}(x)\\) is true with probability
less/more than \\(\alpha\\).

If that statement is true, then, in the long run, we should satisfy
the SLA "\\(100 \alpha\%\\) of \\(x\in X\\) satisfy
\\(\mathrm{pred}(x)\\)," e.g., "99% of service requests (that look
like our test data) return a response in fewer than 20 milliseconds."

Sometimes, especially for internal metrics, we're interested in
aggregate statistics, e.g., "does the new release improve or worsen
our average/median/p99?"

If we are able to generate a large number of measurements, we can
split up measurement in batches, and compute and compare statistics
for each batch, and test SLAs over batches ("For 99% of batches of 1M
requests, the new release makes at least 20% fewer full table scans.").

Otherwise, we can use the [exact test]().  Let's say we have a fixed
set of measurements under two conditions (old/new release), and we
want to know if the difference in average or 99th percentile is
statistically significant: what would be the odds of observing as
extreme a difference if the conditions were identical (A/A testing)?

We can estimate these odds with an SLA type property: "Given random
labelings of the data, at most 1% of the labelings yield a difference
in `p99(old) - p99(new) > delta`," where `delta` is the actual
observed difference between the old and new release.  We can even test
for a meaningful difference by biasing one of the values after
shuffling: `p99(old) - p99(new) + min_delta > delta` would help us
determine if the difference we observed is significatively greater
than `min_delta`.

A generic way to randomise labels is to shuffle the concatenation of
both data sets, and split the result in two parts, *while preserving
sample sizes*. For example, if we have 100 observations for the new
release and 1000 for the old one, we want to keep these sample sizes
of 100 and 1000 when splitting shuffled data as well.

When observations are paired (e.g., runtimes for two implementations,
given the same input for each pair), we can randomise the labels by
flipping the assignment with probability 0.5.  If the values we're
comparing (runtimes) are a function of the label (implementations A
and B) *and of the input data*, this form of randomisation reduces
variance and should terminate faster (if that's not the case, it
doesn't hurt, but might need more data pairs to have enough random
labelings).  By only flipping labels for individual pairs, we make
sure that each label gets the same number of "easy" and "hard" inputs.

There is one important property on the predicate \\(\mathrm{pred}\\)
(e.g., "returns a response in fewer than 20 milliseconds") that makes
all the probabilities work: its probability of returning true must be
the same at each iteration, the truth values we generate must be
indepedent and identically distributed.  Usually, the sources of
stochasticity in the result are the input, and, sometimes,
environmental noise.  We must always find a way to make sure the
latter is reasonably constant (independent and identically
distributed), otherwise it's hard to conclude anything (running paired
measurements in parallel might help).  Assuming noise is handled, the
easiest way to make the truth values i.i.d. is to make sure the input
is i.i.d.

Some test harnesses don't lend themselves well to that (e.g., we might
generate inputs from different distributions sequentially).  One way
to fix that is to pick one random distribution uniformly before
generate each input.  If that's not possible or desirable, we should
find a way to have comparable "epochs" (samples of data from the same
underlying phenomenon), e.g., each sequential run through the set of
input distributions.  Only testing for the CSM criterion at the end of
each epoch is close enough to an uniform selection.

That's not always possible; for example, if we replay production logs,
requests from Friday 9PM might not look like requests from Sunday
10AM.  We could reshuffle the data to mix all the hours.  If that's
not desirable, we have to run separate statistical tests for each
dataset (each hour), and check that we probably satisfy the SLA for
each dataset (hour of production logs).  Alternatively, we can
formalise an SLA-type property for each dataset in aggregate (e.g.,
"At least 99% of requests in an hour are processed successfully in
99.9% of hours that look like historical datasets."), and
select/generate epochs of data independently.

Finally, there's the question of termination.  In theory, the `CSM`
test will (with probability 1) terminate, as long as the real, but
unknown, success probability \\(p\\) differs from the threshold
\\(\alpha\\).  In practice, this might still take a long time if
\\(p\\) is very close to \\(\alpha\\).

However, in practice, the thresholds are usually arbitrary, and we'd
rather do a bit better than be exactly on the line.  If my internal
goal is that a service must respond in fewer than 10ms, 99% of the
time, I'd probably feel more comfortable if my testing showed it
achieved a 10ms response 99.5% of the time.

We can use that to improve the convergence rate of the `CSM` tests by
comparing against 2 limits at once, at a slight distance from each
other.  At least one of the limits should terminate quickly, and we'll
get one of four results:

1. We're probably doing somewhat better than our goal (\\(p > 99.5\% > 99 \%\\)).
2. We're probably not doing much better than our goal (\\(p < 99.5\%\\)).
3. We're probably meeting our goal (\\(p > 99\%\\)).
4. We're probably not meeting our goal (\\(p< 99\%\\)).

Results 1 and 4 are easy to interpret.  Ideally, we would treat 2, and
perhaps 3, like 4, but that's not always possible.  When a test run
returns either 2 or 3, it might be helpful to let it go through a
couple more iterations (e.g., 1% more) and hope that we'll find that
*both* 2 and 3 probably hold.

There is one important detail: if we test two hypotheses, we must
correct for the multiple testing problem.  We can just apply a
[Bonferroni correction](), and test with false positive rate
\\(\varepsilon/2\\) for each hypothesis.

With the 2-limits trick, I feel comfortable running CSM tests
automatically, e.g., as part of an continuous integration pipeline.
We can now more easily estimate the [power]() of a given testing
setup: if we run a test for at most 10000 iterations, what are the
odds that CSM will stop for neither limit?  Given a pair of limits
\\((\alpha\sb{l}, \alpha\sb{h}),\\) \\(\alpha\sb{l} < \alpha\sb{h},\\)
the worst case is with \\(p = \frac{\alpha\sb{l} + \alpha\sb{h}}{2}.\\)

That's all we need to run `csm_power(p, alpha_l, 10000, eps=5e-7, success_rate=0.99)`
and estimate the probability of succcessfully terminating away from
\\(\alpha_l\\) after 10000 iterations and a false positive rate of
\\(10\sp{-6}/2\\); if the estimate is more than `success_rate`, we can
be confident that we'll get one of results above at least
`success_rate` of the time (i.e., 99% here).  We can also run the same
thing against `alpha_h`, and use the maximum of 
`csm_power(p, alpha_l, ...)` and `csm_power(p, alpha_h, ...)` as a
conservative estimate on the probability of successful termination
away from either limit after our fixed number of iterations and false
positive rate.

The correct values for the limits \\(\alpha\sb{l}\\) and
\\(\alpha\sb{h},\\) for the number of iterations (`max_count`), and
for the false positive rate \\(\varepsilon\\) (`eps`) in order to
achieve a given success rate are up to our judgment.  Moving the
limits \\(\alpha\\) further away from each other (which also affects
the \\(p = \frac{\alpha\sb{l} + \alpha\sb{h}}{2}\\) we use to estimate
power) will make success more likely.  Increasing the
number of iterations or the false positive rate will also improve the
success rate.  I expect the values for \\(\alpha\\) will usually be
fixed after heated discussions, and the number of iterations set to the
maximum reasonable runtime.  The only remaining tunable knob is to
balance increases in the the false positive rate \\(\varepsilon\\) and
the probability of terminating without any statistically significant
result.  If none of the options are reasonable, we just have to get
everyone to dampen their expectations and accept either slower tests
*in the worst case*, or coarser limits.

The tools I wrote about in this blog post should suffice to test
statistical properties (either SLA-type, or transformed to SLAese) and
consistently generate actionable results.  The power estimate should
also make it possible to run these tests automatically without
annoying teammates.  It's not the only approach, but I like that it's
reasonable to code from scratch, that the results are easy to
interpret, and, most importantly, that it can stop tests early for
fast turnaround times.

I also hope that I managed to convey what the CSM's dynamic stopping
criterion does, and how to implement it on computers.  The 
[basic code, along with additional tools, is available on github](), 
under the Apache 2.0 license.  Come help me make SLAs easier to test!
