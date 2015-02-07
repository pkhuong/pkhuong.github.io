---
layout: post
title: "Computational complexity, Linear Programming, and economics (part 1)"
date: 2012-12-30 12:32
comments: true
published: false
categories: 
---

[Philip Maymin](http://philipmaymin.com/)'s
[Markets are efficient if and only if P = NP [PDF]](http://iospress.metapress.com/content/0wh736n18j682433/fulltext.pdf)
made a tiny splash in my circle of friends, mostly as a "heh, and some
people think markets are efficient" kind of thing.  Perhaps because of
confirmation bias, but, mainly, I hope, because it doesn't actually
attempt to settle \\( \mathsf{P} \stackrel{?}{=} \mathsf{NP}\\), none
of us read much more than the abstract. I recently did, and found the
argument far from convincing.

The equivalence between \\( \mathsf{P} = \mathsf{NP} \\) and market
efficiency is shown with a pair of implications, one from an \\( \mathsf{NP} \\) oracle
to tractable efficient markets, and another that solves an \\( \mathsf{NP} \\)-complete
problem in polynomial time given an efficient market.

I believe most people who have been exposed to computational complexity
have little trouble accepting that, for a given optimisation
problem (in this case, devising trading strategies), the question of
whether there exists a solution that is at least as good as some
externally-provided threshold is in \\( \mathsf{NP} \\).  Out of the
nine pages, four are devoted to various such reductions, showing that
an oracle for an \\( \mathsf{NP} \\)-complete language would imply the
tractability of some form of market efficiency.

Throughout these four pages (pp. 4-7), Maymin appeals to the reader's
intuition that the market-solving problems are not only in \\( \mathsf{NP} \\), but
also difficult (i.e. not in \\( \mathsf{P}\\)).  I don't know why… If the
super-polynomiality were clear, we'd have a much stronger result: \\(
\mathsf{P} \neq \mathsf{NP} \\) .  The \\( \mathsf{NP} \\)-completeness of the problems
isn't shown either, but the wording sometimes suggests it.  For
example, when exploring the fourth question -- the feasibility of
devising some technical strategy subject to a budget constraint --
(p. 6), Maymin writes "The \[0-1\] knapsack problem 
is \\( \mathsf{NP} \\)-complete. It is also a rephrasing of our
question 4."  This implies equivalence -- Maymin later asserts that one
"is the same as" the other (p. 6) --, but only a reduction from "question 4" to
the 0-1 knapsack problem is sketched.  Given that rolling windows in a
time series are closely related, it isn't obvious that any instance of
the 0-1 knapsack problem can be reduced to this strategy-finding with
budget problem.

Surprisingly, Maymin asserts that these reductions suffice to show
equivalence, rather than one-way implication: "In short, if \\(
\mathsf{P} \neq \mathsf{NP}\\), then the market cannot be efficient for
very long… Contrariwise, if indeed \\( \mathsf{P} = \mathsf{NP}\\),
then the market will be efficient… Therefore, the market is efficient
if and only if \\( \mathsf{P} = \mathsf{NP}\\)." (p. 7) Yet, even if
the various problems were shown to be \\( \mathsf{NP}\\)-complete, that
would not suffice to conclude that efficient markets imply that
\\( \mathsf{P} = \mathsf{NP}\\). For example, it is just as impossible
to reliably extract value from an efficient market as from an
information-free market in which agents put and call randomly; does true
randomness thus imply \\( \mathsf{P} = \mathsf{NP}\\)?

In pages 4-7, the author attempts to show that some market-related
problems are \\( \mathsf{NP}\\)-complete, but only seems to show their
inclusion in \\(\mathsf{NP}\\); it's also not clear how the problems'
\\(\mathsf{NP}\\)-completeness imply the equivalence between
\\(\mathsf{P} = \mathsf{NP}\\) and efficient markets.

The paper may still be redeemed: its next section shows
how efficient markets could be exploited as powerful computational
tools.  It is shorter (pp. 7-8) and *hints* at a method to
solve
[3-SAT](http://en.wikipedia.org/wiki/Boolean_satisfiability_problem)
by letting any efficient market work its magic on a small set of orders.
Detractors of the paper ought to focus their attention here.  No one
is surprised that an \\( \mathsf{NP}\\) oracle enables cool stuff,
while this second reduction is the one that allegedly undermines the
credibility of the Efficient Market Hypothesis.  Some commentators
instead refute a strawman, saying that one can't, e.g., expect to solve a
boolean satisfiability instance by creating an option whose payoff is
based on the existence of a solution.  The method proposed in the
paper seems much more realistic, but incorrect.

The paper supposes the existence of three-way order-cancels-order
(OCO-3) orders.  Such orders are made of three Buy or Sell suborders,
at most one of which can be filled: as soon as one filled, the
remaining two suborders in the triplet are instantaneously cancelled.
No (public) markets supports OCO-3 orders yet, but we may well
suppose that a market could efficiently handle them.

The idea is that each propositional variable in a SAT instance is
mapped to a different security.  Then, given a 3-SAT instance in
[CNF](http://en.wikipedia.org/wiki/Conjunctive_normal_form), each
clause (disjunction) is converted to an OCO-3 order: a variable
becomes a Buy order for the corresponding security if it occurs
positively (not negated), and a Sell if it occurs negatively
(negated).  In either case, the price is the median of the current bid
and offer, and the minimum lot size is used.

Maymin asserts that an efficient market will extract the most profit
from such orders, and that the initial 3-SAT instance is satisfiable
iff the profit-maximising response fills all the OCO-3 orders.

The first assertion seems like a reasonable characterisation of
efficient markets.  The second, however, ought to be convincingly
argued, if not formally proven.  The paper instead completely begs the
question.

Let's not.

What happens if transaction costs completely eliminate any potential
profit?  Must we then conclude that an instance is UNSAT simply
because we traded too lightly?  I'll let The Market off and suppose
that it's so incredibly well lubricated that not only are transactions
instantaneous, but that they're also completely cost-free.

Under the hypothesis of efficiency, for each security \\( s \\),
the current expected value \\( e\sb{s} \\) (with respect to,
e.g., public information)
is known to The Market, and falls somewhere between the bid and offer.
We, not being The Market, do not know if the median price \\( m\sb{s} \\) is
below or above \\( e\sb{s} \\) (I'll be generous and suppose equality 
doesn't happen).  However, we can tell if \\( m\sb{s} > e\sb{s} \\) or
vice versa by putting an order on the exchange and watching it be
filled (or not) after a short amount of time.  Moreover, it seems
reasonable to suppose that the SAT instance provides no information to
the market, so the orders have no effect on expected values.

Obviously, this means that some orders will not be filled, even after
a long delay.  Yet, a single clause is always SAT (choose any literal,
set it to true or false depending on whether it's positive or
negative, assign arbitrary truth values to the rest).  Is there any
reason to believe than an OCO-3 order with prices set at the median,
as described above, will always be filled? I don't see why (or OCO-3
orders are most likely broken).  For example, we could, unlucky us,
generate an OCO-3 order with three buys at prices below the expected
values of the corresponding securities.

In fact, it's easy to see that the profit-maximisation task performed
by efficient markets is in \\( \mathsf{P}\\), and can even
be executed with a linear number of arithmetic operations.  The key is that the
market is looking for "some way to execute all [sic] of those *separate* OCO
orders in such a way that an overall profit is guaranteed."  (p. 8)
First, the market will execute *some* of those orders, in order to
maximise overall profit. Second, there might still be hope of a
reduction to some  \\( \mathsf{NP} \\)-complete packing problem, if it
weren't for the fact that the orders are independent; e.g., the market
filling a given OCO-3 by buying a security \\( s \\) does not mean
that all other offers for \\( s \\) must also be bought, even if
they're at the same price.

In other words, and this is crucial, maximum overall profit is
separable as (the sum of) the maximum profit for each order.  Given
the expected value for each security, the profit-maximisation task can
be solved by inspection: for each OCO-3 order, choose the sub-order
that maximises profit, or none if they're all lossy.  So, not only is
the efficient market not solving SAT, but it's actually solving a
linear-time problem (given linear-time subtractions and comparisons,
which is reasonable until exchanges trade in rationals or even in
reals [now *that* would be interesting]). I'm fairly certain that we
have some positive class separation results in that vicinity!

So, one issue is that each OCO-3 order isn't a covering constraint of
the type
\\[ x\sb{is\sb{1}} + x\sb{is\sb{2}} + x\sb{is\sb{3}} \geq 1,\\]
but rather a packing constraint
\\[x\sb{is\sb{1}} + x\sb{is\sb{2}} + x\sb{is\sb{3}} \leq 1,\\]
where \\( x\sb{is\sb{j}} \\) is the binary decision variable for whether
OCO-3 order \\(i\\) is filled by the market selling commodity
\\(s\sb{j}\\)  (sell orders are represented as
\\(1-x\sb{is\sb{j}}\\)).

However, there are packing problems that are \\( \mathsf{NP} \\)-complete as well.  The
fundamental problem is that the buy/sell decision is independent for
each order.  The only link between them is the payoff for all
\\(x\sb{is\sb{j}}\\):  \\(e\sb{s\sb{j}} - m\sb{s\sb{j}}\\), regardless
of \\( i \\).

If we further suppose that expected values \\(e\\) are adjusted to
respond to our orders (and thus minimise the profit to be made by
filling them), things look very much like a typical Lagrangian dual
problem.  Each packing constraint has distinct decision variables, but
these variables share the same price.  It's the subproblem we obtain
when applying the natural Lagrangian decomposition to a packing
problem: the decision variables (0 (false, market buys) or 1 (true,
market sells)) are duplicated for each constraint, and the weak linking
constraints \\[\sum\sb{i\in I(s)}x\sb{is} = |I(s)| x\sb{s}\\]
(where \\(I(s)\\) is the set of all constraints in which \\(s\\) occurs)
are dualised.

The only problem is that the resulting subproblem is a small LP (the
natural formulation clearly has the integrality property), so the
[Lagrangian dual is equivalent to the LP bound [PDF]](http://www.anderson.ucla.edu/faculty/art.geoffrion/home/docs/e19.pdf).  Specifically, it computes
a (dual) solution to a continuous relaxation of a packing problem.
Thus, even if we repeat the experiment so that the market adjusts its
expected values in response to those who punish us, it looks like we
can, at best, hope to solve the small LP relaxation (which is known to
be in \\( \mathsf{P}\\) since
[1979](http://en.wikipedia.org/wiki/Ellipsoid_method)) of some
packing problem.

Maymin's paper attempts to relate computational complexity and market
efficiency.  I strongly believe it's a failure.  Next up: linear
programming, Lagrangian relaxation, economic planning… and
markets.
