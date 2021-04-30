---
layout: post
title: "Convex relaxation with experts: a decennial checkup"
date: 2021-04-16 15:15:42 -0400
draft: true
hidden: true
comments: true
categories: 
---

Sometime in 2011,[^reciprocal] I really had to take a break from
Lagrangian relaxations for combinatorial problems and read
[Arora, Hazan, and Kale's survey on the multiplicative weight update (MWU) algorithm](https://www.cs.princeton.edu/~arora/pubs/MWsurvey.pdf).
After tangling with the heavily theoretical presentation, I saw
that the the tight, non-asymptotic, convergence bounds guaranteed by
MWU (and other state-of-the-art algorithms for the experts problem)
might be a good way to address a practical obstacle for Lagrangian
relaxations: "pricing" algorithms (which tell us the relative
importance of relaxed constraints) are sensitive to parameters, and
the more robust ones tend to scale badly to larger instances.

[^reciprocal]: I'm pretty sure that was the reason behind [this post on double float reciprocals](https://pvk.ca/Blog/LowLevel/software-reciprocal.html).

Unfortunately for me, theoreticians treat optimisation as a trivial
extension of satisfaction (just add a linear constraint on the
objective value and binary/galloping search), which is fine for coarse
complexity classes like \\(\mathcal{P}\\), but not really for useful
metrics like wall-clock time... and I was already focusing on getting
out of graduate school, ideally but not necessarily with a doctorate.
TL;DR: I was pretty sure we could make it useful, but I couldn't
figure out how, and I wasn't in the mood to spend too much time on
this high-risk angle.

I eventually defended something, found a job in industry, and happily
stopped thinking about mathematical optimisation for a couple years...
but eventually this unresolved business came back to haunt me, and
I now believe we can build on top of the experts problem to fix
multiple practical issues with Lagrangian relaxations outside academia:

1. We can not only generate a relaxation bound, but also a nearly feasible and super-optimal fractional solution, or a certificate of infeasibility.
2. The [AdaHedge](https://papers.nips.cc/paper/2011/file/64223ccf70bbb65a3a4aceac37e21016-Paper.pdf) algorithm comes with worst-case convergence bounds and *no* parameter to tune.
3. We don't need subgradients, so don't have to solve subproblems to optimality, and can instead adaptively detect when we must work harder (or prove infeasibility).

The first property is important when embedding the convex relaxation
in a search program.  Fractional solutions help guide branch-and-bound
methods, but they're also an important input for cut separation
algorithms (e.g., in branch-and-cut).  Explicitly detecting
infeasibility is also helpful when we don't necessarily have a primal
feasible solution, even when we assume there is one of unknown quality.

Worst-case bounds mean that we can guarantee progress in the innermost
relaxation, instead of relying on a graduate student to stare at logs
and artfully tweak parameters.  Ideally practitioners don't have to
tweak any parameter, and the best way to achieve that is with a method
that doesn't any parameter.  Sharp convergence guarantees also makes
it easier to debug implementations: as soon as we fail to match the
worst-case (non-asymptotic) convergence rate, something is definitely
wrong.  That's much more easily actionable than slow convergence in
regular (sub)gradient methods: while that's often a sign of incorrect
gradient computation or of a bug in the descent algorithm, we also
expect these methods to sometimes converge very slowly in the short
term, or even to temporarily worsen the solution quality, even when
all the code is actually fine.

Avoiding a full solve to optimality for each subproblem was the main
driver for the formulations I describe in my dissertation: useful
(stronger than the linear relaxation) Lagrangian relaxations break
discrete optimisation problems into simpler, but usually still
\\(\mathcal{NP}\\)-hard, ones.  In practice, subproblems don't vary
that much from one iteration to the next, and so the optimal solutions to these
subproblems also tend to be stable... but proving optimality is still
hard.  For example, when I applied a MIP solver to my subproblems I
would often find that the optimal solutions in consecutive iterations
had the same values for integer decision variables; I could have
simply solved a linear program for the remaining continuous decision
variables, except that I would then have no proof of optimality, and
accidentally feeding a suboptimal solution to the outer Lagrangian
method would cause all sorts of badness.

Finally, I believe robust Lagrangian relaxation solvers are important
because most programmers (most people) don’t have any intuition for
the quality of integer linear formulations,[^hard-means-good] but
programmers tend to be very good at writing code to solve small
discrete optimisation problems, especially when heuristics are
acceptable… and Lagrangian relaxation, Lagrangian decomposition in
particular, reduces large integrated discrete optimisation problems to
a lot of smaller independent ones.

There are also two more additional points that I don't know how
to explain better than these bullet points:

1. [AdaNormalHedge](http://proceedings.mlr.press/v40/Luo15.pdf) adds support for warm starts and generating constraint/variable on demand
2. The iteration complexity for both algorithms above scales logarithmatically with the number of decision variables \(n\), and both only need \\mathcal{O}(n)\) spa
ce, and time per iteration.

[^hard-means-good]: ILPs are often compared to assembly language, and it seems newcomers to both tools fall prey to the same fallacy: it works, and it was hard to write, so it must be good ;)

The above probably only makes sense if you’re familiar with both
online learning and Lagrangian decomposition.  If that’s you, you
might enjoy <a href="https://www.authorea.com/users/2541/articles/324048-no-regret-surrogate-decomposition">this draft</a>.
These are two topics from disjoint research communities that aren’t
explored at the undergraduate level, so, for almost everyone else,
let’s see if I can provide the necessary background here!

The plan:

1. The experts problem: what is it, why is it even solvable?
2. A classic reduction from linear feasibility to the experts problem
3. Surrogate relaxation, a.k.a. I can’t believe it’s not Lagrangian relaxation<
4. Lagrangian decomposition, a relaxation method for arbitrary finite domain problems
5. A reduction from surrogate decomposition to the experts problem<

<p><hr style="width: 50%" /></p>
