---
layout: post
title: "Fractional set covering with experts"
date: 2019-04-23 18:05:09 -0400
comments: true
categories: 
---

Last winter break, I played with one of the annual
[capacitated vehicle routing problem](https://en.wikipedia.org/wiki/Vehicle_routing_problem)
(CVRP) "Santa Claus" contests.  Real world family stuff
took precedence,  so, after the
obvious [LKH](http://webhotel4.ruc.dk/~keld/research/LKH-3/)
with [Concorde](http://www.math.uwaterloo.ca/tsp/concorde.html)
polishing for individual tours, I only had enough time for
one diversification moonshot.  I decided to treat the
high level problem of assembling prefabricated routes as
a [set covering problem](https://en.wikipedia.org/wiki/Set_cover_problem):
I would solve the linear programming (LP) relaxation for the
min-cost set cover, and use randomised rounding to feed new starting
points to LKH.  Add a lot of luck, and that might
just strike the right balance between solution quality and diversity.

Unsurprisingly, luck failed to show up, but I had ulterior motives:
I'm much more interested in exploring first order methods for
relaxations of combinatorial problems than in solving CVRPs.  The
routes I had accumulated after a couple days turned into a
[set covering LP with 1.1M decision variables, 10K constraints, and 20M nonzeros](https://archive.org/details/santa-cvrp-set-cover-instance).
That's maybe denser than most combinatorial LPs (the aspect ratio
is definitely atypical), but 0.2% non-zeros is in the right ballpark.

As soon as I had that fractional set cover instance, I tried to solve
it with a simplex solver.  Like any good Googler, I used [Glop](https://developers.google.com/optimization/lp/glop)… and stared at a blank terminal for more than one hour.

Having observed that lack of progress, I implemented the toy I really
wanted to try out: first order online "learning with experts"
(specifically, [AdaHedge](https://arxiv.org/abs/1301.0534)) applied to
LP *optimisation*.  I let this [not-particularly-optimised serial CL code](https://gist.github.com/pkhuong/c508849180c6cf612f7335933a88ffa6)
run on my 1.6 GHz laptop for 21 hours, at which point the first
order method had found a 4.5% infeasible solution (i.e., all the
constraints were satisfied with \\(\ldots \geq 0.955\\) instead of
\\(\ldots \geq 1\\)).  I left Glop running long after the contest was
over, and finally stopped it with no solution after more than 40 *days*
on my 2.9 GHz E5.

Given the shape of the constraint matrix, I would have loved to try an
interior point method, but all my licenses had expired, and I didn't
want to risk OOMing my workstation.  [Erling Andersen](https://twitter.com/e_d_andersen)
was later kind enough to test Mosek's interior point solver on it.
The runtime was much more reasonable: 
[10 minutes on 1 core, and 4 on 12 cores](https://twitter.com/e_d_andersen/status/1120579664806842368), with the sublinear speed-up mostly caused by the serial
crossover to a simplex basis.

At 21 hours for a naïve implementation, the "learning with experts"
first order method isn't practical yet, but also not obviously
uninteresting, so I'll write it up here.

Using online learning algorithms for the "experts problem" (e.g.,
[Freund and Schapire's Hedge algorithm](https://cseweb.ucsd.edu/~yfreund/papers/adaboost.pdf))
to solve linear programming *feasibility* is now a classic result;
[Jeremy Kun has a good explanation on his blog](https://jeremykun.com/2017/02/27/the-reasonable-effectiveness-of-the-multiplicative-weights-update-algorithm/).  What's
new here is:

1. Directly solving the optimisation problem.
2. Confirming that the parameter-free nature of [AdaHedge](https://arxiv.org/abs/1301.0534) helps.

The first item is particularly important to me because it's a simple
modification to the LP feasibility meta-algorithm, and might make the
difference between a tool that's only suitable for theoretical
analysis and a practical approach.

I'll start by reviewing the experts problem, and how LP feasibility is
usually reduced to the former problem.  After that, I'll
cast the reduction as a [surrogate relaxation](https://smartech.gatech.edu/bitstream/handle/1853/24230/karwan_mark_h_197612_phd_154133.pdf)
method, rather than a [Lagrangian relaxation](https://en.wikipedia.org/wiki/Lagrangian_relaxation);
optimisation should flow naturally from that
point of view.  Finally, I'll guess why I had more success
with [AdaHedge](https://arxiv.org/abs/1301.0534) this time than with 
[Multiplicative Weight Update](https://www.satyenkale.com/papers/mw-survey.pdf)
eight years ago.[^wall]

[^wall]: Yes, I have been banging my head against that wall for a while.

The experts problem and LP feasibility
--------------------------------------

I first heard about the experts problem while researching
dynamic sorted set data structures:
[Igal Galperin's PhD dissertation](https://dspace.mit.edu/handle/1721.1/10639)
describes [scapegoat trees](http://user.it.uu.se/~arnea/abs/partb.html), but is really about online learning with
experts.
[Arora, Hazan, and Kale's 2012 survey of multiplicative weight update methods](https://www.satyenkale.com/papers/mw-survey.pdf).
is probably a better introduction to the topic ;)

The experts problem comes in many variations.  The simplest form
sounds like the following.  Assume you're playing a binary prediction
game over a predetermined number of turns, and have access to a fixed
finite set of experts at each turn.  At the beginning of every turn,
each expert offers their binary prediction (e.g., yes it will rain
today, or it will not rain today).  You then have to make a prediction
yourself, with no additional input.  The actual result (e.g., it
didn't rain today) is revealed at the end of the turn.  In general,
you can't expect to be right more often than the best expert at the
end of the game.  Is there a strategy that bounds the "regret," how
many more wrong prediction you'll make compared to the expert(s) with
the highest number of correct predictions, and in what circumstances?

Amazingly enough, even with an omniscient adversary that has access to
your strategy and determines both the experts' predictions and the
actual result at the end of each turn, a stream of random bits (hidden
from the adversary) suffice to bound our expected regret in 
\\(\mathcal{O}(\sqrt{T}\,\lg n)\\), where \\(T\\) is the number of 
turns and \\(n\\) the number of experts.

I long had trouble with that claim: it just seems too good of a magic
trick to be true.  The key realisation for me was that we're only
comparing against invidivual experts.  If each expert is a move in a
[matrix game](https://www.encyclopediaofmath.org/index.php/Matrix_game),
that's the same as claiming you'll never do much worse than any pure
strategy.  One example of a pure strategy is always playing rock in 
Rock-Paper-Scissors; pure strategies are really bad!  The trick is
actually in making that regret bound useful.

We need a more continuous version of the experts problem for LP
feasibility.  We're still playing a turn-based game, but, this time,
instead of outputting a prediction, we get to "play" a mixture of the
experts (with non-negative weights that sum to 1).  At the beginning
of each turn, we describe what weight we'd like to give to each
experts (e.g., 60% rock, 40% paper, 0% scissors).  The cost
(equivalently, payoff) for each expert is then revealed (e.g.,
\\(\mathrm{rock} = -0.5\\), \\(\mathrm{paper} = 0.5\\), 
\\(\mathrm{scissors} = 0\\)), and we incur the weighted average
from our play (e.g., \\(60\% \cdot -0.5 + 40\% \cdot 0.5 = -0.1\\))
before playing the next round.[^equivalent]  The goal is to minimise
our worst-case regret, the additive difference between the total cost
incurred by our mixtures of experts and that of the a posteriori best single
expert.  In this case as well, online learning
algorithms guarantee regret in \\(\mathcal{O}(\sqrt{T} \, \lg n)\\)

[^equivalent]: This is equivalent to minimising expected loss with random bits, but cleans up the reduction.

This line of research is interesting because simple algorithms achieve
that bound, with explicit constant factors on the order of 1,[^which-log]
and [those bounds are known to be non-asymptotically tight for a large class of algorithms](http://drops.dagstuhl.de/opus/volltexte/2017/7499/pdf/LIPIcs-ICALP-2017-48.pdf).
Like dense linear algebra or fast Fourier transforms, where algorithms
are often compared by counting individual floating point operations,
online learning has matured into such tight bounds that worst-case
regret is routinely presented without Landau notation.  Advances improve
constant factors in the worst case, or adapt to easier inputs in order
to achieve "better than worst case" performance.

[^which-log]: When was the last time you had to worry whether that log was natural or base-2?

The [reduction below](https://jeremykun.com/2017/02/27/the-reasonable-effectiveness-of-the-multiplicative-weights-update-algorithm/)
lets us take any learning algorithm with an additive regret bound,
and convert it to an algorithm with a corresponding worst-case
iteration complexity bound for \\(\varepsilon\\)-approximate LP feasibility.
An algorithm that promises low worst-case regret in \\(\mathcal{O}(\sqrt{T})\\)
gives us an algorithm that needs at most \\(\mathcal{O}(1/\varepsilon\sp{2})\\)
iterations to return a solution that almost satisfies every constraint in the
linear program, where each constraint is violated by \\(\varepsilon\\) or less (e.g.,
\\(x \leq 1\\) is actually \\(x \leq 1 + \varepsilon\\)).

We first split the linear program in two components, a simple domain
(e.g., the non-negative orthant or the \\([0, 1]\sp{d}\\) box) and the
actual linear constraints.  We then map each of the latter constraints
to an expert, and use an arbitrary algorithm that solves our
continuous version of the experts problem as a black box.  At each
turn, the black box will output a set of non-negative weights for the
constraints (experts).  We will average the constraints using these
weights, and attempt to find a solution in the intersection of our
simple domain and the weighted average of the linear constraints.  We
can do so in the "experts problem" setting by consider each linear
constraint's violation as a *payoff*, or, equivalently, satisfaction
as a loss.

Let's use Stigler's [Diet Problem with three foods and two constraints](https://neos-guide.org/content/diet-problem)
as a small example, and further simplify it by disregarding the
minimum value for calories, and the maximum value for vitamin A.  Our
simple domain here is at least the non-negative orthant: we can't
ingest negative food.  We'll make things more interesting by also
making sure we don't eat more than 10 servings of any food per day.

The first constraint says we mustn't get too many calories

\\[72 x\sb{\mathrm{corn}} + 121 x\sb{\mathrm{milk}} + 65 x\sb{\mathrm{bread}} \leq 2250,\\]

and the second constraint (tweaked to improve this example) ensures
we ge enough vitamin A

\\[107 x\sb{\mathrm{corn}} + 400 x\sb{\mathrm{milk}} \geq 5000,\\]

or, equivalently,

\\[-107 x\sb{\mathrm{corn}} - 400 x\sb{\mathrm{milk}} \leq -5000,\\]

Given weights \\([3/4, 1/4]\\), the weighted average of the two constraints is

\\[27.25 x\sb{\mathrm{corn}} - 9.25 x\sb{\mathrm{milk}} + 48.75 x\sb{\mathrm{bread}} \leq 437.5,\\]

where the coefficients for each variable and for the right-hand side
were averaged independently.

The subproblem asks us to find a feasible point in the intersection
of these two constraints:
\\[27.25 x\sb{\mathrm{corn}} - 9.25 x\sb{\mathrm{milk}} + 48.75 x\sb{\mathrm{bread}} \leq 437.5,\\]
\\[0 \leq x\sb{\mathrm{corn}},\, x\sb{\mathrm{milk}},\, x\sb{\mathrm{bread}} \leq 10.\\]

Classically, we claim that this is just Lagrangian relaxation, and
find a solution to

\\[\min 27.25 x\sb{\mathrm{corn}} - 9.25 x\sb{\mathrm{milk}} + 48.75 x\sb{\mathrm{bread}}\\]
subject to
\\[0 \leq x\sb{\mathrm{corn}},\, x\sb{\mathrm{milk}},\, x\sb{\mathrm{bread}} \leq 10.\\]

In the next section, I'll explain why I think this analogy is wrong
and worse than useless.  For now, we can easily find the minimum one
variable at a time, and find the solution 
\\(x\sb{\mathrm{corn}} = 0\\), \\(x\sb{\mathrm{milk}} = 10\\),
\\(x\sb{\mathrm{bread}} = 0\\), with objective value \\(-92.5\\) (which
is \\(530\\) less than \\(437.5\\)).

In general, three things can happen at this point.  We could discover
that the subproblem is infeasible.  In that case, the original
non-relaxed linear program itself is infeasible: any solution to the
original LP satisfies all of its constraints, and thus would also
satisfy any weighted average of the same constraints.  We could also
be extremely lucky and find that our optimal solution to the relaxation is
(\\(\varepsilon\\)-)feasible for the original linear program; we can stop
with a solution.  More commonly, we have a solution that's feasible for the
relaxation, but not for the original linear program.

Since that solution satisfies the weighted average constraint and
payoffs track constraint violation, the black box's payoff for this
turn (and for every other turn) is non-positive.  In the current case,
the first constraint (on calories) is satisfied by \\(1040\\), while
the second (on vitamin A) is violated by \\(1000\\).  On weighted
average, the constraints are satisfied by \\(\frac{1}{4}(3 \cdot
1040 - 1000) = 530.\\) Equivalently, they're violated by \\(-530\\) on
average.

We'll add that solution to an accumulator vector that will come in
handy later.

The next step is the key to the reduction: we'll derive payoffs
(negative costs) for the black box from the solution to the last
relaxation.  Each constraint (expert) has a payoff equal to its level
of violation in the relaxation's solution.  If a constraint is
strictly satisfied, the payoff is negative; for example, the constraint
on calories is satisfied by \\(1040\\), so its payoff this turn is
\\(-1040\\).  The constraint on vitamin A is violated by \\(1000\\),
so its payoff this turn is \\(1000\\).  Next turn, we expect the
black box to decrease the weight of the constraint on calories,
and to increase the weight of the one on vitamin A.

After \\(T\\) turns, the total payoff for each constraint is equal to
the sum of violations by all solutions in the accumulator.  Once we
divide both sides by \\(T\\), we find that the divided payoff for each
constraint is equal to its violation by the average of the solutions
in the accumulator.  For example, if we have two solutions, one that
violates the calories constraint by \\(500\\) and another that
satisfies it by \\(1000\\) (violates it by \\(-1000\\)), the total
payoff for the calories constraint is \\(-500\\), and the average
of the two solutions does strictly satisfy the linear constraint by
\\(\frac{500}{2} = 250\\)!

We also know that we only generated feasible solutions to the relaxed
subproblem (otherwise, we'd have stopped and marked the original LP as
infeasible), so the black box's total payoff is \\(0\\) or negative.

Finally, we assumed that the black box algorithm guarantees an additive
regret in \\(\mathcal{O}(\sqrt{T}\, \lg n)\\), so the black box's payoff
of (at most) \\(0\\) means that any constraint's payoff is at most
\\(\mathcal{O}(\sqrt{T}\, \lg n)\\).  After dividing by \\(T\\), we obtain
a bound on the violation by the arithmetic mean of all solutions in
the accumulator: for all constraint, that violation is in 
\\(\mathcal{O}\left(\frac{\lg n}{\sqrt{T}}\right)\\).  In other words, the number
of iteration \\(T\\) must scale with
\\(\\mathcal{O}\left(\frac{\lg n}{\varepsilon\sp{2}}\right)\\), 
which isn't bad when \\(n\\) is in the millions but
\\(\varepsilon \approx 0.01\\).

Theoreticians find this reduction interesting because there are
concrete implementations of the black box, e.g., the
[multiplicative weight update (MWU) method](https://www.satyenkale.com/papers/mw-survey.pdf)
with non-asymptotic bounds.  For many problems, this makes it
possible to derive the exact number of iterations necessary
to find an \\(\varepsilon-\\)feasible fractional solution, given
\\(\varepsilon\\) and the instance's size (but not the instance
itself).

That's why algorithms like MWU are theoretically useful tools for
fractional approximations, when we already have subgradient methods
that only need \\(\mathcal{O}\left(\frac{1}{\varepsilon}\right)\\) iterations:
state-of-the-art algorithms for learning with experts explicit
non-asymptotic regret bounds that yield, for many problems, iteration
bounds that only depend on the instance's size, but not its data.
While the iteration count when solving LP feasibility with MWU scales
with \\(\frac{1}{\varepsilon\sp{2}}\\), it is merely proportional to
\\(\lg n\\), the log of the the number of linear constraints.  That's
attractive, compared to subgradient methods for which the iteration
count scales with \\(\frac{1}{\varepsilon}\\), but also scales
linearly with respect to instance-dependent values like the distance
between the initial dual solution and the optimum, or the Lipschitz
constant of the Lagrangian dual function; these values are hard to
bound, and are often proportional to the square root of the number
of constraints.  Given the choice between
\\(\mathcal{O}\left(\frac{\lg n}{\varepsilon\sp{2}}\right)\\) 
iterations with explicit constants, and a looser
\\(\mathcal{O}\left(\frac{\sqrt{n}}{\varepsilon}\right)\\), it's 
obvious why MWU and online learning are powerful additions to 
the theory toolbox.

Theoreticians are otherwise not concerned with efficiency, so the
usual answer to someone asking about optimisation is to tell them they
can always reduce linear optimisation to feasibility with a binary
search on the objective value.  I once made the mistake of
implementing that binary search last strategy.  Unsurprisingly, it
wasn't useful.  I also tried another theoretical reduction, where I
looked for a pair of primal and dual -feasible solutions that happened
to have the same objective value.  That also failed, in a more
interesting manner: since the two solution had to have almost the same
value, the universe spited me by sending back solutions that were
primal and dual infeasible in the worst possible way.  In the end, the
second reduction generated fractional solutions that were neither
feasible nor superoptimal, which really isn't helpful.

Direct linear optimisation with experts
---------------------------------------

The reduction above works for any "simple" domain, as long as it's
convex and we can solve the subproblems, i.e., find a point in the
intersection of the simple domain and a single linear constraint or
determine that the intersection is empty.

The set of (super)optimal points in some initial simple domain is
still convex, so we could restrict our search to the search of the
domain that is superoptimal for the linear program we wish to
optimise, and directly reduce optimisation to the feasibility problem
solved in the last section, without binary search.

That sounds silly at first: how can we find solutions that are
superoptimal when we don't even know the optimal value?

Remember that the subproblems are always relaxations of the original
linear program.  We can port the objective function from the original
LP over to the subproblems, and optimise the relaxations.  Any
solution that's optimal for a realxation must have an optimal or
superoptimal value for the original LP.

Rather than treating the black box online solver as a generator of 
[Lagrangian dual](https://en.wikipedia.org/wiki/Duality_\(optimization\)#The_strong_Lagrangian_principle:_Lagrange_duality)
vectors, we're using its weights as solutions to the
[*surrogate* relaxation dual](https://smartech.gatech.edu/bitstream/handle/1853/24230/karwan_mark_h_197612_phd_154133.pdf).
The latter interpretation isn't just more powerful by handling
objective functions.  It also makes more sense: the weights generated
by algorithms for the experts problem are probabilities, i.e., they're
non-negative and sum to \\(1\\).  That's also what's expected for surrogate
dual vectors, but definitely not the case for Lagrangian dual vectors,
even when restricted to \\(\leq\\) constraints.

We can do even better!

Unlike Lagrangian dual solvers, which only converge when fed
(approximate) subgradients and thus make us (nearly) optimal solutions
to the relaxed subproblems, our reduction to the experts problem only
needs feasible solutions to the subproblems.  That's all we need to
guarantee an \\(\varepsilon-\\)feasible solution to the initial problem
in a bounded number of iterations.  We also know exactly how that
\\(\varepsilon-\\)feasible solution is generated: it's the arithmetic
mean of the solutions for relaxed subproblems.

This lets us decouple finding lower bounds from generating feasible
solutions that will, on average, \\(\varepsilon-\\)satisfy the
original LP.  In practice, the search for an \\(\varepsilon-\\)feasible
solution that is also superoptimal will tend to improve the lower
bound.  However, nothing forces us to evaluate lower bounds
synchronously, or to only use the experts problem solver to improve
our bounds.

We can find a new bound from any vector of non-negative constraint
weights: they always yield a valid surrogate relaxation.  We can solve
that relaxation, and update our best bound when it's improved.  The
Diet subproblem earlier had

\\[27.25 x\sb{\mathrm{corn}} - 9.25 x\sb{\mathrm{milk}} + 48.75 x\sb{\mathrm{bread}} \leq 437.5,\\]
\\[0 \leq x\sb{\mathrm{corn}},\, x\sb{\mathrm{milk}},\, x\sb{\mathrm{bread}} \leq 10.\\]

Adding the original objective function back yields the linear program

\\[\min 0.18 x\sb{\mathrm{corn}} + 0.23 x\sb{\mathrm{milk}} + 0.05 x\sb{\mathrm{bread}}\\]
subject to
\\[27.25 x\sb{\mathrm{corn}} - 9.25 x\sb{\mathrm{milk}} + 48.75 x\sb{\mathrm{bread}} \leq 437.5,\\]
\\[0 \leq x\sb{\mathrm{corn}},\, x\sb{\mathrm{milk}},\, x\sb{\mathrm{bread}} \leq 10,\\]

which has a trivial optimal solution at \\([0, 0, 0]\\).

When we generate a feasible solution for the same subproblem, we can
use any valid bound on the objective value to find the most feasible
solution that is also assuredly (super)optimal.  For example, if some
oracle has given us a lower bound of \\(2\\) for the original Diet
problem, we can solve for

\\[\min 27.25 x\sb{\mathrm{corn}} - 9.25 x\sb{\mathrm{milk}} + 48.75 x\sb{\mathrm{bread}}\\]
subject to
\\[0.18 x\sb{\mathrm{corn}} + 0.23 x\sb{\mathrm{milk}} + 0.05 x\sb{\mathrm{bread}}\leq 2\\]
\\[0 \leq x\sb{\mathrm{corn}},\, x\sb{\mathrm{milk}},\, x\sb{\mathrm{bread}} \leq 10.\\]

We can relax the objective value constraint further, since we know
that the final \\(\varepsilon-\\)feasible solution is a simple
arithmetic mean.  Given the same best bound of \\(2\\), and, e.g., a
current average of \\(3\\) solutions with a value of \\(1.9\\), a new
solution with an objective value of \\(2.3\\) (more than our best
bound, so not necessarily optimal!) would yield a new average solution
with a value of \\(2\\), which is still (super)optimal.  This means
we can solve the more relaxed subproblem

\\[\min 27.25 x\sb{\mathrm{corn}} - 9.25 x\sb{\mathrm{milk}} + 48.75 x\sb{\mathrm{bread}}\\]
subject to
\\[0.18 x\sb{\mathrm{corn}} + 0.23 x\sb{\mathrm{milk}} + 0.05 x\sb{\mathrm{bread}}\leq 2.3\\]
\\[0 \leq x\sb{\mathrm{corn}},\, x\sb{\mathrm{milk}},\, x\sb{\mathrm{bread}} \leq 10.\\]

Given a bound on the objective value, we swapped the constraint and
the objective; the goal is to maximise feasibility, while generating a
solution that's "good enough" to guarantee that the average solution
is still (super)optimal.

For box-constrained linear programs where the box is the convex
domain, subproblems are bounded linear knapsacks, so we can simply
stop the greedy algorithm as soon as the objective value constraint is
satisfied, or when the knapsack constraint becomes active (we found a
better bound).

This last tweak doesn't just accelerate convergence to
\\(\varepsilon-\\)feasible solutions.  More importantly for me, it
pretty much guarantees that out \\(\varepsilon-\\)feasible solution
matches the best known lower bound, even if that bound was provided by
an outside oracle.  [Bundle methods](http://www.inrialpes.fr/bipop/people/malick/Docs/05-frangioni.pdf)
and the [Volume algorithm](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.217.8194&rep=rep1&type=pdf)
can also mix solutions to relaxed subproblems in order to generate
\\(\varepsilon-\\)feasible solutions, but the result lacks the last
guarantee: their fractional solutions are even more superoptimal
than the best bound, and that can make bounding and variable fixing
difficult.

The secret sauce: AdaHedge
--------------------------

Before last Christmas's CVRP set covering LP, I had always used the 
[multiplicative weight update (MWU) algorithm](https://www.satyenkale.com/papers/mw-survey.pdf)
as my black box online learning algorithm:  it wasn't great, but I
couldn't find anything better. The two main downsides for me
were that I had to know a "width" parameter ahead of time, as well
as the number of iterations I wanted to run.

The width is essentially the range of the payoffs; in our case, the
potential level of violation or satisfaction of each constraints by
any solution to the relaxed subproblems.  The dependence isn't
surprising: folklore in Lagrangian relaxation also says that's a big
factor there.  The problem is that the most extreme violations and
satisfactions are initialisation parameters for the MWU algorithm,
and the iteration count for a given \\(\varepsilon\\) is quadratic
in the width (\\(\mathrm{max}\sb{violation} \cdot \mathrm{max}\sb{satisfaction}\\)).

What's even worse is that the MWU is explicitly tuned for a specific
iteration count.  If I estimate that, give my worst-case width estimate,
one million iterations will be necessary to achieve \\(\varepsilon-\\)feasibility,
MWU tuned for 1M iterations will need 1M iterations, even if the actual
width is narrower.

[de Rooij and others published AdaHedge in 2013](https://arxiv.org/abs/1301.0534),
an algorithm that addresses both these issues by smoothly estimating
its parameter over time, without using the doubling trick.[^doubling]
AdaHedge's loss (convergence rate to an \\(\varepsilon-\\)solution)
still depends on the relaxation's width.  However, it depends on the
maximum width actually observed during the solution process, and not
on any explicit worst-case bound.  It's also not explicily tuned for a
specific iteration count, and simply keeps improving at a rate that
roughly matches MWU.  If the instance happens to be easy, we will find
an \\(\varepsilon-\\)feasible solution more quickly.  In the worst
case, the iteration count is never much worse than that of an
optimally tuned MWU.

[^doubling]: The doubling trick essentially says to start with an estimate for some parameters (e.g., width), then adjust it to at least double the expected iteration count when the parameter's actual value exceeds the estimate. The sum telescopes and we only pay a constant multiplicative overhead for the dynamic update.

These [400 lines of Common Lisp](https://gist.github.com/pkhuong/c508849180c6cf612f7335933a88ffa6)
implement AdaHedge and use it to optimise the set covering LP.  AdaHedge acts
as the online black box solver for the surrogate dual problem, the relaxed
set covering LP is a linear knapsack, and each subproblem attempts
to improve the lower bound before maximising feasibility.

When I ran the code, I had no idea how long it would take to find a
feasible enough solution: covering constraints can never be violated
by more than \\(1\\), but some points could be covered by hundreds of
tours, so the worst case satisfaction width is high. I had to rely on
the way AdaHedge adapts to the actual hardness of the problem.  In the
end, \\(34492\\) iterations sufficed to find a solution that was \\(4.5\%\\)
infeasible.[^wrong-log]  This corresponds to a worst case with a width
of less than \\(2\\), which is probably not what happened.  It seems
more likely that the surrogate dual isn't actually an omniscient
adversary, and AdaHedge was able to exploit some of that "easiness."

[^wrong-log]: I think I computed the \\(\log\\) of the number of decision variables instead of the number of constraints, so maybe this could have gone a bit better.

The iterations themselves are also reasonable: one sparse matrix /
dense vector multiplication to convert surrogate dual weights to an
average constraint, one solve of the relaxed LP, and another sparse
matrix / dense vector multiplication to compute violations for each
constraint.  The relaxed LP is a fractional \\([0, 1]\\) knapsack, so
the bottleneck is sorting double floats.  Each iteration took 1.8
seconds on my old laptop; I'm guessing that could easily be 10-20
times faster with vectorisation and parallelisation.

In another post, I'll show how using the same surrogate dual optimisation
algorithm to mimick [Lagrangian decomposition](https://link.springer.com/article/10.1007/BF02592954)
[instead of Lagrangian relaxation](https://perso.ensta-paristech.fr/~diam/ro/online/Monique.Guignard-top11201.pdf)
guarantees an iteration count in \\(\mathcal{O}\left(\frac{\lg \\#\mathrm{nonzero}}{\varepsilon\sp{2}}\right)\\) independently of luck or the specific linear constraints.
