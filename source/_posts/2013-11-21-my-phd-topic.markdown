---
layout: post
title: "My Ph.D. topic"
date: 2013-11-21 13:51
comments: true
categories: 
published: false
---

I will soon present the work I did during Ph.D. It will be one of the
few chances I'll have to really explain what I've been doing for the
past 5+ years, and I'm hoping writing this post will help me do that.

I'm in a Computer Science Ph.D. program, but really, I've been
studying Operations Research.  This field is relatively young and not
very well known.  There are many ways to understand the discipline,
but I like the way INFORMS is trying to brand it:
[the science of better](http://www.scienceofbetter.org/).  In my view,
operations research is a set of tools to help people understand
complex situations and make better informed choices by automating away
routine decisions.  When operations research is taught in business
schools, the focus may be on modeling tools that enable us to
disregard details and perform experiments on simplified systems;
the hope is that insights into abstract models will carry over to the
real situation.  I have the feeling that, for computer science
graduates and engineers, operations research is mostly about computing
good feasible solutions: given predetermined model and objective
function, what sort of algorithm may we use to compute good solutions
to problems that are usually NP-hard?  In mathematics, people seem to
be more interested in constructing proofs of optimality or
infeasibility and in showing that methods approach these witnesses
more or less quickly; in fact, a lot of works goes into approximating
the value of optimal solutions, without generating any feasible
solution.

I believe these perspectives are complementary and consider myself
lucky to have been exposed to all three.  Université de Montréal made
an interesting choice with its department of computer science *and*
operations research.  I'm also a member of the CIRRELT, an
interdisciplinary research center with members in business and
engineering schools as well as in computer science departments.

My work finds its practical motivation in the operations of an
industrial partner that manages its own parcel delivery network over a
whole country.  In addition to the challenging scale, there are hard
limits on delivery times, individual orders are tiny, and demand
fluctuates rapidly.  Thankfully, deliveries only go one way, from a
few identical large hubs to customers.  With time, the company has
settled on a general structure for the distribution network; that
structure lets it satisfy delivery time constraints and respond to
changes in demand rapidly, while keeping costs reasonably low.

The trick is to operate only a few large distribution hubs, and to
rent intermediate depots on a short term basis.  Large vehicles link
each depot to the nearest hub: only a few depots are open, so each one
receives many parcels.  Medium-size vehicles then depart from depots
to many more temporary trans-shipment locations; once there, parcels
are sorted and distributed between a few small vehicles.  These
vehicles finally deliver parcels to all the customers on their
respective routes.  Practicality adds the constraint that each
trans-shipment location receives parcels from exactly one depot: it is
too difficult to coordinate deliveries otherwise.

This situation could be represented arbitrarily precisely.  For
example, a fellow Ph.D. candidate at CIRRELT works on computing routes
such that parcels fit correctly in delivery vans and are accessible
from the back door when needed.  We chose to assume that parcels are
assigned predetermined delivery routes (perhaps thanks to that
student), and to instead focus on location decisions: where depots
should be rented, where parcels should be moved from medium vehicles
to delivery vehicles, from which trans-shipment location each route
should depart, and how depots and temporary locations should be
linked.  This choice is idiosyncratic -- research on such two-level
distribution networks mostly attempts to capture the routing aspect
accurately -- but I believe that it makes sense when guiding the
operations of a large distribution network: at a large scale, the
impact of location decisions dominate that of last-mile routing.

A long time ago, after my first semester of undergraduate studies in
mathematics and computer science, my now-advisor tasked me with
developing a heuristic method to generate good solutions to this
location problem in reasonable time.  The problem must be solved
frequently, if only to react to changes in demand, but classic exact
methods failed to generate decent solutions after CPU-hours.  Although
this effort is now obsolete, it helped me understand the problem
better. I'm told that's a common situations: heuristic methods are
developed and eventually thrown away in favour of approaches that
would never have been imagined without that first step.

Two summers later, I started work on proving the quality of solutions.
Primal heuristics usually generate close to optimal solutions (e.g.,
within 2-3% of optimum, when the error margin on the input data may be
on the order of 5% or more), but this is not always true.  Computing
bounds on the optimal value let us be certain that a specific solution
is close enough to optimum that it's not useful to try and improve it
further.  Initially, I depended on bounds computed by the exact method
after a few CPU-hours, but that's not practical.  Instead, my advisor
and I developed a Lagrangian decomposition method: the hard problem is
simplified by forcibly decomposing it in independent subproblems.
When, e.g., parcels can flow from a hub to a depot without making sure
that there are delivery vehicles between the two, the problem is
relaxed into something much simpler to solve.  Better: the cost of an
optimal solution to such a relaxation is a lower bound on the cost of
optimal solutions to the original complicated problem.  However, that
bound tends to be so far from the actual value that it is useless.

Lagrangian decomposition adds linear (i.e., separable) penalties to
the subproblems' cost functions: there is no hard constraint that the
decisions made in different subproblems be coherent, but the penalties
are adjusted iteratively to make incompatible configurations less
attractive.  Moreover, because of the way these penalties are
adjusted, the cost of optimal solutions to such simplified and
penalised problems are also lower bounds on the optimal value for the
original problem.  The advantage is that, thanks to the penalties,
these bounds are usually much more accurate than that of the brutal
decomposition.

At first, I was attracted to Lagrangian decomposition (and Lagrangian
relaxation of arbitrary linear constraints) because it doesn't depend
on compiling a nice problem down to a mixed integer program: I could
reason about the (simplified) problem domain instead of casting
everything in terms of affine inequalities and equalities over
variables taking either fractional or integer values.

That still leaves the problem of determining penalties that will give
a close lower bound (i.e., that leads to the maximal lower bound), but
variations of the natural gradient descent method (penalise
incompatibilities in the current solution) are unbeatable in theory.

[Same thing. My work: make eqv practical]
