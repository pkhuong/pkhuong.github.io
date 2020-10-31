---
layout: post
title: "Chubanov's projection methods for 0/1 programming"
date: 2017-06-17 15:24:00 -0400
comments: true
categories: LinearProgramming
---

I've long felt that compilers (and symbolic processing in general)
would benefit from embedding integer programming solvers.  However, I
was never comfortable with actually doing so for a production system
that others would have to run: industrial strength integer linear
programming solvers are large systems with complex runtime behaviour,
and that's not the kind of black box you want to impose on people who
just want to build their project.  (That's also true of SAT solvers,
though, so maybe embedding complicated black boxes is the new normal?)

However, if we had something simple enough to implement natively in
the compiler, we could hope for the maintainers to understand what the
ILP solver is doing.  This seems realistic to me mostly because the
generic complexity tends to lie in the continuous optimisation part.
Branching, bound propagation, etc. is basic, sometimes domain
specific, combinatorial logic; cut generation is probably the most
prominent exception, and even that tends to be fairly
combinatorial. (Maybe that's why we seem to be growing comfortable
with SAT solvers: no scary analysis.)  So, for the past couple years,
I've been looking for simple enough specialised solvers I could use in
branch-and-bound for large 0/1 ILP.

Some stuff with augmented lagrangians and specialised methods for
box-constrained QP almost panned out, but nested optimisation sucks
when the inner solver is approximate: you never know if you should be
more precise in the lower level or if you should aim for more outer
iterations.

A subroutine in [Chubanov's polynomial-time linear programming algorithm [PDF]](http://www.optimization-online.org/DB_FILE/2013/07/3948.pdf)
([related journal version](https://link.springer.com/article/10.1007/s10107-014-0823-8))
seems promising, especially since it doesn't suffer from the numerical
issues inherent to log barriers.

Chubanov's subroutine in branch-and-bound
-----------------------------------------

Chubanov's "Basic Subroutine" accepts a problem of the form \\(Ax =
0\\), \\(x > 0\\), and either:

1. returns a solution;
2. returns a non-empty subset of variables that must be 0 in any feasible solution;
3. returns a non-empty subset of variables \\(x\sb{i}\\) that always
   satisfy \\(x\sb{i} \leq u\\) in feasible solutions with \\(x\sp{\star} \in [0, 1]\\),
   for some constant \\(u < 1\\) (Chubanov sets \\(u = \frac{1}{2}\\)).

The class of homogeneous problems seems useless (never mind the
nondeterministic return value), but we can convert "regular" 0/1
problems to that form with a bit of algebra.

Let's start with \\(Ax = b\\), \\(0 \leq x \leq 1\\), we can
reformulate that in the homogeneous form:

\\[Ax - by = 0,\\]
\\[x + s - \mathbf{1}y = 0,\\]
\\[x, s, y \geq 0.\\]

Any solution to the original problem in \\([0, 1]\\) may be translated
to the homogeneous form (let \\(y = 1\\) and \\(s = 1 - x\\)).
Crucially, any 0/1 (binary) solution to the original problem is still
0/1 in the homogeneous form.  In the other direction, any solution
with \\(y > 0\\) may be converted to the box-constrained problem by
dividing everything by \\(y\\).

If we try to solve the homogenous form with Chubanov's subroutine, we
may get:

1. a strictly positive (for all elements) solution.  In that case,
   \\(y > 0\\) and we can recover a solution to the box-constrained
   problem.
2. a subset of variables that must be 0 in any feasible solution.  If
   that subset includes \\(y\\), the box-constrained problem is
   infeasible.  Otherwise, we can take out the variables and try
   again.
3. a subset of variables that are always strictly less than 1 in
   feasible solutions.  We exploit the fact that we only really care
   about 0/1 solutions (to the original problem or to the homogenous
   reformulation) to also fix these variables to 0; if the subset
   includes \\(y\\), the *0/1* problem is infeasible.

As soon as we invoke the third case to recursively solve a smaller
problem, we end up solving an interesting ill-specified relaxation of
the initial 0/1 linear program: it's still a valid relaxation of the
binary problem, but is stricter than the usual box linear relaxation.

That's more than enough to drive a branch-and-bound process.  In
practice, branch-and-bound is much more about proving the (near-)
optimality of an existing solution than coming up with strong feasible
solutions.  That's why the fact that the subroutine "only" solves
feasibility isn't a blocker.  We only need to prove the absence of 0/1
solutions (much) better than the incumbent solution, and that's a
constraint on the objective value.  If we get such a proof, we can
prune away that whole search subtree; if we don't, the subroutine
might have fixed some variables 0 or 1 (always useful), and we
definitely have a fractional solution.  That solution to the
relaxation could be useful for primal heuristics, and will definitely
be used for branching (solving the natural LP relaxation of constraint
satisfaction problems ends up performing basic propagation for us, so
we get some domain propagation for free by only branching on variables
with fractional values).

At the root, if we don't have any primal solution yet, we should
probably run some binary search on the objective value at the root
node and feed the resulting fractional solutions to rounding
heuristics.  However, we can't use the variables fixed by the
subroutine: until we have a feasible binary solution with objective
value \\(Z\sp{\star}\\), we can't assume that we're only interested in
binary solutions with object value \\(Z < Z\sp{\star}\\), so the
subroutine might fix some variables simply because there is no 0/1
solution that satisfy \\(Z < k\\) (case 3 is vacuously valid if there
is no 0/1 solution to the homogeneous problem).

That suffices to convince me of correctness.  I still have to
understand Chubanov's "Basic Subroutine."

Understanding the basic subroutine
----------------------------------

This
[note by Cornelis/Kees Roos](https://pdfs.semanticscholar.org/d55f/3e7a49012930320aff836e737533726c78d8.pdf)
helped me understand what makes the subroutine tick.

The basic procedure updates a dual vector \\(y\\) (not the same
\\(y\\) as the one I had in the reformulation… sorry) such that \\(y
\geq 0\\) and \\(\|y\|_1 = 1\\), and constantly derives from the dual
vector a tentative solution \\(z = P\sb{A}y\\), where \\(P\sb{A}\\)
projects (orthogonally) in the null space of the homogeneous
constraint matrix \\(A\\) (the tentative solution is \\(x\\) in
Chubanov's paper).

At any time, if \\(z > 0\\), we have a solution to the homogenous
system.

If \\(z = P\sb{A}y = 0\\), we can exploit the fact that, for any
feasible solution \\(x\\), \\(x = P\sb{A}x\\): any feasible solution
is alrady in the null space of \\(A\\).  We have

\\[x\sp{\top}y = x\sp{\top}P\sb{A}y = x\sp{\top}\mathbf{0} = 0\\]

(the projection matrix is symmetric).  The solution \\(x\\) is
strictly positive and \\(y\\) is non-negative, so this must mean that,
for every component of \\(y\sb{k} > 0\\), \\(x\sb{k} = 0\\).  There is
at least one such component since \\(\|y\|_1 = 1\\).

The last condition is how we bound the number of iterations.  For any feasible solution
\\(x\\) and any component \\(j\\),

\\[y\sb{j}x\sb{j} \leq y\sp{\top}x = y\sp{\top}P\sb{A}x \leq \|x\| \|P\sb{A}y\| \leq \sqrt{n} \|z\|.\\]

Let's say the max element of \\(y\\), \\(y\sb{j} \geq 2 \sqrt{n}\|z\|\\).
In that case, we have
\\[x\sb{j} \leq \frac{\sqrt{n}\|z\|}{y\sb{j}} \leq \frac{1}{2}.\\]

Chubanov uses this criterion, along with a potential argument on
\\(\|z\|\\), to bound the number of iterations.
However, we can apply the result at any iteration where we find that
\\(x\sp{\top}z < y\sb{j}\\): any such \\(x\sb{j} = 0\\) in binary
solutions.  In general, we may upper bound the left-hand side with
\\(x\sp{\top}z \leq \|x\|\|z\| \leq \sqrt{n}\|z\|\\), but we can
always exploit the structure of the problem to have a tighter bound
(e.g., by encoding clique constraints
\\(x\sb{1} + x\sb{2} + ... = 1\\) directly in the homogeneous
reformulation).

The rest is mostly applying lines 9-12 of the basic procedure in
[Kees's note](https://pdfs.semanticscholar.org/d55f/3e7a49012930320aff836e737533726c78d8.pdf).
Find the set \\(K\\) of all indices such that
\\(\forall k\in K,\ z\sb{k} \leq 0\\) (Kees's criterion is more relaxed,
but that's what he uses in experiments), project the vector
\\(\frac{1}{|K|} \sum\sb{k\in K}e\sb{k}\\) in the null space of
\\(A\\) to obtain \\(p\sb{K}\\), and update \\(y\\) and \\(z\\).

The potential argument here is that after updating \\(z\\),
\\(\frac{1}{\|z\|\sp{2}}\\) has increased by at least \\(|K| > 1\\).
We also know that \\(\max y \geq \frac{1}{n}\\), so we can fix a
variable to 0 as soon as \\(\sqrt{n} \|z\| < \frac{1}{n}\\), or,
equivalently, \\(\frac{1}{\|z\|} > n\sp{3/2}\\).  We need to increment
\\(\frac{1}{\|z\|\sp{2}}\\) to at most \\(n\sp{3}\\), so we will go
through at most \\(1 + n\sp{3})\\) iterations of the basic procedure
before it terminates; if the set \\(K\\) includes more than one
coordinate, we should need fewer iterations to reach the same limit.

Chubanov shows how to embed the basic procedure in a basic iterative
method to solve binary LPs.  The interesting bit is that we reuse the
dual vector \\(y\\) as much as we can in order to bound the total
number of iterations in the basic procedure.  We fix at least one
variable to \\(0\\) after a call to the basic procedure that does not
yield a fractional solution; there are thus at most \\(n\\) such calls.

Next step
---------

In contrast to regular numerical algorithms, the number of iterations
and calls so far have all had exact (non asymptotic) bounds.  The
asymptotics hide in the projection step, where we average elementary
unit vectors and project them in the null space of \\(A\\).  We know
there will be few (at most \\(n\\)) calls to the basic procedure, so
we can expend a lot of time on matrix factorisation.  In fact,
Chubanov outright computes the projection matrix in
\\(\mathcal{O}(n\sp{3})\\) time to get his complexity bound of
\\(\mathcal{O}(n\sp{4})\\).  In practice, this approach is likely to
fill a lot of zeros in, and thus run out of RAM.

I'd start with the sparse projection code in
[SuiteSparse](http://faculty.cse.tamu.edu/davis/suitesparse.html).
The direct sparse solver spends less time on precomputation than fully
building the projection matrix (good if we don't expect to always hit
the worst case iteration bound), and should preserve sparsity (good
for memory usage).  In return, computing projections is slower, which
brings the worst-case complexity to something like
\\(\mathcal{O}(n\sp{5})\\), but that can be parallelised, should be
more proportional to the number of non-zeros in the constraint matrix
(\\(\mathcal{O}(n)\\) in practice), and may even exploit sparsity in
the right-hand side.  Moreover, we can hope that the \\(n\sp{3}\\)
iteration bound is pessimistic; that certainly seems to be the case
for most experiments with random matrices.

The worst-case complexity, between \\(\mathcal{O}(n\sp{4})\\) and
\\(\mathcal{O}(n\sp{5})\\), doesn't compare that well to interior
point methods (\\(\mathcal{O}(\sqrt{n})\\) sparse linear solutions).
However, that's all worst-case (even for IPMs).  We also have
different goals when embedding linear programming solvers in
branch-and-bound methods.  Warm starts and the ability to find
solution close to their bounds are key to efficient branch-and-bound;
that's why we still use simplex methods in such methods.  Chubanov's
projection routine seems like it might come close to the simplex's
good fit in branch-and-bound, while improving efficiency and
parallelisability on large LPs.
