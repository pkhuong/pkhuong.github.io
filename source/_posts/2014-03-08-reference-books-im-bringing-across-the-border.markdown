---
layout: post
title: "Reference books I'm bringing across the border"
date: 2014-03-08 22:15
comments: true
categories: 
---

I just spent the evening filling two boxes with books I'll bring from
Québec to my new apartment in New York.  To my surprise and delight,
that's less than a third of my collection (plus newer ones I have yet
to make my mind about).  Perhaps this list of books that survived the
cull can guide others in their acquisitions.  If you're around NYC,
I'd also be very happy to share my stash!

EDIT: I was asked for feedback on some of these books, so I added some
inline.

Fundamentals
============

[Brassard and Bratley, Fundamentals of Algorithmics](http://www.amazon.com/Fundamentals-Algorithmics-Gilles-Brassard/dp/0133350681)
is my go-to reference book for basic algorithmic stuff.  I love the
presentation, the authors are careful with Landau notation, and they
specify in what circumstances hand-waving tricks apply.  The quality
of Brassard's lectures is probably a factor as well: the book reminds
me of one of the best courses in
[Université de Montréal's](http://diro.umontreal.ca/accueil/)
undergraduate curriculum.

[CLRS](http://www.amazon.com/Introduction-Algorithms-Thomas-H-Cormen/dp/0262033844)
for baseline implementations of classic data structures and
algorithms.

[Knuth 1-4A](http://www.amazon.com/Computer-Programming-Volumes-1-4A-Boxed/dp/0321751043/)
for its attention to minutiae.  Also very useful as a reference to
point to: if something is so old or well known that I can't think of a
canonical reference, it's probably in Knuth.
 
Sedgewick, Algorithms (First edition) has an interesting section on
geometric algorithms.  I also like the way many older data structure
and algorithm books clearly take low-level coding consideration into
account while discussing asymptotics.  I find nearly everything by
Tarjan very good in that regard; his papers are among the few that
describe (and painstakingly prove) algorithmic improvements (e.g., the
inverse-Ackermann Union Find) only to reveal that a simpler
implementation with a worse bound consistently performs better in
practice.

Correctness
===========

[Bertot and Castéran, Interactive Theorem Proving and Program Development](http://www.amazon.com/Interactive-Theorem-Proving-Program-Development/dp/3540208542) is heavy and I'm still going through the book.

[Pierce, Types and Programming Languages](http://www.amazon.com/Types-Programming-Languages-Benjamin-Pierce/dp/0262162091)
should be mandatory skimming before entering a debate on "static" and
"dynamic" types.

[Gries, The Science of Programming](http://www.amazon.com/The-Science-Programming-Monographs-Computer/dp/0387964800)
changed the way I write programs.  It's not that I go full formal on
my code, but that I try and structure it so it's easier to reason
about.  I was just reminded of
[Dijkstra's Discipline of Programming](http://www.amazon.com/Discipline-Programming-Edsger-W-Dijkstra/dp/013215871X).
The first time I read both books, I did so concurrently.  It was a
long time ago, and I was less mature mathematically, but I definitely
remember finding Gries clearer and more directly applicable, while
Dijkstra did a better job of making me think hard about how to best
express the meaning of code.  Even now, I'd say they're still
complementary.

[Jackson, Software Abstraction](http://www.amazon.com/Software-Abstractions-Logic-Language-Analysis/dp/0262017156)
describes an interesting approach to interpolate between testing and
formal methods.  This book changed the way I write tests.  It's also
based on a nice application of combinatorial optimisation ;)

[Oram and Wilson, Beautiful Code](http://www.amazon.com/Beautiful-Code-Leading-Programmers-Practice/dp/0596510047)
feels uneven to me.  Some chapters cover inspiring gems, but many left
no mark.  A friend suggests
[Bentley's Programming Pearls](http://www.amazon.com/Programming-Pearls-Jon-Bentley/dp/8177588583)
or
[Neuman's Computer Related Risks](http://www.amazon.com/Computer-Related-Risks-Peter-G-Neumann/dp/020155805X).

Performance
===========

[Golub and Van Loan, Matrix Computations](http://www.amazon.com/Computations-Hopkins-Studies-Mathematical-Sciences/dp/1421407949)
is obviously a good reference for the implementation of dense linear
algebra.  However, I like this work even more for its clear and
precise discussion of performance and numerical precision matters.
One can't hide behind big-Oh when discussing linear algebra.  Yet, the
book is as enlightening and general as classic tomes on asymptotic
analysis of algorithms and data structures.  Better: that is true for
both the implementation and the use of the algorithms they describe.
If all software engineering were as well understood as numerical
linear algebra, our discipline would be in a good place.

[McGeoch, A Guide to Experimental Algorithmics](http://www.amazon.com/Guide-Experimental-Algorithmics-Catherine-McGeoch/dp/0521173019)
was my bedside companion for the holiday season.  On several
occasions, I told friends that this is the book I wish I had read ten
years ago and that I dreamt of writing the past couple year.  It has
good rules of thumb and suggestions for everything from designing
proper experiments to deriving robust performance models.

[Warren, Hacker's Delight](http://www.amazon.com/Hackers-Delight-Edition-Henry-Warren/dp/0321842685)
is really a fundamental book for me.  It's not that bit twiddling
tricks are that important, but that I know I can count on Warren's
nice exposition if I need to direct someone to a reference.  Now
nicely complemented by Knuth 4A.

Operations research
===================

Continuous optimisation
-----------------------

[Ahuja, Magnanti and Orlin, Network Flows](http://www.amazon.com/Network-Flows-Theory-Algorithms-Applications/dp/013617549X)
because it describes all the classics.  In particular, if an
optimisation problem is in P, it's probably in there.

[Bazaraa, Sherali and Shetty, Nonlinear Programming](http://www.amazon.com/Nonlinear-Programming-Algorithms-Mokhtar-Bazaraa/dp/0471486000)
mostly for a couple convergence proofs and as a reference.

[Chvátal, Linear Programming](http://www.amazon.com/Linear-Programming-Series-Mathematical-Sciences/dp/0716715872)
for its unique (? rare, definitely) and insightful presentation of
linear optimisation and of classic duality theorems.  I like the
sections on Network flows and on Basis factorisation for similar
reasons.

[Hiriart-Urruty and Lemaréchal, Convex Analysis and Minimization Algorithms](http://www.amazon.com/Convex-Analysis-Minimization-Algorithms-mathematischen/dp/3540568506)
because decomposition methods depend on convex nondifferentiable
optimisation.  I rarely care about general convex optimisation, but
it's an unavoidable building block for Lagrangian decomposition
methods.

Combinatorial optimisation
--------------------------

[Bollobás, Modern Graph Theory](http://www.amazon.com/Modern-Graph-Theory-Graduate-Mathematics/dp/0387984887)
mostly as a reference for classic results.  I remember having a lot of
fun with the section on matchings.

[Lawler, Combinatorial Optimization](http://www.amazon.com/Combinatorial-Optimization-Networks-Matroids-Mathematics/dp/0486414531)
is short but dense.  I don't grok matroids, and someone I respect gave
me this book and told me it would all make sense once I'd gone through
it.

[Nemhauser and Wolsey, Integer and Combinatorial Optimization](http://www.amazon.com/Integer-Combinatorial-Optimization-Laurence-Wolsey/dp/0471359432)
for the dark times when I need to directly think in terms of polytopes
and facets, or when I try to generate fancy cuts.

[Schrijver, Combinatorial Optimization](http://www.amazon.com/Combinatorial-Optimization-Alexander-Schrijver/dp/3540443894)
mostly as a reference for theoretical results.  It's exhaustive, so I
can use it like the TAoCP of optimisation and as a springboard to
other work.

[Wolsey, Integer programming](http://www.amazon.com/Integer-Programming-Laurence-A-Wolsey/dp/0471283665)
when Nemhauser is hard to follow.

Messy stuff
-----------

These books help me approach problems that don't fit neatly in any
box.

[Bertsekas, Dynamic Programming and Optimal Control](http://www.amazon.com/Dynamic-Programming-Optimal-Control-Vol/dp/1886529086)
and
[Law, Simulation, Modeling and Analysis](http://www.amazon.com/Simulation-Modeling-Analysis-Expertfit-Software/dp/0073294411)
fill holes left by my focusing on deterministic single-period
problems.  I use Bertsekas and Law as refreshers when I remember that
I'm forgetting something.  I can then decide whether to go more deeply
in that direction.

[Polya, How to Solve It](http://www.amazon.com/How-Solve-It-Mathematical-Princeton/dp/069111966X).
For the really dark times when it feels like I'm asked to do the
impossible.  Perhaps it's mostly useful because it distracts me from
the immediate task for a couple minutes.  Either way, I like to
re-read it when I get stuck.

[Tufte, The Visual Display of Quantitative Information](http://www.amazon.com/The-Visual-Display-Quantitative-Information/dp/0961392142),
along with [Hadley Wickham's ggplot2](http://ggplot2.org/), taught me
to think deliberately about presenting results.  Our visual cortex is
pretty awesome but bad visualisation wastes our brain on distractions.
