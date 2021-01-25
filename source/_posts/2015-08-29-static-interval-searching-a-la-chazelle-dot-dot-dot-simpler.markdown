---
layout: post
title: "Static interval searching à la Chazelle... simpler"
date: 2015-08-29 23:32:24 -0400
draft: true
hidden: true
comments: true
categories: 
---

I'm currently spending a lot of time on a static representation for a
set of intervals that will let me efficiently determine the subset of
intervals that contain a given point.  This type of query is known as
a "stabbing" query (picture the intervals as horizontal segments and
the point as a vertical line that stabs through intersecting
segments).

[drawing]

In
[Filtering search: a new approach to query-answering](https://www.cs.princeton.edu/~chazelle/pubs/FilteringSearch.pdf),
Bernard Chazelle introduces data structures with output sensitive
runtimes for the stabbing query problem and five more related
problems.  The output sensitivity is essential for useful analysis: I
can easily build a set of \\(n\\) intervals and a point \\(x\\) such
that \\(\Theta(n)\\) intervals contain \\(x\\).  Output sensitive
runtimes enable finer analyses by letting the runtime depend on both
\\(n\\), the number of elements, and (linearly) on \\(k\\), the
cardinality of the return value.

For stabbing queries, Chazelle's filtering search approach partitions
the domain (e.g., \\((-\infty, \infty)\\)) in a number of windows, and
associates with each window a list of intervals that intersect with
that window; windows are small enough that, for any stabbing point
that fall in a window, the window is associated with at most 
\\(\delta > 1\\) times as many intervals as the number of intervals
that actually contain the stabbing point.

[drawing]

In other words, if we know that every point in a window \\(w\\)
intersects with at least \\(c\\) intervals, the window must intersect
with at most \\(\delta c\\) intervals.

This means we can answer a stabbing query at \\(x\\) by finding the
window that contains \\(x\\) in \\(\log n\\) time (there mustn't be
too many windows) and naïvely looking at all the intervals that
intersect the window.

The intuition
=============

It's easy to see how the time to process a window is linear in the
number of results.  It's fairly plausible that the number of windows
can be bounded by a polynomial in the number of intervals, and binary
search over disjoint windows is then logarithmic time in \\(n\\).  The
hard part is showing that we can build small enough windows while
controlling the space overhead (linear in \\(n\\)).

Space overhead is a problem because a single interval may appear in
multiple windows (e.g.,
\\([1, 5)\\) might be shared by windows \\([0, 3), [3, 8)\\)): any
given interval will intersect partially with at most two windows, but
may fully enclose an arbitrary number of windows.

The trick is to realise that if a window \\(j\\) is fully enclosed by
\\(A\sb{j}\\) intervals, then any query in that window returns at least
\\(A\sb{j}\\) intervals, and there are thus
\\(B\sb{j} \approx (\delta - 1)A\sb{j}\\) other intervals in the window
that only intersect partially.  We also have
\\(\sum\sb{j\in J} B\sb{j} \leq 2n\\)
(where \\(J\\) is the set of windows).  We can sum both sizes of the previous
approximation, and see that the number of references to intervals in all the
windows remains linear in \\(n\\), and shrinks with \\(\delta\\).

Moreover, we only need to close a window when an interval reaches its upper
limit, so there at most as many windows as there are intervals.

The paper derives tighter bounds with the same idea, but the above
is all we need to understand Chazelle's to construct windows that
satisfy the \\(\delta c\\) condition.  The real issue for me is how
to represent windows in order to allow efficient queries.

Performance problems
====================

Chazelle suggests a simple and pointer-heavy representation for windows.  
Windows themselves are stored in a sorted array for simple binary searching.
However, each window has, in addition to its lower and upper bounds, an
array of pointers to intervals (that intersect with that window).

[drawing]

A stabbing query is then a binary search over the array of windows.  For
successful queries we find a window, and there's one more indirection to
traverse the window's array, plus a bunch more to query each interval in
that array.  That's a lot of random accesses; most of them come from binary
search, but we have a couple ideas to make that faster.  These random
accesses are particularly worrying for a common case (for me): windows that
contain exactly one interval of width 1.

Chazelle's construction returns a set of intervals, so we have to assume
the pointer identity of intervals matter.  Let's make things more concrete
and assume that a value is associated with each interval, and we want the
set of values associated with intervals that contain our stabbing point.

We can replace the array of pointers to intervals with an array of (copies)
of intervals and save one indirection.  There's still the indirection to
go from a window to its array of intervals.

Windows have copies of overlapping intervals; let's clamp these copies to only
represent the intersection between the window and the interval.  For example,
in window \\([0, 3)\\), interval \\([1, 5)\\) becomes \\([1, 3)\\).

This doesn't change the output (we only look at window \\([0, 3)\\) for queries
that fall in \\([0, 3)\\)), but means that we can now concatenate the array of
intervals for windows to find a nice structure.  If we first sort each window's
intervals by their lower bounds, the concatenation of all the windows' arrays
of intervals yields an array of intervals that is globally sorted by lower
bound!

[drawing]

We may now binary search on that array of split up intervals (remember, the
number of such clamped sub-intervals is linear in the number of input
intervals) instead of using the array of windows: an interval (clamped or
not) cannot contain \\(x\\) if its lower bound is greater than \\(x\\).
Binary search gives us the rightmost sub-interval with a small enough
lower bound, and we keep going left in the window.

We know where to start looking for matching intervals... but we have a
banana problem, because we don't know where to stop (where the window
begins)!  Looking  at the array of sub-intervals, it seems like that
shouldn't be an issue: all the upper bounds for sub-intervals left
of the window are less than \\(x\\) (and all upper bounds for sub-intervals
right of the window are greater than \\(x\\)).  However, the upper bounds
for subintervals in the window are in arbitrary order, so we can't easily
tell whether we reached the limit of the current window or are only going
through a bunch of misses.

[drawing]

Let's add one field, `begin`, to mark the first sub-interval in
each window.  We can stop after we process a sub-interval with
`begin = true`.

There's one last problem: there's no sub-interval to represent empty
windows, so queries with empty return values might accidentally
process an arbitrarily long window.  Let's set a maximum number of
extra sub-intervals (we already have a multiplier \\(\delta\\) for
useless sub-intervals) for empty return values, \\(\Delta\\).  If
we close a window with more than \\(\Delta\\) subintervals, and the
next (non-empty) window isn't adjacent, insert a dummy sub-interval
\\([b, b)\\) to mark the beginning of an empty window at \\(b\\).

We only add such sentinels after a window that contains more than
\\(\Delta\\) sub-intervals, and Chazelle shows that there are at
most \\(\delta/(\delta - 1) n\\) sub-intervals in total, so we
have at most \\(\delta/(\Delta + 1)(\delta - 1)\\) sentinels
(still linear!).

Unexpected goodness
===================

Chazelle's construction assume no particular order on the subintervals
associated with each window.  Thus, if the smallest answer to a stabbing
query in the window has \\(k\\) intervals, the window must have at most
\\(\delta k\\) sub-intervals.  When building windows, Chazelle must track
the *minimum* number of matching intervals for every value in the window.

Here, we also impose an order on subintervals (sorted by lower bound), so
we also skip subintervals that are fully to the right of our query \\(x\\).
This lets us relax the \\(\delta\\) condition: for every point \\(x\\) that
falls in the window, there must be at most \\(\delta k\\) sub-intervals with
lower bounds less than or equal to \\(x\\), where \\(k\\) is the number of
matches for \\(x\\).

Here's an example of intervals that use less space with the relaxed condition.

[drawing]

Implementation (search)
=======================

