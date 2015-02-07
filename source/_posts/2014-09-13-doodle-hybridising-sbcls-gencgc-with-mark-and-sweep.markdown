---
layout: post
title: "Doodle: hybridising SBCL's GENCGC with mark and sweep"
date: 2014-09-13 22:56
comments: true
categories: 
---

_Meta-note: this is more of a journal entry than the usual post
here.  I'll experiment with the format and see if I like publishing
such literal and figurative doodles._

Garbage collection is in the air.  My friend
[Maxime](http://pointersgonewild.wordpress.com/2014/09/09/ds-garbage-collector-problem/)
is having issues with D's garbage collector, and Martin Cracauer has a
large patch to improve SBCL's handling of conservative references.  I
started reviewing that patch today, and, after some discussion with
Alastair Bridgewater, I feel like adding a mark-and-sweep component to
SBCL's GC might be easier than what the patch does, while achieving
its goal of reducing the impact of conservative references.  That lead
to the whiteboarding episode below and a plan to replace the garbage
collecting half of SBCL's generational GC.  But first, a summary of
the current implementation.

{% img /images/2014-09-13-doodle-hybridising-sbcls-gencgc-with-mark-and-sweep/gc_notes.png %}

The present, and how we got here
================================

CMUCL started out with a Cheney-style two-space collector.  Two-space
collectors free up space for more allocations by copying objects that
might still be useful (that are reachable from "roots," e.g.,
variables on the stack) from the old space to the new space.  Cheney's
algorithm elegantly simplifies this task by storing bookkeeping
information in the data itself.  When we copy an object to the new
space (because it is reachable), we want to make sure that all other
references to that object are also replaced with references to the
copy.  Cheney's solution to that desideratum is obvious: overwrite the
old object with a broken heart (forwarding pointer), a marker that

1. the object has already been copied to the new space;
2. the copy lives at address `x`.

This adds a constraint that heap-allocated objects can never be
smaller than a broken heart, but they're usually one or two words (two
in SBCL's case) so the constraint is rarely binding.

When the garbage collector traverses the roots (the stack, for
example) and finds a pointer, the code only has to dereference that
pointer to determine if the objects it points to has been moved.  If
so, the GC replaces the root pointer with a pointer to the copy in the
new space.  Otherwise, the GC copies the object to the new space,
repoints to that copy, and overwrites the old object with a broken heart.

We also need to traverse objects recursively: when we find that an
object is live and copy it to the new space, we must also make sure
that anything that objects points to is also preserved, and that any
pointer in that object is updated with pointers to copies in the new
space.

That's a graph traversal, and the obvious implementation
maintains a workset of objects to visit which, in the worst case,
could include all the objects in old space.  The good news is we don't have
to worry about objects re-entering that workset: we always
overwrite objects (in old space) with a broken heart when we visit
them for the first time.

Cheney proposed a clever trick to implement this workset.  Whenever an
object enters the workset, it has just been copied to the new space;
as long as we allocate in the new space by incrementing an allocation
pointer, the new space itself can serve as the workset!  In addition
to the allocation pointer, we now need a "to-scan" pointer.  Any
object in the new space that's below the to-scan pointer has already
been scanned for pointers and fixed to point in the new space; any object
between the to-scan pointer and the allocation pointer must be scanned
for pointers to the old space.  We pop an element from the workset by
looking at the next object (in terms of address) after the to-scan
pointer and incrementing that pointer by the object's size.  When the
to-scan and the allocation pointers meet, the workset is empty
and GC terminates.

Some SBCL platforms still use this two-space collector, but it doesn't
scale very well to large heaps (throughput is usually fine, but we
waste a lot of space and GC pauses can be long).  The generational
conservative garbage collector (GENCGC, GENGC on
precise/non-conservative platforms) is a hack on top of that Cheney GC.

The GC is "generational" because most passes only collect garbage from
a small fraction of the heap, and "conservative" because we have to
deal with values that may or may not be pointers (e.g., we don't always
know if the value in a register is a Lisp reference or just a machine
integer) by considering some objects as live (not up for collection)
while pinning them at their current address.

The runtime uses mprotect to record writes to the heap, except for the
nursery (newly allocated objects) where we expect most writes to land.
The heap is partitioned in pages, and the first write to a page after
a GC triggers a protection fault; the signal handler marks that page
as mutated and changes the protection to allow writes.

When a GC is triggered, we usually want to collect only the nursery,
i.e., only objects that were allocated after the previous GC pass.
GEN(C)GC adapts Cheney to this use case by building the set of all
pages that might have been mutated to point somewhere in the nursery
(thanks to the mprotect write barrier) and scanning them for roots,
like the stack in Cheney GC.  The default GENGC configuration has 7
generations and we extend this scheme by flagging pages with pointers
to younger generations (newer objects), without noting what these
generations might be.

Pinned objects are also handled by abusing the root set: pages that
contain at least one pinned object don't undergo garbage collection
and are directly scanned for pointers, like the stack in Cheney GC.

Instead of having two heaps, an old space and a new space, we now have
a lot of pages, and each page belongs to a generation.  When we want
to collect a given generation, pages in that generation form the old
space, and pages allocated during GC the new space.  This means that
we lose the simplicity of Cheney's new-space-is-the-workset trick: the
new space isn't contiguous, so a single to-scan pointer doesn't cut it
anymore!  GENGC works around that by scanning the page table, but it's
not pretty and I really don't know if Cheney is a good fit anymore.

Martin Cracauer's patch
=======================

GENCGC's approach to pinned objects is stupid.  If a page has no
reference except for one conservative pointer, the whole page is
considered live and scanned for references.  

Martin's solution is to allocate additional temporary metadata only
for pinned pages and track the pinned status of individual objects.
When the GC encounters a pointer to a page with pinned objects, it
checks if it's a pointer to a pinned object.  If so, the pointee is
left in place.  Otherwise, it's copied normally.

The patch has code to mark objects as live (pinned) and to overwrite
objects once they have been copied.  Basically, it is half of a
mark-and-sweep garbage collector.  The main difference is that the set
of pinned objects doesn't grow (being pinned isn't a contagious
property), so we don't need a worklist for pinned objects.  However, I
already noted that I'm not convinced the worklist hack in GENGC is a
good idea.

A hybrid collector!
===================

Instead of marking pages as containing pinned objects, I feel it may
be simpler to collect some pages by copying, and others by marking.
Any pinned page would have the "mark" GC policy, while pages that
likely contain few live objects (e.g., the nursery and pages with a
lot of unused memory) would be collected by copying.  This too would
avoid the issue with considering whole *pages* as live when pinned,
and I think that having the choice of copying or marking at a page
granularity will be simpler than toggling at the level of individual
object.

Each "mark" page now has two (bit)sets, one for live objects and
another for live objects that have already been scanned.  We can
maintain a worklist at the page granularity with an embedded linked
list: whenever a "mark" page gains a new live object and it's not
already in the worklist, that page is enqueued for scanning.

Instead of emulating Cheney's trick by looking for newly allocated
pages in our page table, we can add pages in new space to the worklist
whenever they become full.

Finally, at the end of the pass, we traverse all "mark" pages and
clear dead objects.

That's pretty simple (arguably simpler than the current
implementation!), and shouldn't involve too many changes to the rest
of the code.  Mostly, I'd have to adapt the weak pointer machinery to
stop assuming that it can use forwarding pointers to determine when
objects have been marked as live.

However, we might lose the ability to run medium GCs, to collect more
than the nursery but less than the whole heap.  If we only want to GC
the nursery, the mprotect write barrier gives us all the information
we need to find references from the rest of the heap to the nursery.
If we wish to collect the whole heap, we only have to consider stacks
and some statically allocated space as roots.

For medium GCs, e.g., collect only generations 1-4 out of 7, GENGC
exploits the way that garbage collection (almost) always copies to
easily track pages with pointers to younger generations.  It's coarse,
but usually acceptable thanks to the copying.  I don't know that it
would work as well if the default is to only copy the nursery.
Moreover, if we have a hybrid GC, it probably makes sense to focus
copying on pages that are mostly empty, regardless of their age.  If
we do want medium GCs, we might have to track, for each page, the set
of pages that point there.  This set can include false positives, so
it's probably easiest to clear it before major GCs, and otherwise only
add to that set (removing pages that were emptied by a GC pass sounds
reasonable).  I also expect that some pages will have many refererrers;
I'm thinking we might use a distinguished value to mean "referred by
every pages" and not consider them for medium GC.

What's next
===========

Martin's patch clearly addresses an important weakness in SBCL's
garbage collector.  If I can't make good progress on the hybrid GC
soon, I'll make sure the patch is cleaned up for master, hopefully by
Thanksgiving.
