---
layout: post
title: "Entomological solutions"
date: 2021-06-07 00:36:03 -0400
comments: true
categories: 
---

Non-blocking algorithms have a reputation for complexity.  However, if
you ask people who work on systems where strong progress guarantees
are mandatory (e.g., hard real-time systems), they'll often disagree.

I believe the difference is rooted in the way systems which *must*
bound their pauses will sacrifice nice-to-haves to more cleanly
satisfy that hard requirement.

Researchers and library writers instead tend to aim for maximal
guarantees or functionality while assuming (sacrificing) as little as
possible.  Someone who's sprinkling lock-free algorithms in a large
codebase will similarly want to rely on maximally general algorithms,
in order to avoid increasing the scope of their work: if
tail latency and lock-freedom were high enough priorities to justify
wide-ranging changes, the program would probably have been designed
that way from the start.

It makes sense to explore general solutions, and academia is
definitely a good place to do so.  It was fruitful for mathematicians
to ask questions about complex numbers, then move on to fields, rings,
groups, etc., like sadistic kids probing what a bug can still do as
they tear its legs off one by one.  Quicksort and mergesort are
probably the strongest exemplars of that sort of research in computer
science: we don't even ask what data is made of before assuming a
comparison sort is probably a good idea.

It is however more typical to trade something in return for
generality.  When there's no impact on performance or resource
usage, code complexity usually takes a hit.

When solving a core problem like lock-freedom in a parallel
realtime system, we instead ask how much more we can assume, what else
we can give up, in order to obtain simpler, more robust solutions.
We don't want generality, we're not crippling bugs; we want
specificity, we're dosing eggs with [Hox to get more legs](https://en.wikipedia.org/wiki/Antennapedia).

The first time someone used to academic non-blocking algorithms hears
about the resulting maximally specialised solutions, they'll sometimes
complain about "cheating." Of course, it's never cheating when a
requirement actually is met; the surprise merely shows that the rules
typically used to evaluate academic solutions are but approximations
of reality, and can be broken...  and practitioners faced with
specific problems are ideally placed to determine what rules
they can flout.

Hoarding is caring?
-------------------

My favourite example of such cheating is type-stable memory.  The
literature on Safe memory reclamation (SMR) conflates[^or-does-it] two
problems that are addressed by SMR algorithms: reclamation races, and
the ABA problem.

[^or-does-it]: [Maged Michael's original Safe Memory Reclamation paper](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.87.5131&rep=rep1&type=pdf) doesn't: allowing arbitrary memory management is the paper's main claim.  I think there's a bit of a first mover's advantage, and researchers are incentivised to play within the sandbox defined by Michael.  For example, [Arbel-Raviv and Brown in "Reuse, don't Recycle"](https://arxiv.org/abs/1708.01797) practically hide the implementation of their proposal on page 17 (Section 5), perhaps because a straightforward sequence counter scheme is too simple for publication nowadays.

A reclamation race is what happens when a thread dereferences a
pointer to the heap, but the pointee has already been deallocated;
even when the racy accesses are loads, they can result in a
segmentation fault (and a crash).

The ABA problem is what happens when a descriptor (e.g., a pointer) is
reused to refer to something else, but some code is unaware of the
swap. For example, a thread could load a global pointer to a logically
read-only object, read data off that pointer, sleep for a while, and
observe that the global pointer has the same value. That does not mean
nothing has changed: while the thread was sleeping, the pointee could
have been freed, and then recycled to satisfy a fresh allocation.

Classic SMR algorithms like [epoch reclamation](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-579.pdf)
and [hazard pointers](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.395.378&rep=rep1&type=pdf)
solve both problems at once; in fact, addressing
reclamation races is their core contribution (it's certainly the
challenging part), and ABA protection is simply a nice corollary.

However, programs *can choose* not to crash on benign use-after-free:
reading from freed objects only triggers crashes when memory is mapped
and unmapped dynamically, and that's usually not an option for hard
realtime systems.  On smaller embedded targets, there's a fixed
physical memory budget; either the program fits, or the program is
broken.  Larger shared-memory parallel programs often can't afford the IPIs
and other hiccups associated with releasing memory to the operating
system.  Either way, half the problem solved by SMR doesn't even exist
for them.

The other half, ABA, is still an issue...  but that subproblem is
easier to solve.  For example, we can tag data with sequence counters.[^reuse-dont-recycle]

[^reuse-dont-recycle]: See [Reuse, don't Recycle](https://arxiv.org/abs/1708.01797) for a flexible take.

Lock-free stacks and ABA
------------------------

A [lock-free multiple producers / single consumer linked stack](https://en.wikipedia.org/wiki/Treiber_stack) might be
the simplest lock-free data structure.[^but-maybe-not-scalable]

[^but-maybe-not-scalable]: A stack fundamentally focuses contention towards the top-of-stack pointer, so lock-free definitely doesn't imply scalable. It's still a good building block.

Pushing a new record to such a stack is easy:[^not-in-c] load the current top
of stack pointer, publish that in our [new record's "next" (CDR) field](https://en.wikipedia.org/wiki/CAR_and_CDR),
and attempt to replace the top of stack with a pointer to our new
record with a compare-and-swap (CAS).

[^not-in-c]: In assembly language, anyway. Language memory models make this surprisingly hard. For example, any ABA in the push sequence is benign (we still have the correct bit pattern in the "next" field), but C and C++'s pointer provenance rules say that accessing a live object through a pointer to a freed object that happens to alias the new object is undefined behaviour.

How do we consume from such a stack?

The simplest way is to use a fetch-and-set (atomic exchange) to
simultaneously clear the stack (set the top-of-stack pointer to the
"empty stack" sentinel, e.g., `NULL`) and read the previous
top-of-stack.  Any number of consumers can concurrently execute such a
batch pop, although only one will grab everything.

Alternatively, if there's only one consumer at a time, it can pop with
a compare-and-swap.  The consumer must load the current top-of-stack
pointer.  If the stack is empty, there's nothing to pop.  Otherwise,
it can read the top record's "next" field, and attempt to CAS out the
top-of-stack pointer from the one it just read to the "next" record.

The tricky step here is the one where the consumer reads the "next"
field in the current top-of-stack record: that step would be subject
to reclamation races, except that there's only one consumer, so
we know no one else concurrently popped that record and freed it.

What can we do to support multiple consumers? Can we
simply make sure that stack records are always safe to read, e.g., by
freeing them to an object pool? Unfortunately, while
use-after-free is benign for producers, it is not for
consumers.

The key problem is that a consumer can observe that the top of stack
points to record A, and that record A's "next" field points to B, and
then get stuck or sleep for a while.  During that time, another thread
pops A and B, frees both, pushes C, and then pushes A', a new record
that happens to have the same address as A.  Finally, the initial
consumer will compare the top-of-stack pointers with A (which also
matches A'), and swap that for B, resurrecting a record that has
already been consumed and freed.

Full-blown SMR would fix all that.  However, if we can instead assume
read-after-free do not crash (e.g., we use a type-stable allocator or
an explicit object pool for records), we simply have to reliably
detect when a record has returned to the top of the stack.[^ll-sc]

[^ll-sc]: Load Linked / Store Conditional solves *this specific problem*, but that doesn't mean LL/SC as found on real computers is necessarily a better primitive than compare-and-swap.

We can do that by tagging the top-of-stack pointer with a sequence
counter, and update both with a double-wide compare-and-swap: instead
of CASing the top-of-stack pointer, we want to CAS that pointer and
its monotonically increasing counter.  Every successful CAS of the
pointer will also increment the counter by one, so the sequence counter
will differ when a record is popped and pushed back on the stack.

There is still a risk of ABA: the counter can wrap around.  That's not
a practical concern with 64-bit counters, and there are reasonable
arguments that narrower counters are safe because no consumer will
stay "stuck" for minutes or hours.[^bitpacking]

[^bitpacking]: Which is nice, because it means we can pack data and sequence counters in 64-bit words, and use the more widely available single-word compare-and-swap.

Single-compare multiple-swaps
-----------------------------

Sometimes, the application can naturally guarantee that CASed fields
are ABA-free.

For example, a hierarchical bump allocator may carve out global
type-specific arenas from a shared chunk of address space, and satisfy
allocations for each object type from the type's current arena.
Within an arena, allocations are reserved with atomic increments.
Similarly, we carve out each arena from the shared chunk of address
space with a (larger) atomic increment.
Neither bump pointer ever decreases: once a region of address space
has been converted to an arena, it stays that way, and once an object
has been allocated from an arena, it also remains allocated (although
it might enter a freelist).  Arenas are also never recycled: once
exhausted, they stay exhausted.

When an allocation type has exhausted its current arena (the arena's
bump pointer is exhausted), we want to atomically grab a new arena
from the shared chunk of address space, and replace the type's arena
pointer with the newly created arena.

A lock-free algorithm for such a transaction looks like it would have
to build on top of multi-word compare-and-swap (MCAS), a hard operation that can
be [implemented in a wait-free manner, but with complex algorithms](https://www.cl.cam.ac.uk/research/srg/netos/papers/2002-casn.pdf).

However, we know that the compare-and-swapped state evolves
monotonically: once an arena has been carved out from the shared
chunk, it will never be returned as a fresh arena again.  In other
words, there is no ABA, and a compare-and-swap on an arena pointer
will never succeed spuriously.

Monotonicity also means that we can acquire a consistent snapshot of both
the type's arena pointer and the chunk's allocation state by reading
everything twice.  Values are never repeated, so any write
that executes concurrently with our snapshot loop will be detected: the
first and second reads of the updated data will differ.

We also know that the only way a type's arena pointer can be replaced
is by allocating a new one from the shared chunk.  If we took a
consistent snapshot of the type's arena pointer and of the shared
chunk's allocation state, and the allocation state hasn't changed
since, the arena pointer must also be unchanged (there's a
hierarchy).

We can combine all these properties to atomically replace a type's
arena pointer with a new one obtained from the shared chunk, using a
much simpler core operation, a single-compare multiple-swap (SCMS). We
want to execute a series of CASes (one to allocate an arena
in the chunk, a few to initialise the arena, and one to publish
the new arena), but we can also assume that once the first
updated location matches the CAS's expected value, all other ones will
as well.  In short, only the first CAS may fail.

That's the key simplifier compared to full-blown multi-word
compare-and-swap algorithms: they have to incrementally acquire update
locations, any of which might turn the operation into a failure.

We can instead encode all the CASes in a transaction descriptor, CAS
that descriptor in the first update location, and know that the
multi-swaps will all succeed iff that CAS is successful.

If the first CAS is successful, we also know that it's safe to execute
the remaining CASes, and finally replace the descriptor with its
final value with one last CAS.  We don't even have to publish the
descriptor to all updated locations, because concurrent allocations
will notice the current arena has been exhausted, and try to get a new
one from the shared chunk... at which point they will notice the
transaction descriptor.

All the CASes after the first one are safe to execute arbitrarily
often thanks to monotonicity.  We already know that any descriptor that has
been published with the initial CAS will not fail, which means the only
potential issue is spuriously successful CASes... but our mutable
fields never repeat a value, so that can't happen.

The application's guarantee of ABA-safety ends up really simplifying
this single-compare multiple-swap algorithm (SCMS), compared to a
multi-word compare-and-swap (MCAS).  In a typical MCAS implementation, helpers
must abort when they detect that they're helping a MCAS operation that
has already failed or already been completed.  Our single-compare
assumption (once the first CAS succeeds, the operation succeeds) takes
care of the first case: helpers never see failed operations.  Lack
of ABA means helpers don't have to worry about their CASes succeeding
after the SCMS operation has already been completed: they will always fail.

Finally, we don't even need any form of SMR on the transaction
descriptor: [a sequence counter in the descriptor and a copy of that counter in a tag next to pointers to that descriptor](https://arxiv.org/pdf/1708.01797.pdf#page=4)
suffice to disambiguate incarnations of the same physical descriptor.

Specialising to the allocator's monotonic state let us use
single-compare multiple-swap, a simplification of full multi-word
compare-and-swap, and further specialising that primitive for
monotonic state let us get away with nearly half as many CASes (k + 1
for k locations) as the state of the art for MCAS (2k + 1 for k locations).

A plea for integration
----------------------

There is a common thread between never unmapping allocated addresses,
sequence tags, type-stable memory, and the allocator's single-compare
multiple-swap: monotonicity.

The lock-free stack shows how easy it is to conjure up artificial
monotonicity.  However, when we integrate algorithms more tightly
with the program and assume the program's state is naturally
monotonic, we'll often unlock simpler and more efficient solutions.  I
also find there's something of a virtuous cycle: it's easier for a
module to guarantee monotonicity to its components when it itself only
has to handles monotonic state, like a sort of end-to-end monotonicity
principle.

Unfortunately, it's not clear how much latent monotonicity there is
in real programs.  I suppose that makes it hard to publish algorithms
that assumes its presence.  I think it nevertheless makes sense to
explore such stronger assumptions, in order to help practitioners estimate
what we could gain in exchange for small sacrifices.

Asymmetric synchronisation is widely used these days, but I imagine it
was once unclear how much practical interest there might be in that
niche; better understood benefits lead to increased adoption.  I hope
the same can happen for algorithms that assume monotonic state.

<p><hr style="width: 50%"></p>
