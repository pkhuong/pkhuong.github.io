---
layout: post
title: "Slitter: a slab allocator that trusts, but verifies"
foo: "Doveryai, no proveryai for your allocator"
date: 2021-08-01 17:26:04 -0400
hidden: true
draft: true
comments: true
categories: 
---

[Slitter](https://github.com/backtrace-labs/slitter) is Backtrace's
deliberately middle-of-the-line
[thread-caching](https://www.usenix.org/legacy/publications/library/proceedings/usenix01/full_papers/bonwick/bonwick.pdf)
[slab allocator](https://people.eecs.berkeley.edu/~kubitron/courses/cs194-24-S13/hand-outs/bonwick_slab.pdf),
with [explicit allocation class tags](https://github.com/backtrace-labs/slitter/blob/7afb9781fd25b8cee62afa555b9d38f391131044/include/slitter.h#L7-L44)
(rather than derived from the object's size class).  The design is
about as standard as it gets: we hope to dedicate the project's
complexity budget to always-on "observability" and safety features.
We don't wish to detect all or even most memory management errors, but
we should statistically catch a small fraction (enough to help
pinpoint production issues) of such bugs, and *always* constrain their
scope to the mismanaged allocation class.[^blast-radius]

[^blast-radius]: In my experience, their unlimited blast radius is what makes memory management bugs so frustrating to track down.  The design goals of generic memory allocators (e.g., recycling memory quickly) and some implementation strategies (e.g., [in-band metadata](http://phrack.org/issues/57/9.html#article)) make it easy for bugs in one module to show up as broken invariants in a completely unrelated one that happened to share allocation addresses with the former.  [Adversarial thinkers](http://phrack.org/issues/57/8.html#article) will even exploit the absence of isolation to [amplify small programming errors into arbitrary code execution](https://www.openwall.com/articles/JPEG-COM-Marker-Vulnerability).  Of course, one should simply not write bugs, but when they do happen, it's nice to know that the broken code most likely hit itself and its neighbours in the callgraph, and not unrelated code that also uses the same memory allocator (something windows got right with private heaps).

We have been running [Slitter](https://crates.io/crates/slitter) in production for over two months, and rely on it to:

-  detect when an allocation is freed with the wrong allocation class
   tag (i.e., detect type confusion on free).
-  avoid any in-band metadata: there are guard pages between
   allocations and allocator metadata, and no intrusive freelist for
   use-after-frees to stomp over.
-  guarantee [type stable allocations](https://www.usenix.org/legacy/publications/library/proceedings/osdi96/full_papers/greenwald/node2.html): once an address has been used
   to fulfill a request for a certain allocation class, it will only
   be used for that class.  Slitter doesn't overlay intrusive lists on
   top of freed allocations, so the data always reflects what the
   application last stored there.  This lets application rely on
   benign use-after-free in non-blocking algorithms.  Type stability
   also means that double-frees or actually detrimental
   use-after-frees only affect the faulty allocation class.
-  let each allocation class specify how its backing memory should
   be mapped in (e.g., plain 4 KB pages, file-backed swappable pages,
   huge pages, or gigantic pages).
   
Thanks to extensive contracts and a mix of hardcoded and random tests,
we encountered only two issues during initial the rollout, both in the
small amount of lock-free C code that is hard to test.[^legacy-gcc]
In the future, we hope to also:

[^legacy-gcc]: It would be easy to blame the complexity of lock-free code, but the initial version, with C11 atomics, was correct.  Unfortunately, gcc backs C11 atomic `uint128_t`s with locks, so we had to switch to the legacy interface, and that's when the errors crept in.

- detect when an interior pointer is freed.
- detect simple[^jump] buffer overflows that cross allocation classes, with guard pages.
- always detect frees of addresses Slitter does not manage.
- detect most back-to-back double-frees.
- detect a random fraction of buffer overflows, with a sampling [eFence](https://en.wikipedia.org/wiki/Electric_Fence).

[^jump]: There isn't much the allocator can do if an application writes to a wild address megabytes away from the base object.

In addition to these safety features, we plan to have the allocator
improve the observability of the calling program, and wish to:

- track the number of objects allocated and recycled in each
  allocation class.
- sample the call stack when the heap grows.
- track allocation and release call stacks for a small fraction of objects.

Here's how it currently works, and why we decided the world needs yet
another memory allocator.

The high level design of Slitter
--------------------------------

At a [high level](https://github.com/backtrace-labs/slitter/blob/fa8629989cb63ca5a4acdc2d26741bccda79aac0/doc/design.md),
Slitter

1. reserves shared 1 GB `Chunk`s of memory via the [`Mapper` trait](https://github.com/backtrace-labs/slitter/blob/fa8629989cb63ca5a4acdc2d26741bccda79aac0/src/mapper.rs)
2. carves out smaller type-specific `Span`s from each chunk with [`Mill` objects](https://github.com/backtrace-labs/slitter/blob/7afb9781fd25b8cee62afa555b9d38f391131044/src/mill.rs)
3. bump-allocates objects from `Span`s with [`Press` objects](https://github.com/backtrace-labs/slitter/blob/7afb9781fd25b8cee62afa555b9d38f391131044/src/press.rs), into [allocation `Magazines`](https://github.com/backtrace-labs/slitter/blob/7afb9781fd25b8cee62afa555b9d38f391131044/src/magazine.rs)
4. pushes and pops objects into/from [thread-local magazines](https://github.com/backtrace-labs/slitter/blob/7afb9781fd25b8cee62afa555b9d38f391131044/c/cache.c)
5. caches populated magazines in global [type-specific lock-free stacks](https://github.com/backtrace-labs/slitter/blob/7afb9781fd25b8cee62afa555b9d38f391131044/src/class.rs#L62-L67)
6. manages empty magazines with a global [mostly lock-free `Rack`](https://github.com/backtrace-labs/slitter/blob/7afb9781fd25b8cee62afa555b9d38f391131044/src/rack.rs)

Many general purpose memory allocators implement strategies similarly
inspired by [Bonwick's slab allocator](https://www.usenix.org/legacy/publications/library/proceedings/usenix01/full_papers/bonwick/bonwick.pdf),
and time-tested mallocs may well provide better performance
and lower fragmentation than Slitter.[^type-stable]
The primary motivation for designing Slitter is that having explicit
allocation classes in the API makes it easier for the allocator to
improve the debuggability and resilience of the calling program.
For example, most allocators can tell you the size of your program's
heap, but that data is much more useful when broken down by struct
type or program module.

[^type-stable]: In fact, Slitter actively worsens external fragmentation to guarantee type-stable allocations, and thus control the blast radius of use-after-frees and double-frees.

Most allocators try to avoid accessing the metadata associated with
allocations.  In fact, that's often seen as a strength of the slab
interface: the allocator can just rely on the caller to pass the
correct allocation class tag, instead of hitting metadata to figure
out there the freed address should go.

We went in the opposite direction with Slitter.  We still rely on the
allocation class tag for speed, but also actively look for mismatches
before returning from deallocation calls. Nothing depends on
values computed by the mismatch detection logic, and the resulting
branch is trivially predictable (the tag always matches), so we can
hope that wide out-of-order CPUs will hide most of the checking
code, if it's simple enough.

This concern (access to metadata in few instructions) combined with our goal of
avoiding in-band metadata lead to a
[simple layout for each chunk's data and metadata](https://github.com/backtrace-labs/slitter/blob/7afb9781fd25b8cee62afa555b9d38f391131044/src/mill.rs#L6-L19).

```
.-------.------.-------|---------------.-------.
| guard | meta | guard | data ... data | guard |
'-------'------'-------|---------------'-------'
  2 MB    2 MB   2 MB  |      1 GB        2 MB
                       v
               Aligned to 1 GB
```

A chunk's data is always a 1 GB address range, aligned to 1 GB: the
underlying mapper doesn't have to immediately back that with memory,
but it certainly can, e.g., in order to use gigantic pages.  The chunk
is preceded and followed by 2 MB guard pages.  The metadata for the
chunk's data lives in a 2 MB range, just before the preceding guard
page (i.e., 4 MB to 2 MB before the beginning of the aligned 1 GB
range).  Finally, the 2 MB metadata range is itself preceded by a 2MB
guard page.

Each chunk is statically divided in 65536 spans of 16 KB each.  We can
thus map a span to its slot in the metadata block with a couple shift,
masks, and some address arithmetic.  [Mill](https://github.com/backtrace-labs/slitter/blob/7afb9781fd25b8cee62afa555b9d38f391131044/src/mill.rs)s
don't have to hand out individual 16 KB spans at a time, they simply
have to make sure to work with multiples of 16 KB, and never split
a span in two.

Why we wrote Slitter
--------------------

We decided to actually code up Slitter last April, when we noticed
that we would immediately benefit from backing allocation with
temporary file mappings: the bulk of our data is directly mapped from
persistent data files, but we also regenerate some cold metadata
during startup, and accesses to that metadata have amazing locality,
both temporal and spatial (assuming bump allocation).  We don't want the OS
to swap out all the heap--that way lie [grey failures](https://blog.acolyer.org/2017/06/15/gray-failure-the-achilles-heel-of-cloud-scale-systems/)--so
we opt specific allocation classes into it.

By itself, this isn't a reason to write a slab allocator: we could
easily have configured [specialised arenas in jemalloc](http://jemalloc.net/jemalloc.3.html#arena.i.extent_hooks),
for example.  However, we also had eyes on longer term improvements to
observability and debugging/mitigation of memory management errors in
production, and those could only be unlocked by switching to an
interface with explicit allocation classes (types).

Slab allocators tend to focus exclusively on speed.  [Forks of libumem](https://github.com/omniti-labs/portableumem)
may be the exception, when they inherit Solaris's culture of pervasive
hooking.  However, `umem`'s design reflects the sensibilities of the
00s, when it was written: threads share a few caches, and the
allocator tries to reuse address space, while Slitter assumes memory
is plentiful enough for thread-local caches and type-stable
allocations.

Why we (mostly) wrote Slitter in Rust
-------------------------------------

We call Slitter from C, but wrote it in Rust, despite the
more painful build[^uber-crate] process: that pain isn't going
anywhere, since we expect our backend to be in a mix of C, C++, and
Rust for a long time.  We also sprinkled in some C when the
alternative would have been to pull in a crate just to make a couple
syscalls, or to enable unstable Rust features: we're not
"rewrite-it-in-Rust" absolutists, and merely wish to use Rust for its
strengths (control over data layout, support for domain-specific
invariants, large ecosystem for less performance critical logic, ability to
lie to the compiler where necessary, ...), while avoiding its
weaknesses (interacting with Linux interfaces defined by C headers or
fine-tuning code generation).

[^uber-crate]: We re-export our dependencies from an uber-crate, and let our outer [meson](https://mesonbuild.com/) build invoke `cargo` to generate a static library for that facade uber-crate.

The majority of allocations only interact with the thread-local
magazines.  That's why we [wrote that code in C](https://github.com/backtrace-labs/slitter/blob/main/c/cache.c):
stable Rust doesn't (yet) let us access [likely/unlikely annotations](https://doc.rust-lang.org/std/intrinsics/fn.likely.html),
nor [fast "initial-exec"](https://www.akkadia.org/drepper/tls.pdf#page=35) [thread-local storage](https://github.com/rust-lang/rust/issues/29594).
Of course, allocation and deallocation are the main entry points into
a memory allocation library, so this creates a bit of friction with
Rust's linking process.[^bad-linker]

[^bad-linker]: [Rust automatically hides foreign symbols when linking `cdylib`s](https://github.com/rust-lang/rfcs/issues/2771).  We worked around that with static linking, but statically linked rust libraries are mutually incompatible, hence the uber-crate.

We also had to implement our [lock-free multi-popper Treiber stack](https://github.com/backtrace-labs/slitter/blob/main/c/stack.c)
in C: x86-64 doesn't have anything like LL/SC, so we instead pair
the top-of-stack pointer with a generation counter... and 
[Rust hasn't stabilised 128-bit atomics](https://github.com/rust-lang/rust/issues/32976#issuecomment-641360955) yet.

We chose to use atomic in C instead of a simple lock in Rust because
the lock-free stack (and the bump pointer, which Rust handles fine)
are important for our use case: when we rehydrate cold metadata at
startup, we do so from multiple I/O-bound threads, and we have
observed hiccups due to lock contention in malloc.  At some point,
lock acquisitions are rare enough that contention isn't an issue;
that's why we rely on locks when refilling bump allocation regions.

Come waste performance on safety!
---------------------------------

A recurring theme in the design of [Slitter](https://github.com/backtrace-labs/slitter)
is that we find ways to make the core (de)allocation logic slighly
faster, and immediately spend that efficiency on safety, debuggability
or, eventually, observability.  For a lot of code, performance is a
constraint to satisfy, not a goal to maximise; once we're close to
good enough, it makes sense to trade performance
away.[^even-works-for-perf] I also believe that there are
[lower hanging fruit in memory placement](https://research.google/pubs/pub50370/)
than shaving a couple nanos from the allocation path.

[^even-works-for-perf]: And not just for safety or productivity features!  I find it often makes sense to give up on small performance wins (e.g., aggressive autovectorisation or link-time optimisation) when they would make future performance investigations harder.  The latter are higher risk, and only potential benefits, but their upside (order of magnitude improvements) dwarfs guaranteed small wins that freeze the code in time.

[Slitter](https://crates.io/crates/slitter) also focuses on
instrumentation and debugging features that are always running, even in
production, instead of leaving that to development tools, or to logic
that must be explicily enabled.  In a SaaS world, development and
debugging is never done.  Opt-in tools are definitely useful, but
always-on features are much more likely to help developers to catch
the rarely occurring bugs on which they tend to spend an inordinate
amount of investigation effort (and if a debugging feature can be
enabled in production at a large scale, why not leave it enabled
forever?).

If that sounds like an interesting philosophy for a slab allocator,
[come hack on Slitter](https://github.com/backtrace-labs/slitter)!
Admittedly, the value of Slitter isn't as clear for pure Rust hackers
as it is for those of us who mix C and Rust, but per-class allocation
statistics and placement decisions should be useful, even in safe
Rust, especially for larger programs with long runtimes.

Our [MIT-licensed code is on github](https://github.com/backtrace-labs/slitter),
there are [plenty of small improvements to work on](https://github.com/backtrace-labs/slitter/issues),
and, while we still have to re-review the documentation, it has decent
test coverage, and we try to write straightforward code.

<p><hr style="width: 50%"></p>
