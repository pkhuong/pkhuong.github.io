---
layout: post
title: "Too much locality... for stores to forward"
date: 2020-02-01 17:29:21 -0500
comments: true
categories: PerfAnalysis
---

<small>Apologies for the failed [Cake reference](https://www.youtube.com/watch?v=K3DRkVjuqmc).<br/>
2020-02-02: Refer to Travis Downs's investigation into this pathological case for forwarding.</small>

I've been responsible for [Backtrace.io](https://backtrace.io/)'s crash analytic database[^started-work] for a couple months now.
I have focused my recent efforts on improving query times for in-memory grouped aggregations, i.e.,
the archetypal MapReduce use-case where we generate key-value pairs, and [fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) over the values
for each key in some [(semi)](https://en.wikipedia.org/wiki/Semigroup)[group](https://en.wikipedia.org/wiki/Group_(mathematics)).
We have a cute cache-efficient data structure for this type of workload;
the inner loop simply inserts in a small hash table with [Robin Hood linear probing](/Blog/more_numerical_experiments_in_hashing.html),
in order to guarantee entries in the table are ordered by hash value.  This
ordering lets us easily dump the entries in sorted order, and [block](https://www2.eecs.berkeley.edu/Pubs/TechRpts/1993/6309.html) the merge loop for an arbitrary number of sorted arrays
into a unified, larger, ordered hash table (which we can, again, dump to a sorted array).[^more-later]

[^started-work]: And by that, I mean I started working there a couple months ago (:

[^more-later]: I think that's a meaty idea, and am planning a longer post on that data structure and where it fits in the hash/sort join continuum.

Observation
===========

As I updated more operators to use this data structure, I noticed that we were spending a lot of time in its inner loop.
In fact, [perf](http://www.brendangregg.com/linuxperf.html) showed that the query server as a whole was spending 4% of its CPU time on one instruction in that loop:

     2.17 |       movdqu     (%rbx),%xmm0
    39.63 |       lea        0x1(%r8),%r14  # that's 40% of the annotated function
          |       mov        0x20(%rbx),%rax
     0.15 |       movaps     %xmm0,0xa0(%rsp)

The first thing to note is that instruction-level profiling tends to put the blame on the instruction *following* the one that triggered a sampling interrupt.
It's not the `lea` (which computes `r14 <- r8 + 1`) that's slow, but the `movdqu` just before.
So, what is that `movdqu` loading into `xmm0`?  Maybe it's just a normal cache miss, something inherent to the workload.

I turned on source locations [(hit `s` in `perf report`)](http://man7.org/linux/man-pages/man1/perf-report.1.html), and observed that this instruction was simply copying to the stack an argument that was passed by address.
The source clearly showed that the argument should be hot in cache: the inner loop was essentially

    A1. Generate a new key-value pair
    B1. Mangle that kv pair just a bit to turn it into a hash element
    C1. Insert the new hash element
    A2.
    B2.
    C2.

and the `movdqu` happens in step C, to copy the element that step B just constructed.[^dont-copy]

[^dont-copy]: Would I have avoided this issue if I had directly passed by value? The resulting code might have been friendlier to store-to-load forwarding than loading a whole 128 bit SSE register, but see the next footnote.

At this point, an important question suggests itself: does it matter?
We could simply increase the size of the base case and speed up the rest of the bottom-up recursion… eventually, the latency for the random accesses in the initial hash table will dominate the inner loop.

When I look into the performance of these deep inner loop, my goal isn't only to do the same thing better.
The big wins, in my experience, come from the additional design freedom that we get from being able to find new uses for the same code.
Improved latency, throughput, or memory footprint really shine when the increased optionality from multiple such improvements compounds and lets us consider a much larger design space for the project as a whole.
That's why I wanted to make sure this hash table insertion loop worked on as wide a set of parameter as possible: because that will give future me the ability to combine versatile tools.

Hypothesis
==========

Back to the original question. Why do we spend so many cycles loading data we just wrote to cache?

The answer is in the question and in the title of this post: too little time elapses between the instructions that write data to the cache and the ones that read the same data.[^but-forwarding]
A modern out-of-order machine (e.g., most amd64 chips) can execute multiple instructions at the same time, and will start executing instructions as soon as their operands are ready, even when earlier instructions in program order are still waiting for theirs.
Machine code is essentially a messy way to encode a [dataflow graph](https://fgiesen.wordpress.com/2018/03/05/a-whirlwind-introduction-to-dataflow-graphs/), 
which means our job as micro-optimisers is, at a high level, to avoid long dependency chains and make the dataflow graph as wide as possible.
When that's too hard, we should distribute as much scheduling slack as possible between nodes in a chain, in order to absorb the knock-on effects of cache misses and other latency spikes.
If we fail, the chip will often find itself with no instruction ready to execute; stalling the pipeline like that is like slowing down by a factor of 10.

[^but-forwarding]: Store-to-load forwarding can help improve the performance of this pattern, when we use forwarding patterns that the hardware supports. However, this mechanism can only decrease the penalty of serial dependencies, e.g., by shaving away some or all of the time it takes to store a result to cache and load it back; even when results can feed directly into dependencies, we still have to wait for inputs to be computed. This is fundamentally a scheduling issue.

The initial inner loop simply executes steps A, B, and C in order, where step C depends on the result of step B, and step B on that of step A.
In theory, a chip with a wide enough instruction reordering window could pipeline multiple loop iterations.
In practice, real hardware can only [plan on the order of 100-200 instructions ahead](http://blog.stuffedcow.net/2013/05/measuring-rob-capacity/), and that mechanism depends on branches being predicted correctly.
We have to explicitly insert slack in our dataflow schedule, and we must distribute it well enough for instruction reordering to see the gaps.

This specific instance is a particularly bad case for contemporary machines:
step B populates the entry with regular (64-bit) register writes,
while step C copies the same bytes with vector reads and writes.
[Travis Downs looked into this forwarding scenario](https://gist.github.com/travisdowns/bc9af3a0aaca5399824cf182f85b9e9c) and found that no other read-after-write setup behaves this badly, on Intel or AMD.
That's probably why the `movdqu` vector load instruction was such an issue.
If the compiler had emitted the copy with GPR reads and writes,
that *might* have been enough for the hardware to hide the latency.
However, as [Travis points out on Twitter](https://twitter.com/trav_downs/status/1223766684932222976), it's hard for a compiler to get that right across compilation units.
In any case, our most reliable (more so than passing this large struct by value and hoping the compiler will avoid mismatched instructions) and powerful tool to fix this at the source level is to schedule operations manually.

The dataflow graph for each loop iteration is currently a pure chain:

             A1
             |
             v
             B1
             |
             v
             C1
                    A2
                    |
                    v
                    B2
                    |
                    v
                    C2

How does one add slack to these chains? With bounded queues!

Experiment
==========

My first fix was to add a one-element buffer between steps B and C.  The inner loop became

    A1. Generate a new key-value pair
    C0. Insert the hash element from the previous iteration
    B1. Mangle the kv pair and stash that in the buffer
    A2.
    C1.
    B2
    etc.

which yields a dataflow graph like

            |     A1
            v     |
            C0    |
                  |
                  v
                  B1
                  |
                  |     A2
                  v     |
                  C1    |
                        |
                        v
                        B2
                        |

We've introduced slack between steps A and B (there's now step C from the previous iteration between them), and between steps B and C (we shifted step A from the next iteration between them).
There isn't such a long delay between the definition of a value and its use that the data is likely to be evicted from L1.
However, there is more than enough work available between them to keep the pipeline busy with useful work while C waits for B's result, or B for A's.
That was a nice single-digit improvement in query latency for my internal benchmark, just by permuting a loop.

If a one-element buffer helps, we should clearly experiment with the buffer size, and that's where I found a more impactful speed-up.
Once we have an array of elements to insert in a hash table, we can focus on a bulk insert of maybe 8 or 10 elements: instead of trying to improve the latency for
individual writes, we can focus on the throughput for multiple inserts at once.
That's good because [throughput is an easier problem than latency](http://www.stuartcheshire.org/rants/Latency.html).
In the current case, passing the whole buffer to the hash table code made it easier to [pipeline the insert loop in software](https://www2.eecs.berkeley.edu/Pubs/TechRpts/1993/6309.html):
we can compute hashes ahead of time, and accelerate random accesses to the hash table with [software prefetching](https://software.intel.com/sites/landingpage/IntrinsicsGuide/#text=_mm_prefetch&expand=4391).
The profile for the new inner loop is flatter, and the hottest part is as follows

          |       mov        0x8(%rsp),%rdx
     9.91 |       lea        (%r12,%r12,4),%rax
     0.64 |       prefetcht0 (%rdx,%rax,8)
    17.04 |       cmp        %rcx,0x28(%rsp)
   
Again, the blame for a "slow" instruction hits the following instruction, so it's not `lea` (multiplying by 5) or `cmp` that are slow; it's the load from the stack and the prefetch.
The good news is that these instructions do not have any dependent.  It's all prefetching, and that's only used for its side effects.
Moreover, they come from a block of code that was pipelined in software and executes one full iteration ahead of where its side effects might be useful.
It doesn't really matter if these instructions are slow: they're still far from being on the critical path!  This last restructuring yielded a 20% speed-up on a few slow queries.

I described two tools that I use regularly when optimising code for contemporary hardware.
Finding ways to scatter around scheduling slack is always useful, both in software and in real life planning.[^unless-people]
One simple way to do so is to add bounded buffers, and to flush buffers as soon as they fill up (or refill when they become empty), instead of waiting until the next write to the buffer.
However, I think the more powerful transformation is using buffering to expose bulk operations, which tends to open up more opportunities than just doing the same thing in a loop.
In the case above, we found a 20% speed-up; for someone who visit their [Backtrace dashboard](https://help.backtrace.io/en/articles/2765535-triage) a couple times a day, that can add up to an hour or two at the end of the year.

TL;DR: When a function is hot enough to look into, it's worth asking why it's called so often, in order to focus on higher level bulk operations.

[^unless-people]: Unless you're writing schedule optimising software and people will look at the result. A final hill climbing pass to make things look artificially tight often makes for an easier sale in that situation.

<p><hr style="width: 50%"></p>
