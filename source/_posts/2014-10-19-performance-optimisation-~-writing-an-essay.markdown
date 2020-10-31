---
layout: post
title: "Performance tuning ~ writing an essay"
date: 2014-10-19 20:05
comments: true
categories: appnexus
---

<a href="#trust-no-one">Skip to the meaty bits.</a>

My work at [AppNexus](http://www.appnexus.com/) mostly involves
performance optimisation, at any level from microarchitecture-driven
improvements to data layout and assembly code to improving the
responsiveness of our distributed system under load.  Technically,
this is similar to what I was doing as a lone developer on
research-grade programs.  However, the scale of our (constantly
changing) code base and collaboration with a dozen other coders mean
that I approach the task differently: e.g., rather than
single-mindedly improving throughput *now*, I aim to pick an evolution
path that improves throughput today without imposing too much of a
burden on future development or fossilising ourselves in a design
dead-end.  So, although numbers still don't lie (hah), my current
approach also calls for something like judgment and taste, as well as
a fair bit of empathy for others.  Rare are the obviously correct
choices, and, in that regard, determining what changes to make and
which to discard as
[over-the-top ricing](http://fun.irq.dk/funroll-loops.org/) feels like
I'm drafting a literary essay.

This view is probably tainted by the fact that, between English and
French classes, I spent something like half of my time in High School
critiquing essays, writing essays, or preparing to write one.
Initially, there was a striking difference between the two languages:
English teachers had us begin with the five paragraph format where one
presents multiple arguments for the same thesis, while French teachers
imposed a thesis/antithesis/synthesis triad (and never really let it
go until CÉGEP, but that's another topic).  When I write that
performance optimisation feels like drafting essays, I'm referring to
the latter "Hegelian" process, where one exposes arguments and
counterarguments alike in order to finally make a stronger case.

I'll stretch the analogy further.  Reading between the lines gives us
access to more arguments, but it's also easy to get the context wrong and
come up with hilariously far-fetched interpretations.  When I try to
understand a system's performance, the most robust metrics treat the
system as a black box: it's hard to get throughput under production
data wrong.  However, I need finer grained information (e.g.,
performance counters, instruction-level profiling, or
application-specific metrics) to guide my work, and, the more useful
that information can be -- like domain specific metrics that highlight
what we could do differently rather than how to do the same thing more
efficiently -- the easier it is to measure incorrectly.  That's not a
cause for despair, but rather a fruitful line of skepticism that helps
me find more opportunities.

Just two weeks ago, questioning our application-specific metrics
lead to an easy 10% improvement in throughput for our biggest
consumer of CPU cycles.  The consumer is an application that
determines whether internet advertising campaigns are eligible to bid
on an ad slot, and if so, which creative (ad) to show and at what bid
price.  For the longest time, the most time-consuming part of that
process was the first step, testing for campaign eligibility.
Consequently, we tracked the execution of that step precisely and
worked hard to minimise the time spent on ineligible campaigns,
without paying much attention to the rest of the pipeline.  However,
we were clearly hitting diminishing returns in that area, so I asked
myself how an adversary could use our statistics to mislead us.  The
easiest way I could think of was to have campaigns that are eligible
to bid, but without any creative compatible with the ad slot (e.g.,
because it's the wrong size or because the website forbids Flash ads):
although the campaigns are technically eligible, they are unable to
bid on the ad slot.  We added code to track these cases and found that
almost half of our "eligible" campaigns simply had no creative in the
right size.  Filtering these campaigns early proved to be a
low-hanging fruit with an ideal code complexity:performance
improvement ratio.

<a href="#trust-no-one" name="trust-no-one">Trust no one, not even performance counters</a>
===========================================

I recently learned that we also had to second-guess instruction level
profiles.  Contemporary x86oids are out of order, superscalar, and
speculative machines, so profiles are always messy: "blame" is
scattered around the real culprit, and some instructions (pipeline
hazards like conditional jumps and uncached memory accesses, mostly)
seem to account for more than their actual share.  What I never
realised is that, in effect, some instructions systematically mislead
and push their cycles to others.

Some of our internal spinlocks use `mfence`.  I expected that to be
suboptimal, since it's
[common](https://blogs.oracle.com/dave/resource/NHM-Pipeline-Blog-V2.txt)
[knowledge](http://shipilev.net/blog/2014/on-the-fence-with-dependencies/)
that `lock`ed instruction are more efficient barriers: serialising
instructions like `mfence` have to affect streaming stores and other
weakly ordered memory accesses, and that's a lot more work than just
preventing store/load reordering.  However, our profiles showed that
we spent very little time on spinlocking so I never gave it much thought…
until eliminating a set of spinlocks had a much better impact on
performance than I would have expected from the profile.  Faced with
this puzzle, I had to take a closer look at the way `mfence` and
locked instructions affect hardware-assisted instruction profiles on
our production Xeon E5s (Sandy Bridge).

I came up with a simple synthetic microbenchmark to simulate locking
on my E5-4617: the loop body is an adjustable set of memory accesses
(reads and writes of out-of-TLB or uncached locations) or computations
(divisions) bracketed by pairs of normal stores, `mfence`, or `lock
inc/dec` to cached memory (I would replace the fences with an
increment/decrement pair and it looks like all read-modify-write
instructions are implemented similarly on Intel).  Comparing runtimes
for normal stores with the other instructions helps us gauge their
overhead.  I can then execute each version under `perf` and estimate
the overhead from the instruction-level profile.  If `mfence` is
indeed extra misleading, there should be a greater discrepancy between
the empirical impact of the `mfence` pair and my estimate from the
profile.

You can find the
[super crufty code here](/images/2014-10-19-performance-optimisation-~-writing-an-essay/fence.c),
[along with an `rdtscp` version of cycle.h](/images/2014-10-19-performance-optimisation-~-writing-an-essay/cycle.h).

With `lock`ed instructions and random reads that miss the L3 cache,
the (cycle) profile for the microbenchmark loop is:
{% codeblock %}
$ perf annotate -s cache_misses
[...]
    0.06 :        4006b0:       and    %rdx,%r10
    0.00 :        4006b3:       add    $0x1,%r9
    ;; random (out of last level cache) read
    0.00 :        4006b7:       mov    (%rsi,%r10,8),%rbp
   30.37 :        4006bb:       mov    %rcx,%r10
    ;; foo is cached, to simulate our internal lock
    0.12 :        4006be:       mov    %r9,0x200fbb(%rip)        # 601680 <foo>
    0.00 :        4006c5:       shl    $0x17,%r10
    [... Skipping arithmetic with < 1% weight in the profile]
    ;; locked increment of an in-cache "lock" byte
    1.00 :        4006e7:       lock incb 0x200d92(%rip)        # 601480 <private+0x200>
   21.57 :        4006ee:       add    $0x1,%rax
    [...]
    ;; random out of cache read
    0.00 :        400704:       xor    (%rsi,%r10,8),%rbp
   21.99 :        400708:       xor    %r9,%r8
    [...]
    ;; locked in-cache decrement
    0.00 :        400729:       lock decb 0x200d50(%rip)        # 601480 <private+0x200>
   18.61 :        400730:       add    $0x1,%rax
    [...]
    0.92 :        400755:       jne    4006b0 <cache_misses+0x30>
{% endcodeblock %}

Looking at that profile, I'd estimate that the two random reads
account for ~50% of runtime, and the pair of `lock inc/dec` for ~40%.

The picture is completely different for `mfence`.

{% codeblock %}
$ perf annotate -s cache_misses
[...]
    0.00 :        4006b0:       and    %rdx,%r10
    0.00 :        4006b3:       add    $0x1,%r9
    ;; random read
    0.00 :        4006b7:       mov    (%rsi,%r10,8),%rbp
   42.04 :        4006bb:       mov    %rcx,%r10
    ;; store to cached memory (lock word)
    0.00 :        4006be:       mov    %r9,0x200fbb(%rip)        # 601680 <foo>
    [...]
    0.20 :        4006e7:       mfence 
    5.26 :        4006ea:       add    $0x1,%rax
    [...]
    ;; random read
    0.19 :        400700:       xor    (%rsi,%r10,8),%rbp
   43.13 :        400704:       xor    %r9,%r8
    [...]
    0.00 :        400725:       mfence 
    4.96 :        400728:       add    $0x1,%rax
    0.92 :        40072c:       add    $0x1,%rax
    [...]
    0.36 :        40074d:       jne    4006b0 <cache_misses+0x30>
{% endcodeblock %}

It looks like the loads from uncached memory represent ~85% of the
runtime, while the `mfence` pair might account for *at most* ~15%, if
I include all the noise from surrounding instructions.

If I trusted the profile, I would worry about eliminating `lock`ed
instructions, but not so much for `mfence`.  However, runtimes (in
cycles), which is what I'm ultimately interested in, tell a different
story.  The same loop of LLC load misses takes 2.81e9 cycles for 32M
iterations without any atomic or fence, versus 3.66e9 for `lock
inc/dec` and 19.60e9 cycles for `mfence`.  So, while the profile for
the `mfence` loop would let me believe that only ~15% of the time is
spent on synchronisation, the `mfence` pair really represents 86%
\\(((19.6 - 2.81) / 19.6)\\) of the runtime for that loop!  Inversely,
the profile for the `lock`ed pair would make me guess that we spend
about 40% of the time there, but, according to the timings, the real
figure is around 23%.

The other tests all point to the same conclusion: the overhead of
`mfence` is strongly underestimated by instruction level profiling,
and that of `lock`ed instructions exaggerated, especially when
adjacent instructions write to memory.

      setup     cycles   (est. overhead)  ~actual overhead
    
    div [ALU] (100 Mi iterations)
     atomic: 20153782848   (20%)          ~ 3.8%
     mfence: 28202315112   (25%)          ~31.3%
    vanilla: 19385020088
    
    Reads:
    
    TLB misses (64Mi iterations)
     atomic:  3776164048   (80%)          ~39.3%
     mfence: 12108883816   (50%)          ~81.1%
    vanilla:  2293219400 
    
    LLC misses (32Mi iterations)
     atomic:  3661686632   (40%)          ~23.3%
     mfence: 19596840824   (15%)          ~85.7%
    vanilla:  2807258536
    
    Writes:
    
    TLB (64Mi iterations)
     atomic:  3864497496   (80%)          ~10.4%
     mfence: 13860666388   (50%)          ~75.0%
    vanilla:  3461354848
    
    LLC (32Mi iterations)
     atomic:  4023626584   (60%)          ~16.9%
     mfence: 21425039912   (20%)          ~84.4%
     vanilla: 3345564432

I can guess why we observe this effect; it's not like Intel is
intentionally messing with us.  `mfence` is a full pipeline flush: it
slows code down because it waits for all in-flight instructions to
complete their execution.  Thus, while it's flushing that slows us
down, the profiling machinery will attribute these cycles to the
instructions that are being flushed.  Locked instructions instead
affect stores that are still queued.  By forcing such stores to
retire, locked instructions become responsible for the extra cycles
and end up "paying" for writes that would have taken up time anyway.

Losing faith in hardware profiling being remotely representative of
reality makes me a sad panda; I now have to double check `perf`
profiles when hunting for misleading metrics.  At least I can tell
myself that knowing about this phenomenon helps us make better
informed -- if less definite -- decisions and ferret out more easy
wins.

P.S., if you find this stuff interesting, feel free to send an email
(pkhuong at $WORK.com).  My team is hiring both experienced developers
and recent graduates (:
