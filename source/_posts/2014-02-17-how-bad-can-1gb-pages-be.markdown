---
layout: post
title: "How bad can 1GB pages be?"
date: 2014-02-18 00:06
comments: true
categories: appnexus
---

I joined the ad server team at [AppNexus](http://www.appnexus.com/)
two weeks ago.  It's a new problem domain for me, one that I find
pretty interesting so far: the workload is definitely on the
branchy/pointer-chasing side of things, and, although aggregate
throughput matters, we work under stringent latency goals.  There are
a few hash table lookups on the code path I've been working on, and
some micro-benchmarking revealed that 1GB pages could easily shave 25%
off the latency for lookups in large (hundreds of MB) tables.

That's not too surprising.  Given any change, it's usually easy to
design a situation that makes it shine.  I find it just as important to
ask the opposite question: what's the worst slowdown I can get from
switching from regular 4KB pages to huge 1GB pages?  This way, I can
make more robust decisions, by taking into account both a good (if not
best) and a worst case. By the end of this post, I'll multiply runtime
by 250% for the same operation, simply by switching from 4KB to 1GB
pages… with a setup is so contrived that it seems unlikely to occur by
accident.

But first, a quick overview of the benefits and downsides of huge
pages.

Why are huge pages interesting?
===============================

In short, manufacturers are adding huge pages because translating
virtual addresses to physical ones is slow.

On x86-64, the mapping from virtual to physical pages is represented
with a trie; each level dispatches on 9 bits (i.e., each node has 512
children), and leaves correspond to 4KB pages. There are 4 levels from
the root node to the leaves, which covers the (currently) standard 48-bit
virtual address space.

The address translation table is (mostly) stored in normal memory and
is too large to fit in cache. Thus, translating a virtual address
requires four reads, any of which can hit uncached memory.

This is where the translation lookaside buffer (TLB) comes in. On my
E5-4617, each core has 64 entries for regular 4KB pages in its L1
dTLB, and 512 (shared) entries in its L2 TLB. I don't know if the TLBs
are exclusive or inclusive, but even if they're exclusive, that's only
enough for 2.25MB worth of data. Assuming that the working set is
completely contiguous (i.e., the best case), the TLB space for 4KB
pages is less than the total cache/core (2.5MB L3/core + 256 KB L2 +
32 KB L1D).

2MB and 1GB "huge pages" address this imbalance: nine 2MB pages
suffice to cover more address space than all the caches in a 6-core
E5-4617. However, there are only 32 L1dTLB entries for 2MB pages --
and 4 entries for 1GB pages -- on my E5.

In addition to covering more address space in the TLB, huge pages
offer secondary benefits: there are fewer page table entries, and the
trie is shallower. Fewer page table entries means that a larger
fraction of memory can be used by data, rather than metadata, and that
the page table walk is more likely to stay in cache. Moreover, larger
pages are closer to the trie's root: while the processor traverses 4
levels to get to a 4KB page, it only traverses 3 levels to reach a 2MB
page and 2 levels to for a 1GB page. These two effects compound to
make TLB misses quicker to handle.

Now, the downsides…
====================

This idea that one must cover as much address space as possible with
the TLB is most relevant in two settings: trivially, if the working
set is completely covered by the (L1d)TLB, or, more interestingly,
when the access patterns show a lot of locality.  Examples of the
latter case are BLAS routines: with appropriate blocking, they can
usually access each page once or twice, but read almost every byte in
a page before switching to the next.

The opposite, worst, case would be something like lookups in a large
(too big for the TLB) hash table: we choose a virtual address
pseudorandomly and painstakingly translate it to a physical address,
only to read a handful of words from that page. In that situation, we
want as many TLB entries as possible, regardless of the address space
each one covers… and that's where 4KB pages ought to shine. Taking
into account both the L1DTLB and the L2TLB, each core has 576 TLB
entries for 4KB (data) pages, versus 64x2MB and 4x1GB. Now, I don't
know if the TLBs are exclusive or not, so I'll assume the worst case
and work with 512*4KB entries.

The thing is, 512 TLB entries aren't that many. If, by chance, our
hash table lookups keep hitting the same 512 pages, a contrived
microbenchmark will show that 4KB pages are a big win (but really, a
software cache might be a better way to exploit the situation). It's
more likely that it'll be hard to avoid TLB misses regardless of page
size, and huge pages then become useful because each TLB miss is
handled more quickly. Regardless, I'll try to approximate this
worst-case behaviour to see how bad things can get.

A first stab at pessimising huge pages
======================================

Ideally, I would want to read from 512 (or a bit fewer) locations 1GB
apart, but I don't have that much RAM. In the interest of realism, I
decided to "only" allocate 24GB.

My first microbenchmark follows: I allocate 24GB, divide that space in
512 chunks, and read the first word of each chunk in a loop. At first,
I didn't even randomise the traversal order (so as to abuse LRU), but
there seems to be some prefetching for 1GB pages.

{% codeblock %}
#define _GNU_SOURCE
#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>

#include "cycle.h"

#ifndef MAP_HUGETLB
# define MAP_HUGETLB 0x40000
#endif

#ifndef MAP_HUGE_1GB
# define MAP_HUGE_1GB (30 << 26)
#endif

#if defined(ONE_G)
# define FLAGS MAP_ANONYMOUS | MAP_PRIVATE | MAP_HUGETLB | MAP_HUGE_1GB
#elif defined(TWO_M)
# define FLAGS MAP_ANONYMOUS | MAP_PRIVATE | MAP_HUGETLB
#else
# define FLAGS MAP_ANONYMOUS | MAP_PRIVATE
#endif

int main (int argc, char **argv)
{
        (void)argc;
        (void)argv;

        char acc = 0;
        size_t stride = (24ul << 30)/512;
        char *data = mmap(NULL, 24ul << 30,
                          PROT_READ | PROT_WRITE, FLAGS,
                          -1, 0);
        assert(data != MAP_FAILED);
        memset(data, 0, 24ul << 30);

        size_t *indices = calloc(1ul<<20, sizeof(size_t));
        for (size_t i = 0; i < 1ul<<20; i++) {
                size_t x = 512.0*random()/RAND_MAX;
                indices[i] = x*stride;
        }

        ticks begin = getticks();
        for (size_t i = 0; i < 1ul << 7; i++) {
                for (size_t j = 0; j < 1ul<<20; j++) {
                        acc += data[indices[j]];
                }
        }
        ticks end = getticks();

        printf("%g %i\n", elapsed(end, begin), acc);

        return acc;
}
{% endcodeblock %}

The results: 1.13e10 cycles for 4KB pages, 1.60e10 for 2MB and 1.56e10
for 1GB. That's only 40% more cycles… it's bad, but not horrible. The
reason is that the data vector spans only 24x1GB, so 1/6th of the
random lookups will hit the 1GB TLB. Instead, let's try and load from
each of these 24 pages, in random order. 24 pages will easily fit in
the L1DTLB for 4KB pages, but not in the 4 slots for 1GB pages.

Takes two to six
================

{% codeblock %}
#define NCHUNKS 24

int main (int argc, char **argv)
{
        (void)argc;
        (void)argv;

        char acc = 0;
        size_t stride = (24ul << 30)/NCHUNKS;
        char *data = mmap(NULL, 24ul << 30,
                          PROT_READ | PROT_WRITE, FLAGS,
                          -1, 0);
        assert(data != MAP_FAILED);
        memset(data, 0, 24ul << 30);

        size_t *indices = calloc(1ul<<20, sizeof(size_t));
        for (size_t i = 0; i < 1ul<<20; i++) {
                size_t x = NCHUNKS*random()/RAND_MAX;
                indices[i] = (x*stride) % (24ul << 30);
        }

        ticks begin = getticks();
        for (size_t i = 0; i < 1ul << 7; i++) {
                for (size_t j = 0; j < 1ul<<20; j++) {
                        acc += data[indices[j]];
                }
        }
        ticks end = getticks();

        printf("%g %i\n", elapsed(end, begin), acc);
        return acc;
}
{% endcodeblock %}

The results are even worse (better)! 4.82e9 cycles for 4KB pages,
versus 3.96e9 and 2.84e9 for 2MB and 1GB pages!

The problem is aliasing. The TLB on my E5 has limited way-ness (4-way,
I believe), so, by aligning everything to a 1GB boundary, the
effective size of the 4KB page TLB is 4 entries (same for 2MB). In a
way, this highlights the effect of page size when TLBs are useless
(random accesses to dozens or hundreds of GBs): 2MB pages shave 18%
off the runtime, and 1GB pages another 30%, for a total of 60% as much
time to handle a 1GB TLB miss versus 4KB.

Let's try again, with `indices[i] = (x*stride + (x*4096)%(1ul<<30)) % (24ul << 30);` on line 19.  I now find 1.14e9, 6.18e9 and 2.65e9 cycles. Much better!

For fun, I also tried to offset by 2MB increments, with `indices[i] =
(x*stride + (x<<21)%(1ul<<30)) % (24ul << 30);`, and found 2.76e9,
1.30e9, and 2.85e9 cycles.

Finally, I tried

                size_t offset = 4096 + (1ul<<21);
                indices[i] = (x*stride + (x*offset)%(1ul<<30)) % (24ul << 30);

so that neither 4KB nor 2MB pages would alias, and got 1.13e9, 1.08e9
and 2.65e9 cycles. That's 234% as much time for 1GB pages as for 4KB.

We're close: this setup is such that 1GB pages cause a lot of TLB
misses, but neither 4KB nor 2MB pages do.  However, `perf stat` shows
there's a lot of cache misses, and that probably reduces the
difference between 4KB and 1GB pages.

Let's try one last thing, with `size_t offset = 4096 + (1ul<<21) +
64;` (to avoid aliasing at the data cache level), and a smaller index
vector that fits in cache.

{% codeblock %}
int main (int argc, char **argv)
{
        (void)argc;
        (void)argv;

        char acc = 0;
        size_t stride = (24ul << 30)/NCHUNKS;
        char *data = mmap(NULL, 24ul << 30,
                          PROT_READ | PROT_WRITE, FLAGS,
                          -1, 0);
        assert(data != MAP_FAILED);
        memset(data, 0, 24ul << 30);

        size_t *indices = calloc(1ul<<10, sizeof(size_t));
        for (size_t i = 0; i < 1ul<<10; i++) {
                size_t x = NCHUNKS*random()/RAND_MAX;
                size_t offset = 4096 + (1ul<<21) + 64;
                indices[i] = (x*stride + ((x*offset)%(1ul<<30))) % (24ul << 30);
        }

        ticks begin = getticks();
        for (size_t i = 0; i < 1ul << 17; i++) {
                for (size_t j = 0; j < 1ul<<10; j++) {
                        acc += data[indices[j]];
                }
        }
        ticks end = getticks();

        printf("%g %i\n", elapsed(end, begin), acc);
        return acc;
}
{% endcodeblock %}

We get 1.06e9, 9.94e8, and 2.62e9 cycles, i.e., 250% as much time with
1GB pages than 4KB ones.

We can easily turn this around: we just have to loop over more than 4
4KB-aligned locations in a 4GB space. For example, with

{% codeblock %}
#define NCHUNKS 4096

int main (int argc, char **argv)
{
        (void)argc;
        (void)argv;

        char acc = 0;
        size_t stride = (4ul << 30)/NCHUNKS;
        char *data = mmap(NULL, 4ul << 30,
                          PROT_READ | PROT_WRITE, FLAGS,
                          -1, 0);
        assert(data != MAP_FAILED);
        memset(data, 0, 4ul << 30);

        size_t *indices = calloc(1ul<<10, sizeof(size_t));
        for (size_t i = 0; i < 1ul<<10; i++) {
                size_t x = NCHUNKS*random()/RAND_MAX;
                size_t offset = 64;
                indices[i] = (x*stride + ((x*offset)%(1ul<<30))) % (4ul << 30);
        }

        ticks begin = getticks();
        for (size_t i = 0; i < 1ul << 17; i++) {
                for (size_t j = 0; j < 1ul<<10; j++) {
                        acc += data[indices[j]];
                }
        }
        ticks end = getticks();

        printf("%g %i\n", elapsed(end, begin), acc);
        return acc;
}
{% endcodeblock %}

With the above, I find 7.55e9 cycles for 4KB pages, 3.35e9 for 2MB and
1.09e9 for 1GB pages. Here, 4KB pages are almost 7x as slow as 1GB
pages.  If I instead let `size_t offset = 4096 + 64;` (to avoid
aliasing in the 4KB TLB), I get 4.72e9 cycles for 4KB pages, so still
433% as much time.

We can also play the same trick over 32*2MB = 64MB.  On my E5, I find
3.23e9 cycles for 4KB pages, versus 1.09e9 for 2MB and 1GB pages.
Eliminating page-level aliasing only brings the 4KB case down to
3.02e9 cycles, and doesn't affect the other two cases.

So, are 1GB pages generally useful?
===================================

The following table summarises the runtimes of all the variations above with
2MB and 1GB pages (as a fraction of the number of cycles for 4KB
pages).

<center>
<table style="border-collapse: collapse;">
<col style="border:1px solid #000000;" />
<col style="border:1px solid #000000;" />
<tr><td>2MB/4KB&nbsp;</td> <td>&nbsp;1GB/4KB</td></tr>
<tr><td>1.42</td> <td>1.38</td></tr>
<tr><td>0.82</td> <td>0.59</td></tr>
<tr><td>5.42</td> <td>2.32</td></tr>
<tr><td>0.47</td> <td>1.03</td></tr>
<tr><td>0.96</td> <td>2.34</td></tr>
<tr><td>0.94</td> <td>2.47</td></tr>
<tr><td>0.44</td> <td>0.14</td></tr>
<tr><td>0.72</td> <td>0.23</td></tr>
<tr><td>0.34</td> <td>0.34</td></tr>
<tr><td>0.36</td> <td>0.36</td></tr>
</table>
</center>

Overall, I think that I wouldn't automatically switch to 2MB pages,
but that 1GB pages are a solid choice for machines that basically run
a single process at a time. When the data fits in 4GB, 1GB pages
completely eliminate TLB misses. When the data is even larger, 2MB and
1GB pages make page table walks quicker (by 18% and 40%,
respectively). It takes a very contrived situation -- in which a
program keeps hitting fewer than 512 4KB-pages that are spread out
across multiple GBs -- for smaller pages to be preferable.  The worst
I managed was 250% as much time for 1GB pages vs 4KB; in the other
direction, I achieved 693% as much time for 4KB pages versus 1GB, and
433% with a realistic situation (e.g., repeated lookups in a 4GB hash
table). Plus, there's another interesting benefits from larger pages
that did not show up in this post: we get more control over aliasing
in data caches.

With multiple processes in play, there are fragmentation issues, and
things aren't as clear-cut… especially given that 1GB pages must
currently be allocated at boot-time, on Linux.

I'm also still unsure how 1GB pages interact with NUMA. I'm
particularly worried about interleaving: interleaving at a 1GB
granularity seems unlikely to smooth out the ratio of local:remote
accesses as much as doing it at a 4KB granularity.
