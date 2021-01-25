---
layout: post
title: "Virtual memory: something old"
date: 2015-03-03 19:42:38 -0500
draft: true
hidden: true
comments: true
categories: appnexus
---

Mapping virtual addresses to physical memory is slow.  However, it's
hard to avoid that step on multi-user systems; in fact, x86-64 doesn't
even have raw addresses in 64 bit mode.  The pressure on efficient
virtual memory mapping is such that even the 386 had
[specialised hardware](http://en.wikipedia.org/wiki/Translation_lookaside_buffer)
to speed up the translation.  When presented with a powerful
abstraction that cannot be avoided (and which hardware works hard to
support well), clever hackers inevitably come up with tricks to
exploit that abstraction and make software simpler or more efficient.
However, the growth of RAM and datasets since the 80s spurred the
development of recent hardware features that are completely changing
the performance envelope of address translation.  It looks like
developers for memory-intensive applications will soon get to relive
challenges previously associated with raw physical memory addressing.

# Why does virtual memory incur an overhead?

Virtual memory mostly plays two roles.  The first one is preventing
processes from stomping on one another or on the kernel, and the
second is to let developers pretend they (almost) fully control
individual processes that can consume large amounts of memory.  Access
control leads to features like memory protection, which can also be
used to implement paging and memory-mapped files.  The idea that
processes are lightweight virtual machines is more "interesting": it
means that different processes on the same machines should be able to
have different chunks of physical memory at the same virtual address.
Some operating systems implement only protection, without
virtualisation; it's an interesting trade off, but mostly it's an
exotic one that we don't see outside the embedded space (where developers
control machines, and not just programs).  Full address
virtualisation means that every memory access must first translate a
virtual address to a physical address (i.e., a signal pattern for RAM
chips).  Nowadays, we rarely use swap memory, and that's the main
overhead of virtual memory: mapping virtual to physical addresses.

Fully general address virtualisation would probably be something like
an arbitrary dictionary from virtual address to physical address at a
word or byte granularity.  That's clearly unnecessary and hard to
implement (caching would be "interesting").  On x86 and x86-64
machines, the mapping instead happens on a 4 KB page granularity: each
aligned virtual address page of 4 KB maps to nothing or to a physical
page, also on a 4 KB boundary.  This choice reduces the amount of data
used to map virtual addresses (we map pages, not bytes), and exploits
an expectation of spatial locality: if a program accesses address X,
it's likely to read from or write to an address not far from X (e.g.,
another field in the same object).

[Drawing]



# Hacking with virtual memory

# 28 years later
