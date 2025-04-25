---
layout: post
title: "VPTERNLOG: when three is 100% more than two"
date: 2024-11-22 21:50:00 -0500
comments: true
published: true
hidden: false
draft: false
categories: 
---

Like many, when I first saw [VPTERNLOG](https://www.felixcloutier.com/x86/vpternlogd:vpternlogq),
my reaction was "\\(\log_2(3) \approx 1.58\\) is a nice reduction in depth, but
my code mostly doesn't have super deep reductions."

A little bit of thinking reveals a big win at smaller (reasonable) scales:
a binary operator takes two values and outputs one, while a ternary operator takes three and outputs one.
In a reduction, each application of the binary operator decrements the number of values by \\(2 - 1 = 1\\),
but each application of the ternary operator decrements it by \\(3 - 1 = 2\\)!

We thus need *half* as many ternary operations to reduce a given number of bitvectors,
compared to binary operations... and it's not like the
[throughput (or latency for that matter) is worse](https://uops.info/table.html?search=vpternlogd%20ymm&cb_lat=on&cb_tp=on&cb_uops=on&cb_ports=on&cb_ADLP=on&cb_ZEN4=on&cb_measurements=on&cb_base=on&cb_avx512=on).
Plus it's hard to be more orthogonal than a lookup table.

Cute lightweight instruction, two thumbs up!
