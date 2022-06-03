---
layout: post
title: "Hacking tips for Linux perf porcelain"
date: 2022-06-01 21:09:08 -0400
comments: true
categories: 
---

Sometimes you just want to abuse [Linux `perf`](https://perf.wiki.kernel.org/index.php/Main_Page)
to make it do a thing it's not designed for, and a proper C program
would represent an excessive amount of work.

Here are two tricks I find helpful when jotting down hacky analysis scripts.

Programmatically interacting with `addr2line -i`
-----------------------------------------------

Perf can resolve symbols itself, but [addr2line](https://sourceware.org/binutils/docs/binutils/addr2line.html)
is a lot more flexible (especially when you inflict
[subtle things on your executable's mappings](https://github.com/libhugetlbfs/libhugetlbfs/blob/6b126a4d7da9490fa40fe7e1b962edcb939feddc/HOWTO#L25-L30)).

It's already nice that `addr2line -Cfe /path/to/binary` lets you write
hex addresses to stdin and spits out the corresponding function name
on one line, and its source location on the next (or `??` / `??:0` if
debug info is missing).  However, for heavily inlined (*cough* C++
*cough*) programs, you really want the whole callstack that's encoded
in the debug info, not just the most deeply inlined function ("oh
great, it's in `std::vector<Foo>::size()`"). 

[The `--inline` flag](https://sourceware.org/binutils/docs/binutils/addr2line.html#:~:text=%2Di-,%2D%2Dinlines,-If%20the%20address)
addresses that... by printing source locations for inline callers on
their own line(s).  Now that the output for each address can span
a variable number of lines, how is one to know when to stop reading?

A simple trick is to always write *two* addresses to `addr2line`'s
standard input: the address we want to symbolicate, and
that never has debug info (e.g., 0).

EDIT: [Travis Downs reports that `llvm-addr2line-14` finds debug info for 0x0](https://twitter.com/trav_downs/status/1532206949038624768)
(presumably a bug. I don't see that on llvm-addr2line-12)
and suggests looking for `0x0: .*` in addition to `??`/`??:0`.  It's
easy enough to stop when either happens, and clang's version of
`addr2line` can be a lot faster than binutil's on files with a lot of
debug information.

We now know that the first set of resolution information lines (one
line when printing only the file and line number, two lines when
[printing function names as well with `-f`](https://sourceware.org/binutils/docs/binutils/addr2line.html#:~:text=%2Df-,%2D%2Dfunctions,-Display%20function%20names))
belongs to the address we want to symbolicate.  We also know to
expect output for missing information (`??:0` or `??` / `??:0`)
from the dummy address.  We can thus keep reading until we find
a set of lines that corresponds to missing information, and 
disregard that final source info.

For example, passing `$IP\n0\n` on stdin could yield:

```
??
??:0
??
??:0
```

or, without `-f` function names,

```
??:0
??:0
```

In both cases we first consume the first set of lines (the output
for`$IP` must include at least one record), then consume the next set
of lines and observe it represent missing information, so we stop
reading.

When debug information is present, we might instead find

```
foo()
src/foo.cc:10
??
??:0
```

The same algorithm clearly works.

Finally, with inlining, we might instead observe

```
inline_function()
src/bar.h:5
foo()
src/foo.cc:12
??
??:0
```

We'll unconditionally assign the first pair of lines to `$IP`,
read a second pair of lines, see that it's not `??` / `??:0`
and push that to the bottom of the inline source location
stack, and finally stop after reading the third pair of lines.

Triggering PMU events from non-PMU perf events
----------------------------------------------

Performance monitoring events in perf tend to be much more powerful
than non-PMU events: each perf "driver" works independently, so only
PMU events can snapshot [the Processor Trace buffer](https://man7.org/linux/man-pages/man1/perf-intel-pt.1.html),
for example.

However, we sometimes really want to trigger on a non-PMU event.  For
example, we might want to watch for writes to a specific address with
a hardware breakpoint, and snapshot the PT buffer to figure out what
happened in the microseconds preceding that write.  Unfortunately,
that doesn't work out of the box: only PMU events can snapshot the
buffer.  I remember running into a similar limitation when I wanted to
capture performance counters after non-PMU events.

There is however a way to trigger PMU events from most non-PMU events:
watch for [far branches](https://perfmon-events.intel.com/index.html?pltfrm=snb.html&evnt=BR_INST_RETIRED.FAR_BRANCH)!
I believe I also found these events much more reliable to detect
preemption than the scheduler's software event, many years ago.

Far branches are rare (they certainly don't happen in regular x86-64
userspace program), but interrupt usually trigger a far CALL to
execute the handler in ring 0 (attributed to ring 0), and a far RET to
switch back to the user program (attributed to ring 3).

We can thus configure

```
perf record \
    -e intel_pt//u \
    -e BR_INST_RETIRED.FAR_BRANCH/aux-sample-size=...,period=1/u \
    -e mem:0x...:wu ...
```

to:

1. trigger a debug interrupt when userspace writes to the watched
memory address
2. which will increment the `far_branch` performance monitoring
counter
3. which triggers Linux's performance monitoring interrupt handler
4. which will finally write both the far branch event and its
   associated PT buffer to the perf event ring buffer.

Not only does this work, but it also minimises the trigger latency.
That's a big win compared to, e.g., [perf record's built-in `--switch-output-event`](https://man7.org/linux/man-pages/man1/perf-record.1.html#:~:text=%2D%2Dswitch%2Doutput%2Devent):
a trigger latency on the order of hundreds of microseconds forces a
large PT buffer in order to capture the period we're actually
interested in, and copying that large buffer slows down everything.

Is this documented?
-------------------

Who knows? (Who cares?) These tricks fulfill a common need in quick
hacks, and I've been using (and rediscovering) them for years.

I find tightly scoped tools that don't try to generalise have an ideal
insight:effort ratio.  Go write your own!
