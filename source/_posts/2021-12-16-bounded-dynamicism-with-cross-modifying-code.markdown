---
layout: post
title: "Bounded dynamicism with cross-modifying code"
date: 2021-12-16 17:55:31 -0500
comments: true
hidden: true
draft: true
categories: 
---

All long-lived programs are either implemented in dynamic languages,[^images]
or eventually Greenspun themselves into subverting static programming
languages to create a dynamic system (e.g., Unix process trees). The
latter approach isn't a bad idea, but it's easy to introduce more
flexibility than intended (e.g.,
[JNDI lookups](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2021-44228))
when we add late binding features piecemeal, without a holistic view
of how all the interacting components engender a weird program
modification language.

[^images]:  It's no accident that canonical dynamic languages like Smalltalk, Forth, and Lisp are all image-based: how would an image-based system even work if it were impossible to redefine functions or types?

At Backtrace, we mostly implement late (re)binding by isolating subtle
logic in dedicated executables with short process lifetimes: we can
replace executables on disk atomically, and their next invocation will
automatically pick up the change. In a pinch, we sometimes edit
template or Lua source files and hot reload them in [nginx](http://openresty.org/en/).
We prefer that to first-class programmatic support for runtime
modification because Unix has a well understood permission model
around files, and it's harder to make code accidentally overwrite a
file when it doesn't perform any disk I/O.

However, these patterns aren't always sufficient. For example, we
sometimes want to toggle code that's deep in performance-sensitive
query processing logic, or tightly coupled with such logic. That's
when we resort to [our `dynamic_flag` library](https://github.com/backtrace-labs/dynamic_flag).
Runtime modifications are restricted to boolean values (a flag can
only be enabled or disabled), so, hopefully, the functionality doesn't
hide any unexpected compounded complexity.

[The `dynamic_flag` library](https://github.com/backtrace-labs/dynamic_flag)
is an [Apache-licensed](https://github.com/backtrace-labs/dynamic_flag/blob/main/LICENSE)
"C" library that offers efficient biased conditionals for statically
linked Linux/x86-64 executables: taking the "likely" path doesn't
access memory or evaluate any jump condition,[^second-order] 
[it merely executes a `test eax` instruction that clobbers flags without any dependent instruction](https://godbolt.org/#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:8,endLineNumber:4,positionColumn:8,positionLineNumber:4,selectionStartColumn:8,selectionStartLineNumber:4,startColumn:8,startLineNumber:4),source:'%23include+%3Cstdio.h%3E%0A%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Aint+foo()%0A%7B%0A++++if+(DF_FEATURE(flag_kind,+flag_name))+%7B%0A++++++++printf(%22unlikely+path%5Cn%22)%3B%0A++++++++return+1%3B%0A++++%7D%0A%0A++++return+0%3B%0A%7D'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4).

[^second-order]:  The compiler must still consider the slow/unlikely path as reachable, so the second order impact on compiler optimisations often dominates the effect of this additional instruction.

We write "C" in scare quotes because the code relies on 
[inline `asm goto`, a GCC extension](https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#:~:text=6.47.2.7%20Goto%20Labels)
[introduced in GCC 4.5](https://gcc.gnu.org/legacy-ml/gcc-patches/2009-07/msg01556.html)
and [adopted by clang 9](https://reviews.llvm.org/D69876).
There are fallback implementations, but they're primarily meant for
static analysers, or to port code with flags disabled.

The library can be seen as a feature flag system, but its minimal
runtime overhead coupled with the ability to flip flags at runtime
opens up additional use cases, like disabling mutual exclusion logic
during single-threaded startup or toggling log statements. It's also
proved invaluable for crisis management, since we leave flags
(enabled by default) in established pieces of code without agonising
over their impact on application performance. These flags can serve as
ad hoc circuit breakers around full features or specific pieces of
code (e.g., to convert use-after-frees into resource leaks by nopping
destructor calls) when new inputs tickle old latent bugs.

The secret behind this minimal overhead? Cross-modifying machine code!

How to use `dynamic_flag`
-------------------------

All dynamic flags have a "kind" (namespace) string, and a name. A
dynamic flag can be disabled by default (like a feature flag), or
enabled by default. Using a feature flag implicitly defines and
registers it.

A dynamic flag introduced with [the `DF_FEATURE` macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L98-L110),
as in the code snippet below, is disabled (evaluates to false) by
default, and is optimised for that default value.

<iframe width="800px" height="400px" src="https://godbolt.org/e#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:16,endLineNumber:12,positionColumn:16,positionLineNumber:12,selectionStartColumn:16,selectionStartLineNumber:12,startColumn:16,startLineNumber:12),source:'%23include+%3Cstdio.h%3E%0A%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Aint+new_feature(void)%3B%0A%0Aint+foo(void)%0A%7B%0A%0A++++/*+...+preexisting+code...+*/%0A++++printf(%22preexisting+prologue%5Cn%22)%3B%0A%0A++++if+(DF_FEATURE(my_module,+flag_name))+%7B%0A++++++++new_feature()%3B%0A++++%7D%0A%0A++++/*+Old+feature+code+*/%0A++++printf(%22preexisting+epilogue%5Cn%22)%3B%0A++++return+1%3B%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4"></iframe>

We can instead enable code by default and optimise for cases where the
flag is enabled with [the `DF_DEFAULT` macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L112-L124).

<iframe width="800px" height="400px" src="https://godbolt.org/e#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:13,endLineNumber:12,positionColumn:13,positionLineNumber:12,selectionStartColumn:13,selectionStartLineNumber:12,startColumn:13,startLineNumber:12),source:'%23include+%3Cstdio.h%3E%0A%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Aint+now_old_feature(void)%3B%0A%0Aint+foo(void)%0A%7B%0A%0A++++/*+...+preexisting+code...+*/%0A++++printf(%22preexisting+prologue%5Cn%22)%3B%0A%0A++++if+(DF_DEFAULT(my_module,+flag_name))+%7B%0A++++++++now_old_feature()%3B%0A++++%7D%0A%0A++++/*+Old+feature+code+*/%0A++++printf(%22preexisting+epilogue%5Cn%22)%3B%0A++++return+1%3B%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4"></iframe>

Each [`DF_*` condition](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L98-L155)
in the source is actually its own feature flag:
the full flag name is `kind:name@source_file:line_number` (e.g.,
`my_module:flag_name@<stdin>:15`), and each condition has its own
state record. It's thus safe, if potentially confusing, to define flag
of different types (feature or default) with the same kind and
name. These macros are safe to use in [inline](https://godbolt.org/#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:1,endLineNumber:17,positionColumn:1,positionLineNumber:17,selectionStartColumn:1,selectionStartLineNumber:17,startColumn:1,startLineNumber:17),source:'%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Aint+old_feature(void)%3B%0A%0Aextern+inline+int+foo(void)%0A%7B%0A%0A++++/*+...+preexisting+code...+*/%0A%0A++++if+(DF_DEFAULT(my_module,+flag_name))+%7B%0A++++++++return+old_feature()%3B%0A++++%7D%0A%0A++++/*+Old+feature+code+*/%0A++++return+0%3B%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cclang1300,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:9,endLineNumber:5,positionColumn:9,positionLineNumber:5,selectionStartColumn:9,selectionStartLineNumber:5,startColumn:9,startLineNumber:5),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+clang+13.0.0+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4)
or
[static inline](https://godbolt.org/#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:18,endLineNumber:21,positionColumn:18,positionLineNumber:21,selectionStartColumn:18,selectionStartLineNumber:21,startColumn:18,startLineNumber:21),source:'%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Aint+old_feature(void)%3B%0A%0Astatic+inline+int+foo(void)%0A%7B%0A%0A++++/*+...+preexisting+code...+*/%0A%0A++++if+(DF_DEFAULT(my_module,+flag_name))+%7B%0A++++++++return+old_feature()%3B%0A++++%7D%0A%0A++++/*+Old+feature+code+*/%0A++++return+0%3B%0A%7D%0A%0Aint+bar(void)%0A%7B%0A%0A++++return+foo()%3B%0A%7D'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:9,endLineNumber:5,positionColumn:9,positionLineNumber:5,selectionStartColumn:9,selectionStartLineNumber:5,startColumn:9,startLineNumber:5),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4)
functions: each instantiation will get its own metadata block, and an
arbitrary number of blocks can share the same name.

Applications must [call `dynamic_flag_init_lib` early during startup](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L291) to
initialise the library's state.
Interactive or configuration-driven usage tends to activate or
deactivate flags by regex on the full name, 
[with `dynamic_flag_activate` and `dynamic_flag_deactivate`](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L220-L230).

Using `dynamic_flag` programmatically
-------------------------------------

The [`DF_FEATURE` and `DF_DEFAULT` macros](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L98-L124)
can easily be used for standard feature flags. However, the
`dynamic_flag` library is more powerful. Applications can
programmatically enable and disable blocks of code to implement a
restricted form of [aspect oriented programming](https://en.wikipedia.org/wiki/Aspect-oriented_programming): 
["advice"](https://en.wikipedia.org/wiki/Advice_(programming)) cannot
be inserted post hoc, and must instead be defined inline in the
source, but may be toggled at runtime by unrelated code.

For example, an application may allow individual HTTP requests to opt
into detailed tracing with a query string parameter `?tracing=1`, and
set `request->tracing_mode = true` in its internal request object when
it accepts such a request. In environments where only one request in a
million enables tracing, we can easily spend more aggregate time
checking whether `request->tracing_mode == true` than in the tracing
logic itself.

It's tempting to use a dynamic flag that is enabled whenever at least
one in-flight request has opted into tracing. That's why 
[the `DF_OPT` (for opt-in logic) macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L141-L155) exists.

<iframe width="800px" height="400px" src="https://godbolt.org/e#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:1,endLineNumber:10,positionColumn:1,positionLineNumber:10,selectionStartColumn:1,selectionStartLineNumber:10,startColumn:1,startLineNumber:10),source:'%23include+%3Cstdbool.h%3E%0A%23include+%3Cstdio.h%3E%0A%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Astruct+request+%7B%0A++++bool+tracing_mode%3B%0A%7D%3B%0A%0Avoid+trace_request(struct+request+*req,+const+char+*func)%3B%0A%0Avoid+foo(struct+request+*req)%0A%7B%0A++++if+(DF_OPT(request_tracing,+check)+%26%26+req-%3Etracing_mode)%0A++++++++trace_request(req,+__FUNCTION__)%3B%0A++++%0A++++return%3B%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4"></iframe>

When all request tracing chunks of code are guarded by such a
conditional, it's always safe to enable flags with the
`request_tracing` kind, but we can improve performance by disabling
them when no in-flight request has opted into tracing. The
[`DF_OPT` macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L141-L155)
instructs the compiler to assume the flag is disabled, but leaves the
flag enabled (i.e., the conditional always looks at
`request->tracing_mode`) until the library is initialised with
`dynamic_flag_init_lib`;[^unreachable] after that initialisation call,
the flag is disabled (i.e., the overhead is a `test eax` instruction
that falls through without any conditional branching) until the flag
is explicitly enabled again.

[^unreachable]: Or if the `dynamic_flag` library isn't aware of that `DF_OPT`, maybe because the function surrounding that `DF_OPT` conditional was loaded dynamically.

This flag-before-check pattern let us trivially preserve correctness
because it's always safe to enable `request_tracing` flags: in the
worst case, we'll just look at the request object, see that
`request->tracing_mode == false`, and skip the tracing logic. Of
course, that's not ideal for performance. When we definitely know that
no request has asked for tracing, we want to disable `request_tracing`
flags and not even look at the request object's `tracing_mode` field.

With [the `DF_OPT` macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L141-L155),
we have correctness in all cases, even when a `DF_OPT` site is somehow
not visible to the `dynamic_flag` machinery: although the compiler is
instructed to assume `DF_OPT` flags are disabled, they are actually
enabled in the machine code generated at compile-time. As soon as
`dynamic_flag_init_lib` confirms that it has access to a `DF_OPT`
macro's expansion, that flag is disabled
([converted to `DF_FEATURE`](https://godbolt.org/#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:19,endLineNumber:13,positionColumn:19,positionLineNumber:13,selectionStartColumn:19,selectionStartLineNumber:13,startColumn:19,startLineNumber:13),source:'%23include+%3Cstdbool.h%3E%0A%23include+%3Cstdio.h%3E%0A%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Astruct+request+%7B%0A++++bool+tracing_mode%3B%0A%7D%3B%0A%0Avoid+trace_request(struct+request+*req,+const+char+*func)%3B%0A%0Avoid+foo(struct+request+*req)%0A%7B%0A++++if+(DF_FEATURE(request_tracing,+check)+%26%26+req-%3Etracing_mode)%0A++++++++trace_request(req,+__FUNCTION__)%3B%0A++++%0A++++return%3B%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4)),
for performance.

Whenever the application receives a request that opts into tracing
(with query string parameter `?tracing=1`), it enables all
`request_tracing` flags by 
[executing `dynamic_flag_activate_kind(request_tracing, NULL)`](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L189-L205).
When that same request leaves the system (e.g., when the application
has fully sent a response back), the application 
[undoes the activation with `dynamic_flag_deactivate_kind(request_tracing, NULL)`](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L207-L218).

Activation and deactivation calls increment and decrement 
[counters associated with each instance of a `DF_...` macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/src/dynamic_flag.c#L99-L110).
Tracing-only blocks will thus enter the "slow" path (checking whether
`request->tracing_mode == true`) when at least one in-flight request
has `tracing_mode == true`, and revert to the fast path as soon as no
such request exists.

Since the first thing our code does after `DF_OPT(request_tracing, ...)` is
to determine whether the request actually opted into tracing, it's
always safe for a request handler to observe a `DF_OPT(request_tracing, ...)`
flag enabled. However, it would be incorrect to leave such a flag
disabled (and thus not even look at the request object's
`request->tracing_mode`) when handling a request that has opted into
tracing. There's always a conservative option (keep `request_tracing`
flags enabled), so we can safely nop out flags structured like the
`request_tracing` `DF_OPT` even when handling different requests
concurrently and in parallel. We must enable `DF_OPT` flags whenever
one request might need the corresponding code, and `dynamic_flag`
library's internal activation counters help us implement that
correctly.

Confirming that a flag is at its expected value (disabled for
`DF_FEATURE` and `DF_OPT`, enabled for `DF_DEFAULT`) is fast... but
that's only because we shifted all the complexity to the flag flipping
code. Changing the value for a set of flags is extremely slow
(milliseconds of runtime and several [IPIs](https://en.wikipedia.org/wiki/Inter-processor_interrupt) for 
[multiple `mprotect(2)` calls](https://man7.org/linux/man-pages/man2/mprotect.2.html)), so it only makes sense to use dynamic flags when they are
rarely activated or deactivated (e.g., less than once a minute or even
less than once an hour).

We have found programmatic flag manipulation to be useful not just for
opt-in request tracing or to enable log statements, but also to
minimise the impact of slow paths during program phases that do not
require them. For example, mutual exclusion and [SMR (PDF)](http://www.cs.toronto.edu/~tomhart/papers/tomhart_thesis.pdf)
deferrals may be redundant while a program is in a single-threaded
startup mode: we can guard such code behind `DF_OPT(steady_state, ...)`,
and enable it just before spawning worker threads. When the program
only enters slow phases every few minutes, `DF_OPT` is also a good
option. That was the case for a software transactional memory system
that batches updates. Most of the time, no write is in flight, so
readers never have to check for a slow path, and all these checks can
be guarded with `DF_OPT(stm, ...)` conditions. The program only has to
enable `stm` flags around a batch of updates; enabling and disabling
all these flags can take a while (milliseconds), but, as long as
updates are infrequent enough, the improved common case (getting rid
of a memory load and a conditional jump for a read barrier) means the
tradeoff makes sense.

When flags are enabled programmatically, it can be useful to work
around bugs by forcibly enabling or disabling a subset of code
blocks. It's easy to force a flag to remain active: flag activations
are counted, so it suffices to [activate it](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L220-L224)
manually, once. Activation counts don't go negative, to make it safe
to [issue ad hoc `dynamic_flag_deactivate` calls](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L226-L230)
without wedging the system in a weird state, we can't use deactivation
to prevent, e.g., a crashy request tracing block from being
activated. We can fully disable such code paths, regardless of dynamic
activation, [with `dynamic_flag_unhook`](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L232-L239).
As long as a `DF_*` condition has been
["unhook"ed](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L232-L239)
more often than it has been 
["rehook"ed](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L241-L245),
attempts to activate it will
silently no-op. Once unhooked, we can 
[issue `dynamic_flag_deactivate` calls](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L226-L230)
until [the activation count reaches 0](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/src/dynamic_flag.c#L99-L110).

Overhead matters
----------------

Runtime efficiency is an essential feature in `dynamic_flag` not only
because it unlocks new use cases, but, more importantly, because it
means programmers do not have to worry about the performance impact of
inserting a feature flag in the most obvious location, even if, e.g.,
that's in the middle of an inner loop.

We designed `dynamic_flag` to minimise the amount of friction and
mental overhead when adding a new feature flag in order to encourage
programmers to spontaneously protect code with flag checks, without
prodding during design or code review. That's why we care so much
about all sources of friction, and not just execution time
overhead. We don't want adding a feature flag to become associated
with painful memories. That's why there's no need to break one's flow
and register flags separately from their use points.

However, we're also aware that feature flags tend to stick around
forever. We try to counteract this tendency with static registration:
all the `DF_*` invocations in an executable appear in 
[its `dynamic_flag_list` section](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L458-L466), and
[the `dynamic_flag_list_state` function](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L247-L277)
enumerates them at runtime. A periodic audit will reveal flags that
have become irrelevant, and each flag's full name includes its source
location.

The core implementation trick
-----------------------------

The [introduction of `asm goto` in GCC 4.5](https://gcc.gnu.org/legacy-ml/gcc-patches/2009-07/msg01556.html)
made it possible to [implement control operators in inline assembly](https://lwn.net/Articles/350714/).
 When the condition actually varies at runtime, it usually
makes more sense to [set an output variable with a condition code](https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#:~:text=6.47.2.4%20Flag%20Output%20Operands),
but `dynamic_flag` conditions are actually static in machine code:
each `DF_*` macro expands to *one* 5-byte instruction, 
[a `test eax, imm32` instruction](https://c9x.me/x86/html/file_module_x86_id_315.html)
that falls through to the common case when that's the flag's value
(i.e., enabled for `DF_DEFAULT`, disabled for `DF_FEATURE` and
`DF_OPT`), and a [32-bit relative `jmp rel32` to the unexpected path](https://c9x.me/x86/html/file_module_x86_id_147.html)
(disabled for `DF_DEFAULT`, enabled for `DF_FEATURE` and `DF_OPT`)
otherwise. Activating and deactivating dynamic flags patches the
corresponding target instructions between `test imm32` (0xA9) and `jmp rel32` (0xE9).

The `DF_...` macros expand into a 
[lot more inline assembly than just that one instruction](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L443-L478);
 the rest of the expansion is a lot of noise to
register everything with structs and pointers in dedicated
sections. The static registration is somewhat orthogonal to the
performance goals, but is key to the (lazy-)programmer-friendly
interface.

We use `test eax, imm32` instead of a nop because it's exactly five
bytes, just like `jmp rel32`, and because its 4-byte immediate is in
the same place as the 4-byte offset of `jmp rel32`. We can thus encode
the jump offset at assembly-time, and flip between falling through to
the common path (`test`) and jumping to the unexpected path (`jmp`) by
overwriting the opcode byte (0xA9 for `test`, 0xE9 for `jmp`).

We update a single byte for each dynamic flag to avoid questions
around the correct order for writes.  That single-byte
cross-modification (we overwrite instruction bytes while other threads
may be executing the mutated machine code) also doesn't affect the
size of the instruction (both `test eax` and `jmp rel` span 5 bytes);
that should hopefully to avoid sharp edges around instruction decoding
in hardware.[^cpu-millenia] Finally, the library must 
[relax and reinstate page protection with `mprotect(2)`](https://man7.org/linux/man-pages/man2/mprotect.2.html)) around
all cross modification writes, and `mprotect`-ing from
Read-Write-eXecute permissions to Read-eXecute acts as a
[membarrier (issues IPIs) on Linux/x86-64](https://lwn.net/Articles/728795/#:~:text=These%20users%20have%20found%20a%20trick%20to%20get%20the%20desired%20behavior%20without%20calling%20membarrier()%3A%20they%20make%20a%20call%20to%20either%20mprotect()%20or%20munmap()%20instead);
we can thus know that the updated code is globally visible by the time
a [call to `dynamic_flag_activate`, etc.](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L189-L230) returns.

[^cpu-millenia]:  After a few CPU-millenia of production experience, the cross-modification logic hasn't been associated with any "impossible" bug, or with any noticeable increase in the rate of hardware hangs or failures.

It's not practical to flip page protection for *each* `DF_` expansion,
especially with inlining (some users have hundreds of inlined calls to
the same flagged function, e.g., to temporarily paper over
use-after-frees by nopping out a few calls to `free(2)`). Most of the
complexity in `dynamic_flag.c` is simply in 
[gathering metadata records for all `DF_` sites that should be activated or deactivated](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/src/dynamic_flag.c#L474-L529), and in
[amortising `mprotect` calls for stretches of `DF_` sites on contiguous pages](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/src/dynamic_flag.c#L390-L449).

Sometimes, code is just done
----------------------------

The [`dynamic_flag` library](https://github.com/backtrace-labs/dynamic_flag)
is an updated interface for the core implementation of the 
[6-year old `an_hook`](https://github.com/appnexus/acf/blob/master/common/an_hook.h),
and reflects years of experience with that functionality. We're happy
to share it, but aren't looking for feature requests or contributions.

There might be some small clean-ups as we add support for ARM or RISC
V, or let the library can interoperate with a Rust implementation.
However, we don't expect changes to the interface (the `DF_` macros
and the activation/deactivation functions), nor to its core structure,
especially given the contemporary tastes for hardening (for example,
the cross-modification approach is completely incompatible with
OpenBSD's and OS X's strict `W^X` policies). The library works for our
target platforms, and we don't wish to take on extra complexity that
is of no benefit to us.

Of course, [it's Apache licensed](https://github.com/backtrace-labs/dynamic_flag/blob/main/LICENSE),
so anyone can fork the library and twist it beyond
recognition. However, if you're interested in powerful patching
capabilities, tools like [Live++](https://liveplusplus.tech) and
[Recode](https://www.indefiant.com/) might be more appropriate. We
want `dynamic_flag` to remain simple and just barely flexible enough
for our usage patterns.

<small>Thank you, Jacob and Per, for feedback on earlier versions.</small>

<p><hr style="width: 50%"></p>
