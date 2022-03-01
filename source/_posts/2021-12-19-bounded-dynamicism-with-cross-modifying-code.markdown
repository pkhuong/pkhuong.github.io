---
layout: post
title: "Bounded dynamicism with cross-modifying code"
date: 2021-12-19 19:43:01 -0500
comments: true
categories:
---

<small>Originally posted on the [Backtrace I/O tech blog](https://engineering.backtrace.io/2021-12-19-bounded-dynamicism-with-cross-modifying-code/).</small>

All long-lived programs are either implemented in [dynamic languages](https://wiki.c2.com/?DynamicLanguage),[^images]
or eventually [Greenspun](https://en.wikipedia.org/wiki/Greenspun%27s_tenth_rule)
themselves into subverting static programming
languages to create a dynamic system (e.g., Unix process trees). The
latter approach isn't a bad idea, but it's easy to introduce more
flexibility than intended (e.g.,
[data-driven JNDI lookups](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2021-44228))
when we add late binding features piecemeal, without a holistic view
of how all the interacting components engender a weird program
modification language.

[^images]:  It's no accident that canonical dynamic languages like Smalltalk, Forth, and Lisp are all image-based: how would an image-based system even work if it were impossible to redefine functions or types?

At Backtrace, we mostly implement late (re)binding by isolating subtle
logic in dedicated executables with short process lifetimes: we can
replace binaries on disk atomically, and their next invocation will
automatically pick up the change. In a pinch, we sometimes edit
template or Lua source files and hot reload them in [nginx](http://openresty.org/en/).
We prefer this to first-class programmatic support for runtime
modification because Unix has a well understood permission model
around files, and it's harder to bamboozzle code into overwriting
files when that code doesn't perform any disk I/O.

However, these patterns aren't always sufficient. For example, we
sometimes wish to toggle code that's deep in performance-sensitive
query processing loops, or tightly coupled with such logic. That's
when we rely on [our `dynamic_flag` library](https://github.com/backtrace-labs/dynamic_flag).

This library lets us tweak flags at runtime, but flags can only take
boolean values (enabled or disabled), so the dynamicism it introduces
is hopefully bounded enough to avoid unexpected emergent
complexity.  The functionality looks like classic feature flags,
but thanks to the flags' minimal runtime overhead coupled with the
ability to flip them at runtime, there are additional use cases, such
as disabling mutual exclusion logic during single-threaded startup or
toggling log statements. The library has also proved invaluable for crisis
management, since we can leave flags (enabled by default) in
well-trodden pieces of code without agonising over their impact on
application performance. These flags can serve as ad hoc circuit
breakers around complete features or specific pieces of code when new
inputs tickle old latent bugs.

The secret behind this minimal overhead? Cross-modifying machine code!

Intel tells us we're not supposed to do that, at least not without
pausing threads... yet the core of [the `dynamic_flag` C library](https://github.com/backtrace-labs/dynamic_flag)
has been toggling branches on thousands of machines for years, without
any issue.  It's available under the
[Apache license](https://github.com/backtrace-labs/dynamic_flag/blob/main/LICENSE)
for other adventurous folks.

Overhead matters
----------------

Runtime efficiency is an essential feature in `dynamic_flag`---
enough to justify mutating machine code while it's executing on other
cores
---not only because it unlocks additional use cases, but, more
importantly, because it frees programmers from worrying about the
performance impact of branching on a flag in the most obvious
location, even if that's in the middle of a hot inner loop.

With the aim of encouraging programmers to spontaneously protect code
with flag checks, without prodding during design or code review, we
designed `dynamic_flag` to minimise the amount of friction and mental
overhead of adding a new feature flag. That's why we care so much
about all forms of overhead, not just execution time. For example,
there's no need to break one's flow and register flags separately from
their use points. Adding a feature flag should not feel like a chore.

However, we're also aware that feature flags tend to stick around
forever. We try to counteract this inertia with static registration:
all the `DF_*` expansions in an executable appear in
[its `dynamic_flag_list` section](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L458-L466), and
[the `dynamic_flag_list_state` function](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L247-L277)
enumerates them at runtime. Periodic audits will reveal flags that
have become obsolete, and flags are easy to find:
each flag's full name includes its location in the source code.

We find value in `dynamic_flag` because its runtime overhead is
negligible for all but the most demanding
code,[^macro-writers-bill-of-rights] while the interface lets us easily
make chunks of code toggleable at runtime without having to worry
about things like "where am I supposed to register this new option?"
The same system is efficient *and* ergonomic enough for all teams in
all contexts, avoids contention in our source tree, and guarantees
discoverability for whoever happens to be on call.

[^macro-writers-bill-of-rights]: Like [guaranteed optimisations in Lisps](https://www.youtube.com/watch?v=LIEX3tUliHw), the predictable performance impact isn't important because all code is performance sensitive, but because performance is a cross-cutting concern, and a predictably negligible overhead makes it easier to implement new abstractions, especially with the few tools available in C.  In practice, the impact of considering a code path reachable in case a flag is flipped from its expected value usually dwarfs that of the single `test` instruction generated for the dynamic flag itself.

How to use `dynamic_flag`
-------------------------

All dynamic flags have a "kind" (namespace) string, and a name. We
often group all flags related to an experimental module or feature in
the same "kind," and use the name to describe the specific functionality
in the feature guarded by the flag. A dynamic flag can be disabled by
default (like a feature flag), or enabled by default, and evaluating a
dynamic flag's value implicitly defines and registers it with the
`dynamic_flag` library.

A dynamic flag introduced with [the `DF_FEATURE` macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L98-L110),
as in the code snippet below, is disabled (evaluates to false) by
default, and instructs the compiler to optimise for that default value.

<iframe width="800px" height="400px" src="https://godbolt.org/e#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:16,endLineNumber:12,positionColumn:16,positionLineNumber:12,selectionStartColumn:16,selectionStartLineNumber:12,startColumn:16,startLineNumber:12),source:'%23include+%3Cstdio.h%3E%0A%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Aint+new_feature(void)%3B%0A%0Aint+foo(void)%0A%7B%0A%0A++++/*+...+preexisting+code...+*/%0A++++printf(%22preexisting+prologue%5Cn%22)%3B%0A%0A++++if+(DF_FEATURE(my_module,+flag_name))+%7B%0A++++++++new_feature()%3B%0A++++%7D%0A%0A++++/*+Old+feature+code+*/%0A++++printf(%22preexisting+epilogue%5Cn%22)%3B%0A++++return+1%3B%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4"></iframe>

We can instead enable code by default and optimise for cases where the
flag is enabled with [the `DF_DEFAULT` macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L112-L124).

<iframe width="800px" height="400px" src="https://godbolt.org/e#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:13,endLineNumber:12,positionColumn:13,positionLineNumber:12,selectionStartColumn:13,selectionStartLineNumber:12,startColumn:13,startLineNumber:12),source:'%23include+%3Cstdio.h%3E%0A%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Aint+now_old_feature(void)%3B%0A%0Aint+foo(void)%0A%7B%0A%0A++++/*+...+preexisting+code...+*/%0A++++printf(%22preexisting+prologue%5Cn%22)%3B%0A%0A++++if+(DF_DEFAULT(my_module,+flag_name))+%7B%0A++++++++now_old_feature()%3B%0A++++%7D%0A%0A++++/*+Old+feature+code+*/%0A++++printf(%22preexisting+epilogue%5Cn%22)%3B%0A++++return+1%3B%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4"></iframe>

Each [`DF_*` condition](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L98-L155)
in the source is actually its own flag;
a flag's full name looks like `kind:name@source_file:line_number` (e.g.,
`my_module:flag_name@<stdin>:15`), and each condition has its own
state record. It's thus safe, if potentially confusing, to define flags
of different types (feature or default) with the same kind and
name. These macros may appear in [inline](https://godbolt.org/#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:1,endLineNumber:17,positionColumn:1,positionLineNumber:17,selectionStartColumn:1,selectionStartLineNumber:17,startColumn:1,startLineNumber:17),source:'%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Aint+old_feature(void)%3B%0A%0Aextern+inline+int+foo(void)%0A%7B%0A%0A++++/*+...+preexisting+code...+*/%0A%0A++++if+(DF_DEFAULT(my_module,+flag_name))+%7B%0A++++++++return+old_feature()%3B%0A++++%7D%0A%0A++++/*+Old+feature+code+*/%0A++++return+0%3B%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cclang1300,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:9,endLineNumber:5,positionColumn:9,positionLineNumber:5,selectionStartColumn:9,selectionStartLineNumber:5,startColumn:9,startLineNumber:5),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+clang+13.0.0+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4)
or
[static inline](https://godbolt.org/#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:18,endLineNumber:21,positionColumn:18,positionLineNumber:21,selectionStartColumn:18,selectionStartLineNumber:21,startColumn:18,startLineNumber:21),source:'%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Aint+old_feature(void)%3B%0A%0Astatic+inline+int+foo(void)%0A%7B%0A%0A++++/*+...+preexisting+code...+*/%0A%0A++++if+(DF_DEFAULT(my_module,+flag_name))+%7B%0A++++++++return+old_feature()%3B%0A++++%7D%0A%0A++++/*+Old+feature+code+*/%0A++++return+0%3B%0A%7D%0A%0Aint+bar(void)%0A%7B%0A%0A++++return+foo()%3B%0A%7D'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:9,endLineNumber:5,positionColumn:9,positionLineNumber:5,selectionStartColumn:9,selectionStartLineNumber:5,startColumn:9,startLineNumber:5),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4)
functions: each instantiation will get its own metadata block, and an
arbitrary number of blocks can share the same full name.

Before manipulating these dynamic flags,
applications must [call `dynamic_flag_init_lib` to initialise the library's state](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L291).
Once the library is initialised, interactive or configuration-driven
usage typically toggles flags by calling
[`dynamic_flag_activate` and `dynamic_flag_deactivate`](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L220-L230) with POSIX extended regexes that match
the flags' full names.

Using `dynamic_flag` programmatically
-------------------------------------

The [`DF_FEATURE` and `DF_DEFAULT` macros](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L98-L124)
directly map to classic feature flags, but the
`dynamic_flag` library still has more to offer. Applications can
programmatically enable and disable blocks of code to implement a
restricted form of [aspect oriented programming](https://en.wikipedia.org/wiki/Aspect-oriented_programming):
["advice"](https://en.wikipedia.org/wiki/Advice_(programming)) cannot
be inserted post hoc, and must instead be defined inline in the
source, but may be toggled at runtime by unrelated code.

For example, an application could let individual HTTP requests opt
into detailed tracing with a query string parameter `?tracing=1`, and
set `request->tracing_mode = true` in its internal request object when
it accepts such a request. Environments where fewer than one request
in a million enables tracing could easily spend more aggregate time
evaluating `if (request->tracing_mode == true)` than they do in the
tracing logic itself.
One could try to reduce the overhead by coalescing the trace code in
fewer conditional blocks, but that puts more distance between the
tracing code and the traced logic it's supposed to record, which
tends to cause the two to desynchronise and adds to development
friction.

It's tempting to instead optimise frequent checks for the common case
(no tracing) with a dynamic flag that is enabled whenever at least one
in-flight request has opted into tracing. That's why
[the `DF_OPT` (for opt-in logic) macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L141-L155) exists.

<iframe width="800px" height="400px" src="https://godbolt.org/e#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:1,endLineNumber:10,positionColumn:1,positionLineNumber:10,selectionStartColumn:1,selectionStartLineNumber:10,startColumn:1,startLineNumber:10),source:'%23include+%3Cstdbool.h%3E%0A%23include+%3Cstdio.h%3E%0A%23include+%3Chttps://raw.githubusercontent.com/backtrace-labs/dynamic_flag/main/include/dynamic_flag.h%3E%0A%0Astruct+request+%7B%0A++++bool+tracing_mode%3B%0A%7D%3B%0A%0Avoid+trace_request(struct+request+*req,+const+char+*func)%3B%0A%0Avoid+foo(struct+request+*req)%0A%7B%0A++++if+(DF_OPT(request_tracing,+check)+%26%26+req-%3Etracing_mode)%0A++++++++trace_request(req,+__FUNCTION__)%3B%0A++++%0A++++return%3B%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cg112,filters:(b:'0',binary:'0',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'1',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O2+-c',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+gcc+11.2+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4"></iframe>

The
[`DF_OPT` macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L141-L155)
instructs the compiler to assume the flag is disabled, but leaves the
flag enabled (i.e., the conditional always evaluates
`request->tracing_mode`) until the library is initialised with
`dynamic_flag_init_lib`.[^unreachable] After initialisation,
the flag acts like a `DF_FEATURE` (i.e., the overhead is a `test eax`
instruction that falls through without any conditional branching)
until it is explicitly enabled again.

[^unreachable]: Or if the `dynamic_flag` library isn't aware of that `DF_OPT`, maybe because the function surrounding that `DF_OPT` conditional was loaded dynamically.

With this flag-before-check pattern, it's always safe to enable
`request_tracing` flags: in the worst case, we'll just look at the
request object, see that `request->tracing_mode == false`, and skip
the tracing logic. Of course, that's not ideal for performance. When
we definitely know that no request has asked for tracing, we want to
disable `request_tracing` flags and not even look at the request
object's `tracing_mode` field.

Whenever the application receives a request that opts into tracing, it
can enable all flags with kind `request_tracing` by
[executing `dynamic_flag_activate_kind(request_tracing, NULL)`](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L189-L205).
When that same request leaves the system (e.g., when the application
has fully sent a response back), the application
[undoes the activation with `dynamic_flag_deactivate_kind(request_tracing, NULL)`](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L207-L218).

Activation and deactivation calls actually increment and decrement
[counters associated with each instance of a `DF_...` macro](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/src/dynamic_flag.c#L99-L110), so this scheme works correctly when
multiple requests with overlapping lifetimes opt into tracing:
tracing blocks will check whether `request->tracing_mode == true`
whenever at least one in-flight request has `tracing_mode == true`, and
skip these conditionals as soon as no such request exists.

Practical considerations for programmatic manipulation
------------------------------------------------------

Confirming that a flag is set to its expected value (disabled for
`DF_FEATURE` and `DF_OPT`, enabled for `DF_DEFAULT`) is fast...
because we shifted all the complexity to the flag flipping
code. Changing the value for a set of flags is extremely slow
(milliseconds of runtime and several [IPIs](https://en.wikipedia.org/wiki/Inter-processor_interrupt) for
[multiple `mprotect(2)` calls](https://man7.org/linux/man-pages/man2/mprotect.2.html)), so it only makes sense to use dynamic flags when they are
rarely activated or deactivated (e.g., less often than once a minute
or even less often than once an hour).

We have found programmatic flag manipulation to be useful not just for
opt-in request tracing or to enable log statements, but also to
minimise the impact of complex logic on program phases that do not
require them. For example, mutual exclusion and
[safe memory reclamation deferral (PDF)](http://www.cs.toronto.edu/~tomhart/papers/tomhart_thesis.pdf)
may be redundant while a program is in a single-threaded
startup mode; we can guard such code behind `DF_OPT(steady_state, ...)`
to accelerate startup,
and enable `steady_state` flags just before spawning worker threads.

It can also make sense to guard slow paths with `DF_OPT` when
a program only enters phases that needs this slow path logic every few minutes. That was the case for a
[software transactional memory system with batched updates](https://www.youtube.com/watch?v=hZDr4pfz0Nc).
Most of the time, no update is in flight, so readers never have to
check for concurrent writes.  These checks can be guarded with
`DF_OPT(stm, ...)` conditions., as long as the program enables `stm`
flags around batches of updates.  Enabling and disabling all these
flags can take a while (milliseconds), but, as long as updates are
infrequent enough, the improved common case (getting rid of a memory
load and a conditional jump for a read barrier) means the tradeoff
is favourable.

Even when flags are controlled programmatically, it can be useful to
work around bugs by manually forcing some flags to remain enabled or
disabled.  In the tracing example above, we could find a crash in one
of the tracing blocks, and wish to prevent `request->tracing_mode` from
exercising that block of code.

It's easy to force a flag into an active state: flag activations
are counted, so it suffices to [activate it](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L220-L224)
manually, once. However, we want it to be safe [issue ad hoc `dynamic_flag_deactivate` calls](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L226-L230)
without wedging the system in a weird state, so activation counts don't go negative.
Unfortunately, this means we can't use deactivations
to prevent, e.g., a crashy request tracing block from being
activated.

Flags can instead be "unhooked" dynamically.  While unhooked,
increments to a flag's activation count are silently disregarded.
The [`dynamic_flag_unhook` function](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L232-L239)
unhooks `DF_*` conditions when their full name matches the extended POSIX regular expression it received as an argument.
When a flag has been
["unhook"ed](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L232-L239)
more often than it has been
["rehook"ed](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L241-L245),
attempts to activate it will
silently no-op. Once a flag has been unhooked, we can
[issue `dynamic_flag_deactivate` calls](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L226-L230)
until [its activation count reaches 0](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/src/dynamic_flag.c#L99-L110).
At that point, the flag is disabled, and will remain disabled
until rehooked.

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
otherwise. Activating and deactivating dynamic flags toggles the
corresponding target instructions between `test imm32` (0xA9) and `jmp rel32` (0xE9).

The `DF_...` macros expand into a
[lot more inline assembly than just that one instruction](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L443-L478);
the rest of the expansion is a lot of noise to
register everything with structs and pointers in dedicated
sections. Automatic static registration is mostly orthogonal to the
performance goals, but is key to the (lazy-)programmer-friendly
interface.

We use `test eax, imm32` instead of a nop because it's exactly five
bytes, just like `jmp rel32`, and because its 4-byte immediate is in
the same place as the 4-byte offset of `jmp rel32`. We can thus encode
the jump offset at assembly-time, and flip between falling through to
the common path (`test`) and jumping to the unexpected path (`jmp`) by
overwriting the opcode byte (0xA9 for `test`, 0xE9 for `jmp`).

Updating a single byte for each dynamic flag avoids questions
around the correct order for writes.  This single-byte
cross-modification (we overwrite instruction bytes while other threads
may be executing the mutated machine code) also doesn't affect the
size of the instruction (both `test eax` and `jmp rel` span 5 bytes),
which should hopefully suffice to avoid sharp edges around instruction
decoding in hardware, despite our disregard for
[Intel's recommendations regarding cross-modifying code in Section 8.1.3 of the SDM](https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-vol-3a-part-1-manual.pdf#page=260).[^cpu-millenia]

The library does try to protect against code execution exploits by
[relaxing and reinstating page protection with `mprotect(2)`](https://man7.org/linux/man-pages/man2/mprotect.2.html)) around
all cross modification writes.  Since `mprotect`-ing from
Read-Write-eXecute permissions to Read-eXecute acts as a
[`membarrier` (issues IPIs) on Linux/x86-64](https://lwn.net/Articles/728795/#:~:text=These%20users%20have%20found%20a%20trick%20to%20get%20the%20desired%20behavior%20without%20calling%20membarrier()%3A%20they%20make%20a%20call%20to%20either%20mprotect()%20or%20munmap()%20instead),
we can also know that the updated code is globally visible by the time
a [call to `dynamic_flag_activate`, etc.,](https://github.com/backtrace-labs/dynamic_flag/blob/00381c2cab5c8628e6a7d18730a98f7d7e6712f2/include/dynamic_flag.h#L189-L230) returns.

[^cpu-millenia]:  After a few CPU-millenia of production experience, the cross-modification logic hasn't been associated with any "impossible" bug, or with any noticeable increase in the rate of hardware hangs or failures.

It's not practical to bounce page protection for *each* `DF_` expansion,
especially with inlining (some users have hundreds of inlined calls to
flagged functions, e.g., to temporarily paper over
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
V, or let the library interoperate with a Rust implementation.
However, we don't expect changes to the interface, i.e., the `DF_` macros
and the activation/deactivation functions, nor to its core structure,
especially given the contemporary tastes for hardening (for example,
the cross-modification approach is completely incompatible with
OpenBSD's and OS X's strict `W^X` policies). The library works for our
target platforms, and we don't wish to take on extra complexity that
is of no benefit to us.

Of course, [it's Apache licensed](https://github.com/backtrace-labs/dynamic_flag/blob/main/LICENSE),
so anyone can fork the library and twist it beyond
recognition. However, if you're interested in powerful patching
capabilities, dynamic languages (e.g., Erlang, Common Lisp, or even
Python and Ruby), or tools like [Live++](https://liveplusplus.tech)
and [Recode](https://www.indefiant.com/) may be more appropriate.[^ask-games]
We want `dynamic_flag` to remain simple and just barely flexible
enough for our usage patterns.

[^ask-games]: The industry could learn a lot from game development practices, *especially* for stateful non-interactive backend servers and slow batch computations.

<small>Thank you, Jacob, Josh, and Per, for feedback on earlier
versions.</small>

<p><hr style="width: 50%"></p>
