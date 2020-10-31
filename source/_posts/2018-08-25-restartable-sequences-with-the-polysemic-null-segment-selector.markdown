---
layout: post
title: "Restartable sequences with the polysemic null segment selector"
date: 2018-08-25 21:57:06 -0400
comments: true
categories: AsymmetricSynchronisation
---

Implementing [non-blocking algorithms](https://en.wikipedia.org/wiki/Non-blocking_algorithm) is one of the few things that are
easier to code in a kernel than in userspace (the only other one I can
think of is physically wrecking a machine). In the kernel, we only
have to worry about designing a protocol that achieves forward
progress even if some threads stop participating. We must guarantee
the absence of conceptual locks (we can't have 
operations that must be paired, or barriers that everyone must
reach), but are free to implement the protocol by naïvely
spinlocking around individual steps: kernel code can temporarily
[disable preemption and interrupts](https://elixir.bootlin.com/linux/v4.18/source/kernel/locking/spinlock.c#L61) to bound a critical section's execution time.

Getting these non-blocking protocols right is still challenging, but
the challenge is one fundamental for reliable systems. The same
problems, solutions, and space/functionality trade-offs appear in
all distributed systems. Some would even argue that the kind of
interfaces that guarantee lock- or wait- freedom are closer to the
object oriented ideals.

Of course, there is still a place for clever instruction sequences
that avoid internal locks, for code that may be paused anywhere without
freezing the whole system: interrupts can't always be disabled, read
operations should avoid writing to shared memory if they can,
and a single atomic read-modify-write operation may be faster than
locking. The key point for me is that this complexity is opt-in: we
can choose to tackle it incrementally, as a performance problem rather
than as a prerequisite for correctness.

We don't have the same luxury in userspace. We can't start by focusing
on the fundamentals of a non-blocking algorithm, and only implement
interruptable sequences where it makes sense. Userspace can't disable
preemption, so we must think about the minutiae of interruptable code
sequences from the start; non-blocking algorithms in userspace are
always in hard mode, where every step of the protocol might be paused
at any instruction.

Specifically, the problem with non-blocking code in user space isn't
that threads or processes can be preempted at any point, but rather
that the preemption can be observed. It's a
[PCLSRing](http://fare.tunes.org/tmp/emergent/pclsr.htm) issue! Even
Unices guarantee programmers won't observe a thread in the middle of a
syscall: when a thread (process) must be interrupted, any pending
syscall either runs to completion, or returns with an error[^1].
What we need is a similar guarantee for steps of our own
non-blocking protocols[^sun].

[^1]: That's not as nice as rewinding the PC to just before the syscall, with a fixed up state that will resume the operation, but is simpler to implement, and usually good enough. Classic worst is better (Unix semantics are also safer with concurrency, but that could have been opt-in…).

[^sun]: That's not a new observation, and SUN heads like to point to prior art like [Dice's and Garthwaite's Mostly Lock-Free Malloc](https://pdfs.semanticscholar.org/5c9e/780fb6e6890d853fb5e44d1b7ce51a68a900.pdf), [Garthwaite's, Dice's, and White's work on Preemption notification for per-CPU buffers](https://www.usenix.org/legacy/events/vee05/full_papers/p24-garthwaite.pdf), or [Harris's and Fraser's Revocable locks](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.219.6486&rep=rep1&type=pdf). Linux sometimes has to reinvent everything with its special flavour.

Hardware transactional memory kind of solves the problem
(preemption aborts any pending transaction) but is a bit slow[^a], and
needs a fallback mechanism. Other emulation schemes for PCLSRing
userspace code divide the problem in two:

1. Determining that another thread was preempted in the middle of a
   critical section.
2. Guaranteeing that that other thread will not blindly resume
   executing its critical section (i.e., knowing that the thread knows
   that we know it's been interrupted[^2]).

[^a]: For instance, [SuperMalloc optimistically uses TSX to access per-CPU caches](http://supertech.csail.mit.edu/papers/Kuszmaul15.pdf), but TSX is slow enough that SuperMalloc first tries to use a per-thread cache. [Dice and Harris](https://timharris.uk/papers/2016-lhp.pdf) explored the use of hardware transactional lock elision solely to abort on context switches; they maintained high system throughput under contention by trying the transaction once before falling back to a regular lock.

[^2]: I did not expect systems programming to get near multi-agent epistemic logic ;)

The first part is relatively easy. For per-CPU data, it suffices to
observe that we are running on a given CPU (e.g., core #4), and that
another thread claims to own the same CPU's (core #4's) data. For
global locks,
[we can instead spin for a while before entering a slow path](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.219.6486&rep=rep1&type=pdf) 
that determines whether the holder has been preempted, by reading
scheduling information in [`/proc`](http://man7.org/linux/man-pages/man5/proc.5.html).

The second part is harder. I have played with schemes that relied on
[signals](http://man7.org/linux/man-pages/man7/signal.7.html), but was never satisfied: I found Linux perf
will rarely, but not never, drop interrupts when I used it to
"profile" context switches, and signaling when we determine that
the holder has been pre-empted has memory visibility issues for
per-CPU data[^3].

[^3]: Which is fixable with LOCKed instructions, but that defeats the purpose of per-CPU data.

Until earlier this month, the best known solution on mainline Linux
involved *cross-modifying code*! When a CPU executes a memory write
instruction, that write is affected by the registers, virtual memory
mappings, and the instruction's bytes. Contemporary operating systems
rarely let us halt and tweak another thread's general purpose
registers (Linux won't let us self-[ptrace](http://man7.org/linux/man-pages/man2/ptrace.2.html), nor pause an individual
thread). Virtual memory mappings are per-process, and can't be
modified from the outside. The only remaining angle is modifying the
premptee's machine code.

That's what [Facebook's experimental library Rseq (restartable sequences)](https://github.com/facebookexperimental/Rseq) actually does.

I'm not happy with that solution either: while it "works," it requires
per-thread clones of each critical section, and makes us deal with
cross-modifying code. I'm not comfortable with leaving code pages
writable, and we also have to guarantee the pre-emptee's writes are
visible. For me, the only defensible implementation is to modify
the code by mmap-ing pages in place, which incurs an IPI per
modification. The total system overhead thus scales superlinearly with
the number of CPUs.

With Mathieu Desnoyers's, Paul Turner's, and Andrew Hunter's patch to
[add an rseq syscall to Linux 4.18](https://github.com/torvalds/linux/blob/v4.18/kernel/rseq.c), we
finally have a decent answer. Rather than triggering special code when
a thread detects that another thread has been pre-empted in the middle
of a critical section, userspace can associate recovery code with the
address range for each restartable critical section's
instructions. Whenever the kernel preempts a thread, it detects
whether the interruptee is in such a restartable sequence, and, if so,
redirects the instruction pointer to the associated recovery code.
This essentially means that critical sections must be read-only
except for the last instruction in the section, but that's not too
hard to satisfy. It also means that we incur recovery even when no one
would have noticed, but the overhead should be marginal (there's at
most one recovery per timeslice), and we get a simpler programming
model in return.

Earlier this year, I found another way to prevent critical sections
from resuming normal execution after being preempted. It's a total
hack that exercises a state saving defect in Linux/x86-64, but I'm
comfortable sharing it now that Rseq is in mainline: if anyone needs
the functionality, they can update to 4.18, or backport the feature.

Here's a riddle!
----------------

{% codeblock riddle.c %}
static const char values[] = { 'X', 'Y' };

static char
read_value(void)
{
        /*
         * New feature in GCC 6; inline asm would also works.
         * https://gcc.gnu.org/onlinedocs/gcc-6.1.0/gcc/Named-Address-Spaces.html#Named-Address-Spaces
         */
        return *(const __seg_gs char *)(uintptr_t)values;
}

int
main(int argc, char **argv)
{
        /* ... */
        char before_switch = read_value();  /* Returns 'X'. */
        usleep(1000 * 1000);  /* Or any other wait for preemption. */
        char after_switch = read_value();  /* Returns 'Y'. */
        /* ... */
}
{% endcodeblock %}

With an appropriate setup, the `read_value` function above will return
a different value once the executing thread is switched out. No, the
kernel isn't overwriting read-only data while we're switched out. When
I listed the set of inputs that affect a memory store or load
instruction (general purpose registers, virtual memory mappings, and
the instruction bytes), I left out one last x86 thing: segment registers.

Effective addresses on x86oids are about as feature rich as it gets:
they sum a base address, a shifted index, a constant offset, *and,
optionally, a segment base*. Today, we simply use segment bases to implement
thread-local storage (each thread's `FS` or `GS` offset points to its
thread-local block), but that usage
repurposes memory segmentation, an old 8086 feature… and x86-64 still
maintains some backward compatibility with its 16-bit ancestor.
There's a lot of unused complexity there, so it's plausible that we'll
find information leaks or otherwise flawed architectural state
switching by poking around segment registers.

How to set that up
------------------

After learning about this [trick to observe interrupts from userland](http://lackingrhoticity.blogspot.com/2018/01/observing-interrupts-from-userland-on-x86.html),
I decided to do a close reading of Linux's task switching code on
x86-64 and eventually found [this interesting comment](https://elixir.bootlin.com/linux/v4.18/source/arch/x86/kernel/process_64.c#L174)[^4].

[^4]: I actually found the logic bug before the Spectre/Meltdown fire drill and was worried the hole would be plugged. This one survived the purge. *fingers crossed*

Observing a value of `0` in the `FS` or `GS` registers can mean
two things:

1. Userspace explicitly wrote the null segment selector in there,
   and reset the segment base to `0`.
2. The kernel wrote a `0` in there before setting up the segment base
   directly, with `WR{FS,GS}BASE` or by writing to a model-specific
   register (MSR).

Hardware has to efficiently keep track of which is actually in
effect. If userspace wrote a `0` in `FS` or `GS`, prefixing an
instruction with that segment has no impact; if the MSR write is
still active (and is non-zero), using that segment must
impact effective address computation.

There's no easy way to do the same in software. Even in ring 0, the
only sure-fire way to distinguish between the two cases is to actually
read the current segment base value, and that's slow. Linux instead
fast-paths the common case, where the segment register is 0 because
the kernel is handling segment bases.  It prioritises that use case so
much that the code knowingly sacrifices correctness when userspace
writes `0` in a segment register after asking the kernel to setup its
segment base directly.

This incorrectness is acceptable because it only affects the thread
that overwrites its segment register, and no one should go through
that sequence of operations. Legacy code can still manipulate segment
descriptor tables and address them in segment registers.  However,
being legacy code, it won't use the modern syscall that directly
manipulates the segment base. Modern code can let the kernel
set the segment base without playing with descriptor tables, and
has no reason to look at segment registers.

The only way to observe the buggy state saving is to go looking for
it, with something like the code below (which uses `GS` because `FS`
is already taken by `glibc` to implement thread-local storage).

{% codeblock h4x.c %}
#define RUN_ME /*
gcc-6 -std=gnu99 $0 -o h4x && ./h4x; exit $?;

Should output
Reads: XXYX
Re-reads: XYX
*/
#define _GNU_SOURCE
#include <asm/prctl.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <unistd.h>

static const char values[] = { 'X', 'Y' };

/* Directly set GS's base with a syscall. */
static void
set_gs_base(unsigned long base)
{
        int ret = syscall(__NR_arch_prctl, ARCH_SET_GS, base);
        assert(ret == 0);
}

/* Write a 0 in GS. */
static void
set_gs(unsigned short value)
{
        asm volatile("movw %0, %%gs" :: "r"(value) : "memory");
}

/* Read gs:values[0]. */
static char
read_value(void)
{
        /*
         * New feature in GCC 6; inline asm would also works.
         * https://gcc.gnu.org/onlinedocs/gcc-6.1.0/gcc/Named-Address-Spaces.html#Named-Address-Spaces
         */
        return *(const __seg_gs char *)(uintptr_t)values;
}

int
main(int argc, char **argv)
{
        char reads[4];
        char re_reads[3];

        reads[0] = read_value();
        reads[1] = (set_gs(0), read_value());
        reads[2] = (set_gs_base(1), read_value());
        reads[3] = (set_gs(0), read_value());
        
        printf("Reads: %.4s\n", reads);
        fflush(NULL);

        re_reads[0] = read_value();
        re_reads[1] = (usleep(1000 * 1000), read_value());
        re_reads[2] = (set_gs(0), read_value());

        printf("Re-reads: %.3s\n", re_reads);
        return 0;
}
{% endcodeblock %}

Running the above on my Linux 4.14/x86-64 machine yields

    $ gcc-6 -std=gnu99 h4x.c && ./a.out
    Reads: XXYX
    Re-reads: XYX

The first set of reads shows that:

1. our program starts with no offset in `GS` (`reads[0] == values[0]`)
2. explicitly setting `GS` to 0 does not change that (`reads[1] == values[0]`)
3. changing the `GS` base to 1 with `arch_prctl` does work (`reads[2] == values[1]`)
4. resetting the `GS` selector to 0 resets the base (`reads[3] == values[0]`).

The second set of reads shows that:

1. the reset base survives short syscalls (`re_reads[0] == values[0]`)
2. an actual context switch reverts the `GS` base to the `arch_prctl`
   value (`re_reads[1] == values[1]`)
3. writing a 0 in `GS` resets the base again (`re_reads[2] == values[0]`).

Cute hack, why is it useful?
----------------------------

The property demonstrated in the hack above is that, after our call to
`arch_prctl`, we can write a `0` in `GS` with a regular instruction to
temporarily reset the `GS` base to 0, and know it will revert to the
`arch_prctl` offset again when the thread resumes execution, after
being suspended.

We now have to ensure our restartable sequences are no-ops when the
`GS` base is reset to the `arch_prctl` offset, and that the no-op is
detected as such. For example, we could set the `arch_prctl` offset to
something small, like 4 or 8 bytes, and make sure that any address we
wish to mutate in a critical section is followed by 4 or 8 bytes of
padding that can be detected as such.  If a thread is switched out in
the middle of a critical section, its `GS` base will be reset to 4 or
8 when the thread resumes execution; we must guarantee that this 
offset will make the critical section's writes fail.

If a write is a compare-and-swap, we only have to make sure the
padding's value is unambiguously different from real data: reading the
padding instead of the data will make compare-and-swap fail, and the
old value will tell us that it failed because we read padding, which
should only happen after the section is pre-empted. We can play
similar tricks with fetch-and-add (e.g., real data is always even,
while the padding is odd), or atomic bitwise operations (steal the
sign bit).

If we're willing to eat a signal after a context switch, we can set
the `arch_prctl` offset to something very large, and take a
segmentation fault after being re-scheduled.  Another option is to set
the `arch_prctl` offset to 1, and use a double-wide compare-and-swap
(`CMPXCHG16B`), or turn on the AC (alignment check) bit in
EFLAGS. After a context switch, our destination address will be
misaligned, which will trigger a `SIGBUS` that we can handle.

The last two options aren't great, but, if we make sure to regularly
write a 0 in `GS`, signals should be triggered rarely, only when
pre-emption happens between the last write to `GS` and a critical
section. They also have the advantages of avoiding the need for
padding, and making it trivial to detect when a restartable section was
interrupted. Detection is crucial because it often isn't safe to
assume an operation failed when it succeeded (e.g., unwittingly
succeeding at popping from a memory allocator's freelist would leak
memory). When a `GS`-prefixed instruction fails, we must be able to
tell from the instruction's result, and nothing else. We can't just
check if the segment base is still what we expect, after the fact: our
thread could have been preempted right after the special `GS`-prefixed
instruction, before our check.

Once we have restartable sections, we can use them to implement
per-CPU data structures (instead of per-thread), or to let thread
acquire locks and hold them until they are preempted: with
restartable sections that only write if there was no preemption
between the lock acquisition and the final store instruction, we can
[create a revocable lock abstraction](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.219.6486&rep=rep1&type=pdf) and implement [wait-free coöperation](https://www.cl.cam.ac.uk/research/srg/netos/papers/2002-casn.pdf) or [flat-combining](http://mcg.cs.tau.ac.il/papers/spaa2010-fc.pdf).

Unfortunately, our restartable sections will always be hard to debug:
observing a thread's state in a regular debugger like GDB will reset
the `GS` base and abort the section. That's not unique to the segment
hack approach.  Hardware transactional memory will abort critical
sections when debugged, and there's similar behaviour with the
official `rseq` syscall. It's hard enough to PCLSR userspace code; it
would be even harder to
PCLSR-except-when-the-interruption-is-for-debugging.

Who's to blame?
---------------

The null `GS` hack sounds like it only works because of a pile of
questionable design decisions. However, if we look at the historical
context, I'd say everything made sense.

Intel came up with segmentation back when 16 bit pointers were big,
but 64KB of RAM not quite capacious enough.  They didn't have 32 bit
(never mind 64 bit) addresses in mind, nor threads; they only wanted
to address 1 MB of RAM with their puny registers.  When thread
libraries abused segments to implement thread-local storage, the only
other options were to over-align the stack and hide information there,
or to steal a register. Neither sounds great, especially with x86's
six-and-a-half general purpose registers.  Finally, when AMD decided
to rip out segmentation, but keep `FS` and `GS`, they needed to make
porting x86 code as easy as possible, since that was the whole value
proposition for AMD64 over Itanium.

I guess that's what systems programming is about. We take our
tools, get comfortable with their imperfections, and use that
knowledge to build new tools by breaking the ones we already have ([#Mickens](https://www.usenix.org/system/files/1311_05-08_mickens.pdf)).

<small>Thank you Andrew for a fun conversation that showed the segment
hack might be of interest to someone else, and to Gabe for snarkily
reminding us `Rseq` is another Linux/Silicon Valley
re-invention.</small>

<p><hr style="width: 50%"></p>
