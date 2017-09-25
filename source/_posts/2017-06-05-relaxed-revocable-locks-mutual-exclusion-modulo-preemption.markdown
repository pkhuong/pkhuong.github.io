---
layout: post
title: "Relaxed revocable locks: mutual exclusion modulo preemption"
date: 2017-06-05 00:25:59 -0400
publish: false
comments: true
categories: 
---

_Update: there's a way to detect "running" status even across cores.
 It's not pretty.  Search for `/proc/sched_debug`._

The hard part about locking tends not to be the locking itself, but
preemption.  For example, if you structure a memory allocator
like jemalloc, you want as few arenas as possible; one per CPU would
be ideal, while one per thread would affect fragmentation and make
some operations scale linearly with the number of threads.  However,
you don't want to get stuck when a thread is preempted while it owns
an arena.  The usual fix is two-pronged:

1. have a few arenas per CPU (e.g., jemalloc defaults to 4x the
number of CPUs);
2. hold exclusive ownership for short critical sections.

The first tweak isn't that bad; scaling the number of arenas, stats
regions, etc. with the number of CPUs is better than scaling with the
number of threads.  The second one really hurts performance: *each*
allocation must acquire a lock with an interlocked write.  Even if the
arena is (mostly) CPU-local, the atomic wrecks your pipeline.

It would be nice to have locks that a thread can acquire once per
scheduling quantum, and benefit from ownership until the thread is
scheduled out.  We could then have a few arenas per CPU (if only to
handle migration), but amortise lock acquisition over the timeslice.

That's not a new idea.  [Dice and Garthwaite](http://dl.acm.org/citation.cfm?id=512451) described this
exact application in 2002
[(PDF)](https://pdfs.semanticscholar.org/5c9e/780fb6e6890d853fb5e44d1b7ce51a68a900.pdf)
and refer to older work for uniprocessors.  However, I think the best
exposition of the idea is Harris and Fraser's
[Revocable locks for non-blocking programming](http://dl.acm.org/citation.cfm?id=1065954),
published in 2005 [(PDF)](https://pdfs.semanticscholar.org/21eb/486b4ef3b059d782ea1976b4ea5985e417df.pdf).  Harris and Fraser want revocable locks for
non-blocking multiwriter code; our problem is easier, but only marginally so.
Although the history of revocable locks is pretty Solaris-centric, Linux
is catching up.   Google, Facebook, and EfficiOS (LTTng) have been pushing for
[restartable sequences](https://lwn.net/Articles/697979/), which is essentially
OS support for sections that are revoked on context switches.  Facebook even
has a pure userspace implementation with
[Rseq](https://github.com/facebookexperimental/Rseq); they report good
results for jemalloc.

Facebook's Rseq implements almost exactly what I described above, for
the exact same reason (speeding up a memory allocator or replacing
miscellaneous per-thread structs with ~per-CPU data).  However,
they're trying to port a kernel idiom directly to userspace:
restartable sequences implement strict per-CPU data.  With kernel
supports, that makes sense.  Without such support though, strict
per-CPU data incurs a lot of extra complexity when a thread migrates
to a new CPU: Rseq needs an asymmetric fence to ensure that the
evicted thread observes its eviction and publishes any write it
performed before being evicted.

I'm not sure that's the best fit for userspace.  We can avoid a lot of
complexity by instead dynamically allocating a few arenas (exclusive
data) per CPU and assuming only a few threads at a time will be
migrated *while owning arenas*.

Here's the relaxed revocable locks interface I propose:

1. Each thread has a thread state struct.  That state struct has:
    * a generation counter;
    * a canceled counter (generation - 1 or equal to generation);
    * a signaled counter (generation - 1 or equal to generation);
    * an acknowledged cancel counter (generation - 1 or equal to generation);
    * an "in critical section" flag (pointer to a revocable lock).

2. Locks are owned by a pair of thread state struct and generation
   counter (ideally packed in one word, but two words are doable).
   Threads acquire locks with normal compare-and-swap, but may bulk
   revoke every lock they own by advancing their generation counter.

3. Threads may execute any number of conditional stores per lock
   acquisition.  Lock acquisition returns an ownership descriptor
   (pair of thread state struct and generation counter), and
   `rlock_store_64(descriptor, lock, dst, value)` stores `value` in
   `dst` if the descriptor still owns the lock and the ownership has
   not been cancelled.

4. Threads do not have to release lock ownership to let others make
   progress: any thread may attempt to cancel another thread's
   ownership of a lock.  After `rlock_owner_cancel(descriptor, lock)`
   returns successfully, the victim will not execute a conditional
   store under the notion that it still owns `lock` with `descriptor`.

The only difference from Rseq is that `rlock_owner_cancel` may fail.
In practice, it will only fail if a thread on CPU A attempts to cancel
ownership for a thread that's currently running on another CPU B.
That could happen after migration, but also when an administrative
task iterates through every (pseudo-)per-CPU struct without changing
its CPU mask.  Being able to iterate through all available
pseudo-per-CPU data without migrating to the CPU is big win for slow
paths; another advantage of not assuming strict per-CPU affinity.

Rather than failing on migration, Rseq issues an asymmetric fence to
ensure both its writes and the victim's writes are visible.  At best,
that's implemented with inter-processor interrupts (IPIs) that scale
linearly with the number of CPUs… for a point-to-point signal.  I
oversubscribed a server with 2-4x more threads than CPUs, and thread
migrations happened at a constant frequency per CPU.  Incurring
`O(#CPU)` IPIs for every migration makes the *per-CPU* overhead of
Rseq linear with the number of CPUs (cores) in the system.  I'm also
wary of the high rate of code self/cross -modification in Rseq:
`mprotect` incurs IPIs when downgrading permissions, so Rseq must
leave some code page with writes enabled.  These downsides (potential
for IPI storm and lack of W\^X) aren't unique to Rseq.  I think
they're inherent to emulating unpreempted per-CPU data in userspace
without explicit OS support.

When `rlock_owner_cancel` fails, I expect callers to iterate down the
*list* of pseudo-per-CPU structs associated with the CPU and eventually
append a new struct to that list.  In theory, we could end up with as
many structs in that list as the peak number of thread on that CPU; in
practice, it should be a small constant since `rlock_owner_cancel`
only fails after thread migration.

Code for Rlock (Linux/x86-64 only)
----------------------------

I [dumped my code as a gist](https://gist.github.com/pkhuong/a622e031e92f7fdfb1df1b49a7627d54), but it is definitely hard to follow, so I'll try to
explain it here.

Bitpacked ownership records must include the address of the owner
struct and a sequence counter.  Ideally, we'd preallocate some address
space and only need 20-30 bits to encode the address.  For now, I'm
sticking to 64 byte aligned allocations and rely on x86-64's 48 bits
of address space.  With 64 bit owner/sequence records, an `rlock`
is a 64 bit spinlock.

{% codeblock "rlock.c" %}
typedef union rlock_owner_seq {
        uint64_t bits;
        struct {
                uint64_t sequence:22;
                uint64_t address:42;
        };
} rlock_owner_seq_t;

struct rlock {
        rlock_owner_seq_t owner;
};
{% endcodeblock %}

In the easy case, acquiring an `rlock` means:

1. reading the `owner` field (with a 64 bit load);
2. confirming that the owner has advanced its sequence;
3. CASing in our own `rlock_owner_seq_t`.

But first, we must make canonicalise our own `owner` struct.

{% codeblock "rlock.c" %}
struct rlock_owner {
        /* SPMC. */
        rlock_owner_seq_t seq;
        /* MPMC: Asked to cancel up to here (inclusively). */
        uint32_t cancel_sequence;
        /* MPMC: Signaled to cancel up to here (inclusively). */
        uint32_t signal_sequence;
        /* SPMC: Acked cancel ask up to here (inclusively). */
        uint32_t acked_sequence;
        /* Private: forcibly release lock after too many ops. */
        uint32_t op_count;
        /* SPMC */
        pid_t tid;
        /* SPMC; "in critical section" flag. */
        struct rlock *critical_section;
} __attribute__((__aligned__(64)));
{% endcodeblock %}

Rlock lazily allocates an `rlock_owner` per thread and stores it in
TLS; we can't free that memory without some safe memory reclamation
scheme (and I'd like to use Rlock to implement SMR), but it is
possible to use a type-stable freelist.

Regardless of the allocation/reuse strategy, canonicalising an rlock
means making sure we observe any cancellation request.

{% codeblock "rlock.c" %}
static inline bool
update_self(struct rlock_owner *self)
{
        rlock_owner_seq_t snapshot = { .bits = self->seq.bits };
        uint32_t cancel_sequence = ck_pr_load_32(&self->cancel_sequence);

        /* We've been asked to cancel if cancel_sequence == seq.sequence. */
        if (LIKELY(self->seq.sequence != cancel_sequence)) {
                return false;
        }
        
        ck_pr_fas_32(&self->cancel_sequence, snapshot.sequence);
        ck_pr_fas_32(&self->signal_sequence, snapshot.sequence);
        ck_pr_fas_32(&self->acked_sequence, snapshot.sequence);

        snapshot.sequence++;
        ck_pr_fas_64(&self->seq.bits, snapshot.bits);
        return true;
}

static inline struct rlock_owner *
get_self(void)
{
        struct rlock_owner *self;

        self = rlock_self;
        if (UNLIKELY(self == NULL)) {
                self = allocate_self();
        }

        update_self(self);
        return self;
}
{% endcodeblock %}

To acquire a lock we observe the current owner, attempt to cancel its
ownership, and (if we did cancel ownership) CAS in our own
owner/sequence descriptor.

{% codeblock "rlock.c" %}
rlock_owner_seq_t
rlock_lock(struct rlock *lock)
{
        struct rlock_owner *self = get_self();
        rlock_owner_seq_t seq, snapshot;

        /* Load the current owner. */
        snapshot.bits = ck_pr_load_64(&lock->owner.bits);
        /* Easy case: we already own the lock. */
        if (snapshot.bits == self->seq.bits) {
                return self->seq;
        }

        for (;;) {
                seq.bits = self->seq.bits;

                /* Make sure the current owner isn't anymore. */
                if (!rlock_owner_cancel(snapshot, lock)) {
                        /* Couldn't; return 0. */
                        seq.bits = 0;
                        return seq;
                }

                /* Replace the old owner with ourself. */
                if (ck_pr_cas_64_value(&lock->owner.bits,
                    snapshot.bits, seq.bits, &snapshot.bits)) {
                        /* Success! */
                        break;
                }

                /* CAS failed.  snapshot.bits has the new owner. */
                /* Eagerly observe any cancellation. */
                update_self(self);
                /* CAS failed. Spin a bit. */
                ck_pr_stall();
        }

        return seq;
}
{% endcodeblock %}

Most of the trickiness hides in `rlock_owner_cancel`.

{% codeblock "rlock.c" %}
bool
rlock_owner_cancel(union rlock_owner_seq owner,
    struct rlock *evict)
{
        struct rlock_owner *victim = (void *)((uintptr_t)owner.address * 64);
        rlock_owner_seq_t snapshot;
        uint32_t acked;
        uint32_t sequence = owner.sequence;

        assert(evict != NULL);
        /* Easy case: no owner. */
        if (victim == NULL) {
                return true;
        }

        snapshot.bits = ck_pr_load_64(&victim->seq.bits);
        if (snapshot.bits != owner.bits) {
                /* The victim has already moved on to a new sequence value. */
                return true;
        }

        acked = ck_pr_load_32(&victim->acked_sequence);
        if (mod_lte(sequence, acked)) {
                /* We already have acked cancellation >= sequence. */
                return true;
        }

        /* Advance the victim's cancel counter to sequence. */
        if (!ensure_cancel_sequence(victim, sequence)) {
                /* Already advanced; nothing to do! */
                return true;
        }

        if (victim_running(victim)) {
                /* The victim isn't obviously scheduled out;

                /* See if we must ensure visibility of our cancel. */
                snapshot.bits = ck_pr_load_64(&victim->seq.bits);
                if (snapshot.bits == owner.bits) {
                        ensure_signal_sequence(victim, sequence);
                }

                return false;
        }

        if (ck_pr_load_ptr(&victim->critical_section) != evict) {
                /*
                 * Easy case: victim isn't in a critical section with
                 * our lock.  The victim has either been scheduled out
                 * since we called `ensure_cancel_sequence`, our went
                 * through a context switch at least once.  In either
                 * case, it has already observed the cancellation or
                 * will before the next critical section.
                 */
                return true;
        }

        /*
         * The victim might be in the middle of a critical section.
         * Send a signal that'll skip the critical section if
         * necessary.
         */
        ensure_signal_sequence(victim, sequence);
        /*
         * If the victim is definitely not running, it either has
         * already executed the signal handler or will before resuming
         * normal execution.  If the victim might be running,
         * we can only hope we got lucky.
         */
        if (!victim_running(victim)) {
                return true;
        }

        /*
         * We know the vitim was scheduled out before we signaled for
         * cancellation.  We can see if the victim has released our
         * critical section at least once since then.
         */
        return (ck_pr_load_ptr(&victim->critical_section) != evict);
}
{% endcodeblock %}

The fancy stuff begins around `ensure_cancel_sequence(victim, sequence);`.
Our code maintains the invariant that the MPMC sequences
(`cancel_sequence`, `signal_sequence`) are either the SPMC `sequence - 1`
 (normal state), or exactly the SPMC sequence (cancellation
request).

`ensure_cancel_sequence` CASes the `cancel_sequence` field from its
expected value of `owner.sequence - 1` to `owner.sequence`.  If
the actual value is neither of them, the owner has already
advanced to a new sequence value, and we're done.

Otherwise, we have to hope the victim isn't running.

Now comes the really tricky stuff.  Our CAS is immediately visible
globally.  The issue is that the victim might already be in the middle
of a critical section.  When writers executes a critical sections, they:

1. Set the critical section flag (with a normal write);
2. Check that the lock hasn't been revoked;
3. Perform the write;
4. Clear the critical section flag.

It's really hard to guarantee that the write in step 1 is visible
(without killing performance in the common case), and if it is, that
the victim isn't about to execute step 3.

We get that guarantee by determining that the victim hasn't been
continuously executing since the time we attempted to CAS the
`cancel_sequence` forward.  That's (hopefully) enough of a barrier to
order the CAS, step 1, and our read of the critical section flag.

That's not information that Linux exposes directly.  However, we can
borrow a trick from `Rseq` and read `/proc/self/task/[tid]/stat`.  The
contents of that file include whether the task is \(R\)unnable (or
(S)leeping, waiting for (D)isk, etc.), and the CPU on which the task
last executed.

If the task isn't runnable, it definitely hasn't been running
continuously since the CAS.  If the task is runnable but last ran on
the CPU the current thread is itself running on (and the current
thread wasn't migrated in the middle of reading the stat file), it's
not running now.

If the task is runnable on another CPU, we can try to look at
`/proc/sched_debug`: each CPU has a `.curr->pid` line that tells us
the PID of the task that's currently running (0 for none).  That file
has a lot of extra information so reading it is *really* slow, but we
only need to do that after migrations.

Finally, the victim might really be running.  Other proposals would
fire an IPI; we instead ask the caller to allocate a few more
pseudo-per-CPU structs.

Assuming we did get a barrier out of the scheduler, we hopefully
observe that the victim's critical section flag is clear.  If that
happens, we had:

1. CAS the cancellation sequence;
2. Barrier *in the victim* from being scheduled out;
3. Critical section flag was empty after the CAS.

This guarantees that the victim hasn't been in the same critical
section since the CAS in step 1.  Either it's not in a critical
section, or if it is, it's a fresh one that will observe the CAS.
It's safe to assume the victim has been successfully evicted.

The less happy path happens when we observe that the victim's critical
section flag is set.  We must assume that it was scheduled out in
the middle of a critical section.  We'll send a (POSIX) signal to the
victim: the handler will skip over the critical section if the victim
is still in one.  Once that signal is sent, we know that the first
thing Linux will do is execute the handler when the victim resumes
execution.  If the victim is still not running after `tgkill`
returned, we're good to go: if the victim is still in the critical
section, the handler will fire when it resumes execution.

Otherwise, the victim might have been scheduled in between the CAS and
the signal; we still have the implicit barrier given by the context
switch between CAS and signal, but we can't rely on signal execution.
We can only hope to observe that the victim has noticed the
cancellation request and advanced its sequence, or that it cleared its
critical section flag.

The rest is straightforward.  The `rlock_store_64` must observe any
cancellation, ensure that it still holds the lock, and enter the
critical section:

1. set the critical section flag (overwrite with the lock's address);
2. check again that we still hold the lock and have not been asked to cancel;
3. flip the result flag to "success";
4. store.

Once it leaves the critical section, `rlock_store_64` clears the
critical section flags, looks for any cancellation request, and
returns success/failure.  The critical section is in inline assembly
for the signal handler: executing the store in step 4 implicitly
marks the end of the critical section.

{% codeblock "rlock.c" %}
bool
rlock_store_64(rlock_owner_seq_t snapshot,
    struct rlock *lock, uint64_t *dst, uint64_t value)
{
        struct rlock_owner *self = (void *)((uintptr_t)snapshot.address * 64);
        rlock_owner_seq_t seq;
        uint32_t op_count;
        int status;

        seq.bits = self->seq.bits;
        op_count = ++self->op_count;
        /* We cancelled this lock. */
        if (UNLIKELY(seq.bits != snapshot.bits)) {
                return false;
        }

        /* The handler will reset RAX to 1 on skip. */
        status = 1;
        asm volatile(
            /* Move the lock's address in the critical section flag. */
            "0: movq %[lock], %[critical_section]\n\t"
            /* Do we still own the lock? */
            "cmpq %[owner], %[snapshot]\n\t"
            "jne 1f\n\t"
            /* Were we asked to cancel? */
            "cmpl %[cancelled], %[seq]\n\t"
            "je 1f\n\t"
            /* Success path! Set status to 0. */
            "xorl %[status], %[status]\n\t"
            /* Store the value in *dst. */
            "movq %[value], %[dst]\n\t"
            /* End of critical section. */
            "1:\n\t"

            /*
             * Make sure the signal handler knows where the
             * critical section code begins & ends.
             */
            ".pushsection rlock_store_list, \"a\", @progbits\n\t"
            ".quad 0b, 1b\n\t"
            ".popsection\n\t"
                : [status] "+a"(status),
                  [critical_section] "+m"(self->critical_section),
                  [dst] "=m"(*dst)
                : [lock] "r"(lock),
                  [snapshot] "r"(snapshot.bits),
                  [owner] "m"(lock->owner.bits),
                  [seq] "r"((uint32_t)seq.sequence),
                  [cancelled] "m"(self->cancel_sequence),
                  [value] "r"(value)
                : "memory", "cc");

        /* Clear the flag. */
        ck_pr_store_ptr(&self->critical_section, NULL);

        /* Acknowledge any cancellation request. */
        if (UNLIKELY(status != 0)) {
                update_self(self);
                return false;
        }

        /* Force lock reacquisition after a couple thousand writes. */
        if (UNLIKELY(op_count >= OP_LIMIT)) {
                self->op_count = 0;
                rlock_owner_release();
        }

        return true;
}
{% endcodeblock %}

Finally, the signal handler for rlock cancellation requests iterates
through the `rlock_store_list` section until it finds a record that
strictly includes the instruction pointer.  If there is such a record,
the thread is in a critical section, and we can skip it by overwriting
`RIP` (to the end of the critical section) and setting `RAX` to 1.

{% codeblock "rlock.c" %}
void
rlock_signal_handler(int signal, siginfo_t *info, void *arg)
{
        ucontext_t *ctx = arg;
        mcontext_t *mctx = &ctx->uc_mcontext;
        struct rlock_owner *self = rlock_self;
        uintptr_t rip;
        size_t nloc = __stop_rlock_store_list - __start_rlock_store_list;

        (void)signal;
        (void)info;

        rip = (uintptr_t)mctx->gregs[REG_RIP];
        for (size_t i = 0; i < nloc; i++) {
                struct rlock_store record;

                record = __start_rlock_store_list[i];
                if (rip < record.begin || rip >= record.end) {
                        continue;
                }

                assert(self != NULL);

                /* skip the critical instruction. */
                mctx->gregs[REG_RIP] = record.end;
                /* set the interrupted flag. */
                mctx->gregs[REG_RAX] = 1;
                return;
        }

        /* Might as well publish that we observed any cancellation request. */
        if (self != NULL) {
                ck_pr_fas_32(&self->acked_sequence,
                    ck_pr_load_32(&self->cancel_sequence));
        }

        return;
}
{% endcodeblock %}

Silly benchmarks
----------------

On my 2.9 GHz Sandy Bridge, a baseline loop to increment a counter a
billion times takes 6.9 cycles per increment, which makes sense given
that I use inline assembly loads and stores to prevent any compiler
cleverness.

The same loop with an interlocked store (`xchg`) takes 36 cycles per
increment.

Interestingly, an `xchg`-based spinlock around normal increments only
takes 31.7 cycles per increment (0.44 IPC).  If we wish to back our
spinlocks with futexes, we must unlock with an interlocked write; releasing
the lock with a compare-and-swap brings us to 53.6 cycles per
increment (0.30 IPC)!  Atomics really mess with pipelining: unless
they're separated by dozens or even hundreds of instructions, their
barrier semantics (that we usually need) practically forces an
in-order, barely pipelined, execution.

FWIW, 50ish cycles per transaction is close to what I see in
microbenchmarks for Intel's RTM/HLE.  So, while the overhead of TSX is
non-negligible for very short critical sections, it seems more than
reasonable for adaptive locks (and TSX definitely helps when
preemption happens, as shown by Dice and Harris in
[Lock Holder Preemption Avoidance via Transactional Lock Elision](https://timharris.uk/papers/2016-lhp.pdf)).

Finally, the figure that really matters: when incrementing with
`rlock_store_64`, we need 13 cycles per increment.  That loop hits
2.99 IPC, so I think the bottleneck is just the number of instructions
in `rlock_store_64`.  The performance even seems independent of the
number of worker threads, as long as they're all on the same CPU.

In tabular form:

    | Method               | Cycle / increment | IPC  |
    |----------------------|-------------------|------|
    | Vanilla              |             6.961 | 1.15 |
    | xchg                 |            36.054 | 0.22 |
    | FAS spinlock         |            31.710 | 0.44 |
    | FAS-CAS lock         |            53.656 | 0.30 |
    | Rlock, 1 thd         |            13.044 | 2.99 |
    | Rlock, 4 thd / 1 CPU |            13.099 | 2.98 |
    | Rlock, 256 / 1       |            13.952 | 2.96 |
    | Rlock, 2 / 2         |            13.047 | 2.99 |

Six more cycles per write versus thread-private storage really isn't
that bad (accessing TLS in a shared library might add as much
overhead)… especially compared to 25-50 cycles (in addition to
indirect slowdowns from the barrier semantics) with locked
instructions.

I also have a statistics-gathering mode that lets me vary the fraction
of cycles spent in critical sections.  On my server, the frequency of
context switches between CPU-intensive threads scheduled on the same
CPU increases in steps until seven or eight threads; at that point,
the frequency tops out at one switch per jiffy (250 Hz).  Apart from
this scheduling detail, evictions act as expected (same
logic as for sampled profiles).  The number of evictions is almost
equal to the number of context switches, which is proportional to the
runtime.  However, the number of hard evictions (with the victim in a
critical section) is always proportional to the number of critical
section executed: roughly one in five million critical section is
preempted.  That's even less than the one in two million we'd expect
from the ~six cycle per critical section: that kind of makes sense
with out of order execution, given that the critical section should
easily flow through the pipeline and slip past timer interrupts.

Trade-offs
----------

The main trade-off is that rlocks do not attempt to handle thread
migrations: when a thread migrates to another CPU, we let it assume
(temporary) exclusive ownership of its pseudo-per-CPU struct instead
of issuing IPIs.  That's good for simplicity, and also -- arguably --
for scaling.  The scaling argument is weak, given how efficient IPIs
seem to be.  However, IPIs feel like one of these operations for which
most of the cost is indirect and hard to measure.  The overhead isn't
only (or even mostly) incurred by the thread that triggers the IPIs:
each CPU must stop what it's currently doing, flush the pipeline,
switch to the kernel to handle the interrupt, and resume execution.  A
scheme that relies on IPIs to handle events like thread migrations
(rare, but happens at a non-negligible base rate) will scale badly to
really large CPU counts, and, more importantly, may make it hard to
identify when the IPIs hurt overall system performance.

The other important design decision is that rlocks uses signals
instead of cross-modifying code.  I'm not opposed to cross-modifying
code, but I cringe at the idea of leaving writable and executable
pages lying around just for performance.  Again, we could `mprotect`
around cross-modification, but `mprotect` triggers IPIs, and that's
exactly what we're trying to avoid.  Also, if we're going to
`mprotect` in the common case, we might as well just `mmap` in
different machine code; that's likely a bit faster than two `mprotect`
and definitely safer (I would use this `mmap` approach for revocable
multi-CPU locks à la Harris and Fraser).

The downside of using signals is that they're more invasive than
cross-modifying code.  If user code expects any (async) signal, its
handlers must either mask the rlock signal away and not use rlocks, or
call the rlock signal handler… not transparent, but not exacting
either.

Rlocks really aren't that much code (560 LOC), and that code is fairly
reasonable (no mprotect or self-modification trick, just signals).
After more testing and validation, I would consider merging them in
[Concurrency Kit](http://concurrencykit.org/) for production use.

Next step: either `mmap`-based strict revocable locks for non-blocking
concurrent code, or a full implementation of pseudo-per-CPU data based
on relaxed rlocks.
