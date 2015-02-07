---
layout: post
title: "Lock-free sequence locks"
date: 2015-01-13 2:30
comments: true
categories: 
---

Specialised locking schemes and lock-free data structures are a big
part of my work these days.  I think the main reason the situation is
tenable is that, very early on, smart people decided to focus on an
SPMC architecture: single writer (producer), multiple readers
(consumers).

As programmers, we have a tendency to try and maximise generality: if
we can support multiple writers, why would one bother with measly SPMC
systems?  The thing is SPMC is harder than SPSC, and MPMC is
even more complex.  Usually, more concurrency means programs are harder to
get right, harder to scale and harder to maintain.  Worse: it also
makes it more difficult to provide theoretical progress guarantees.

Apart from architecting around simple cases, there's a few ways to
deal with this reality.  We can define new, weaker, classes of program,
like obstruction-freedom: a system is obstruction-free when one thread
is guaranteed to make progress if *every other thread is suspended*.  We
can also [weaken the guarantees of our data structure](http://www.cosy.sbg.ac.at/research/tr/2010-07_Kirsch_Payer_Roeck.pdf).  For example,
rather than exposing a single FIFO, we could distribute load and
contention across multiple queues; we lose strict FIFO order, but we
also eliminate a system bottleneck.  Another option is to try and
identify how real computers are more powerful than our abstract
models: some argue that, [realistically, many lock-free schemes are wait-free](http://arxiv.org/abs/1311.3200), and others [exploit the fact that x86-TSO machines have finite store buffers](http://www.cs.technion.ac.il/~mad/publications/asplos2014-ffwsq.pdf).

Last week, I got lost doodling with x86-specific cross-modifying code,
but still stumbled on a cute example of a simple lock-free protocol:
lock-free sequence locks.  This sounds like an oxymoron, but I promise
it makes sense.

Lock-free sequence locks
========================

It helps to define the terms
better. [Lock-freedom](http://en.wikipedia.org/wiki/Non-blocking_algorithm#Lock-freedom)
means that the overall system will always make progress, even if some
(but not all) threads are suspended.
[Classical sequence](http://en.wikipedia.org/wiki/Seqlock) locks are
an optimistic form of write-biased reader/writer locks: concurrent
writes are forbidden (e.g., with a spinlock), read transactions abort
whenever they observe that writes are in progress, and a generation
counter avoids
[ABA problems](http://en.wikipedia.org/wiki/ABA_problem) (when a read
transaction would observe that no write is in progress before and after a
quick write).

In
[Transactional Mutex Locks (PDF)](http://www.cs.rochester.edu/u/scott/papers/2010_EuroPar_TML.pdf),
sequence locks proved to have enviable performance on small systems and
scaled decently well for read-heavy workloads.  They even allowed lazy
upgrades from reader to writer by atomically checking that the
generation has the expected value when acquiring the sequence lock for
writes.  However, we lose nearly all progress guarantees: one
suspended writer can freeze the whole system.

The central trick of lock-freedom is cooperation: it doesn't matter if
a thread is suspended in the middle of a critical section, as long as
any other thread that would block can instead complete the work that
remains.  In general, this is pretty hard, but we can come up with
restricted use cases that are idempotent.  For lock-free sequence
locks, the critical section is a precomputed set of writes: a series
of assignments that must appear to execute atomically.  It's fine if
writes happen multiple times, as long as they stop before we move on
to another set of writes.

There's a primitive based on compare-and-swap that can easily achieve
such conditional writes: restricted double compare and single swap
(RDCSS, introduced in
[A Practical Multi-Word Compare-and-Swap (PDF)](http://www.cl.cam.ac.uk/research/srg/netos/papers/2002-casn.pdf)).
RDCSS atomically checks if both a control word (e.g., a generation
counter) and a data word (a mutable cell) have the expected values and,
if so, writes a new value in the data word.  The pseudocode for
regular writes looks like

    if (CAS(self.data, self.old, self) == fail) {
        return fail;
    }
   
    if (*self.control != self.expected) {
        CAS(self.data, self, self.old);
        return fail;
    }

    CAS(self.data, self, self.new);
    return success;

The trick is that, if the first CAS succeeds, we always know how to
undo it (`data`'s old value must be `self.old`), and that
information is stored in `self` so any thread that observes the first
CAS has enough information to complete or rollback the RDCSS.  The
only annoying part is that we need a two-phase commit: reserve `data`,
confirm that `control` is as `expected`, and only then write to `data`.

For the cost of two compare-and-swap per write -- plus one to acquire the
sequence lock -- writers don't lock out other writers (writers help
each other make progress instead).  Threads (especially readers) can
still suffer from starvation, but at least the set of writes can be
published ahead of time, so readers can even lookup in that set rather
than waiting for/helping writes to complete.  The generation counter
remains a bottleneck, but, as long as writes are short and happen
rarely, that seems like an acceptable trade to avoid the 3n CAS in
multi-word compare and swap.

Real code
=========

Here's what the scheme looks like in SBCL.

First, a mutable box because we don't have raw pointers (I could also
have tried to revive my sb-locative hack) in CL.

{% codeblock %}
(defstruct (box
            (:constructor make-box (%value)))
  %value)
{% endcodeblock %}

Next, the type for write records: we have the the value for the next
generation (once the write is complete) and a hash table of box to
pairs of old and new values.  There's a key difference with the way
RDCSS is used to implement multiple compare and swap: we don't check
for mismatches in the old value and simply assume that it is correct.
{% codeblock %}
(defstruct (record
             (:constructor %make-record (generation ops)))
  (generation (error "Missing arg") :type fixnum :read-only t)
  ;; map of box -> (cons old new).  I use a hash table for
  ;; convenience but I doubt it's the right choice.
  (ops (error "Missing arg") :type hash-table :read-only t))
(declaim (freeze-type record))
{% endcodeblock %}

The central bottleneck is the sequence lock, which each (read)
transaction must snapshot before attempting to read consistent
values.
{% codeblock %}
(declaim (type (or (and unsigned-byte fixnum) record) **current-record**))
(defglobal **current-record** 0)

(defvar *initial-record*)

(defun snapshot-generation ()
  (let ((initial *initial-record*))
    (if (record-p initial)
        (record-generation initial)
        initial)))
{% endcodeblock %}

The generation associated with a snapshot is the snapshot if it is a
positive fixnum, otherwise it is the write record's generation.

Before using any read, we make sure that the generation counter hasn't
changed.

{% codeblock %}
(defun check ()
  #-(or x86 x86-64) (sb-thread:barrier (:read)) ; x86 don't reorder reads
  (let ((initial *initial-record*)
        (current **current-record**))
    (unless (or (eql initial current)
                (and (record-p initial)
                     (eql (record-generation initial) current)))
      (throw 'fail t))))
{% endcodeblock %}

I see two ways to deal with starting a read transaction while a write
is in progress: we can help the write complete, or we can overlay the
write on top of the current heap in software.  I chose the latter:
reads can already be started by writers.  If a write is in progress
when we start a transaction, we stash the write set in `*current-map*`
and lookup there first:

{% codeblock %}
(defvar *current-map* nil)

(defun box-value (box)
  (prog1 (let* ((map *current-map*)
                (value (if map
                           (cdr (gethash box map (box-%value box)))
                           (box-%value box))))
           (if (record-p value)
               ;; if we observe a record, either a new write is in
               ;; progress and (check) is about to fail, or this is
               ;; for an old (already completed) write that succeeded
               ;; partially by accident.  In the second case, we want
               ;; the *old* value.
               (car (gethash box (record-ops value)))
               value))
    (check)))
{% endcodeblock %}

We're now ready to start read transactions.  We take a snapshot of the
generation counter, update `*current-map*`, and try to execute a
function that uses `box-value`.  Again, we don't need a read-read
barrier on x86oids (nor on SPARC, but SBCL doesn't have threads on
that platform).

{% codeblock %}
(defun call-with-transaction (function &rest arguments)
  (catch 'fail
    (let* ((*initial-record* **current-record**)
           (*current-map* (and (record-p *initial-record*)
                               (record-ops *initial-record*))))
      #-(or x86 x86-64) (sb-thread:barrier (:read))
      (return-from call-with-transaction
        (values (apply function arguments) t))))
  (values nil nil))

(defmacro with-transaction ((&rest bindings) &body body)
  `(call-with-transaction (lambda ,(mapcar #'first bindings)
                            ,@body)
                          ,@(mapcar #'second bindings)))
{% endcodeblock %}

The next function is the keystone: helping a write record go through
exactly once.

{% codeblock %}
(defun help (record)
  (flet ((write-one (box old new)
           ;; if record isn't the current generation anymore,
           ;; it has already been completed
           (unless (eq **current-record** record)
               (return-from help nil))
             (let ((actual (sb-ext:cas (box-%value box) old record)))
               (when (eql actual new) ;; already done? next!
                 (return-from write-one))
               
               ;; definite failure -> no write went though; leave.
               (unless (or (eql actual old)
                           (eql actual record))
                 (return-from help nil))

               ;; check for activity before the final write
               (unless (eq **current-record** record)
                 (sb-ext:cas (box-%value box) record old)
                 (return-from help nil))

               ;; Really perform write (this can only fail if
               ;; another thread already succeeded).
               (sb-ext:cas (box-%value box) record new))))
    (maphash (lambda (box op)
               (write-one box (car op) (cdr op)))
             (record-ops record)))
  ;; Success! move the generation counter forward.
  (eql record (sb-ext:cas (symbol-value '**current-record**)
                          record
                          (record-generation record))))
{% endcodeblock %}

Now we can commit with a small wrapper around `help`. Transactional
mutex lock has the idea of transaction that are directly created as
write transactions.  We assume that we always know how to undo writes,
so transactions can only be upgraded from reader to writer.
Committing a write will thus check that the generation counter is
still consistent with the (read) transaction before publishing the new
write set and helping it forward.

{% codeblock %}
(defun commit (record)
  (check-type record record)
  (let ((initial
          (loop
           (let ((value **current-record**))
             (check)
             (if (record-p value)
                 (help value)
                 (return value))))))
    (unless (and (eql (sb-ext:cas (symbol-value '**current-record**)
                                  initial record)
                      initial)
                 (help record))
      (throw 'fail t))
    t))
{% endcodeblock %}

And now some syntactic sugar to schedule writes
{% codeblock %}
(defvar *write-record*)

(defun call-with-write-record (function)
  (let ((*write-record* (%make-record (mod (1+ (snapshot-generation))
                                           (1+ most-positive-fixnum))
                                      (make-hash-table))))
    (multiple-value-prog1 (funcall function)
      (commit *write-record*))))

(defun (setf box-value) (value box)
  (setf (gethash box (record-ops *write-record*))
        (cons (box-value box) value))
  value)

(defmacro with-write (() &body body)
  `(call-with-write-record (lambda ()
                             ,@body)))
{% endcodeblock %}

That's enough for a smoke test on my dual core laptop.

{% codeblock %}
(defvar *a* (make-box 0))
(defvar *b* (make-box 0))
(defvar *semaphore* (sb-thread:make-semaphore))

(defun test-reads (n)
  (let ((a *a*)
        (b *b*))
    (sb-thread:wait-on-semaphore *semaphore*)
    (loop repeat n
          count (with-transaction ()
                  (assert (eql (box-value a) (box-value b)))
                  t))))

(defun test-writes (n)
  (let ((a *a*)
        (b *b*))
    (sb-thread:wait-on-semaphore *semaphore*)
    (loop repeat n
          count (with-transaction ()
                  (with-write ()
                    (incf (box-value a))
                    (incf (box-value b)))
                  t))))
{% endcodeblock %}

The function `test-reads` counts the number of successful read
transactions and checks that `(box-value a)` and `(box-value b)` are
always equal. That consistency is preserved by `test-writes`, which
counts the number of times it succeeds in incrementing both
`(box-value a)` and `(box-value b)`.

The baseline case should probably be serial execution, while the ideal
case for transactional mutex lock is when there is at most one
writer.  Hopefully, lock-free sequence locks also does well when there
are multiple writers.

{% codeblock %}
(defun test-serial (n)
  (setf *a* (make-box 0)
        *b* (make-box 0)
        *semaphore* (sb-thread:make-semaphore :count 4))
  (list (test-reads (* 10 n))
        (test-reads (* 10 n))
        (test-writes n)
        (test-writes n)))

(defun test-single-writer (n)
  (setf *a* (make-box 0)
        *b* (make-box 0)
        *semaphore* (sb-thread:make-semaphore))
  (let ((threads
          (list (sb-thread:make-thread #'test-reads :arguments (* 10 n))
                (sb-thread:make-thread #'test-reads :arguments (* 10 n))
                (sb-thread:make-thread #'test-writes
                                       :arguments (ceiling (* 1.45 n))))))
    (sb-thread:signal-semaphore *semaphore* 3)
    (mapcar (lambda (x)
              (ignore-errors (sb-thread:join-thread x)))
            threads)))

(defun test-multiple-writers (n)
  (setf *a* (make-box 0)
        *b* (make-box 0)
        *semaphore* (sb-thread:make-semaphore))
  (let ((threads
          (list (sb-thread:make-thread #'test-reads :arguments (* 10 n))
                (sb-thread:make-thread #'test-reads :arguments (* 10 n))
                (sb-thread:make-thread #'test-writes :arguments n)
                (sb-thread:make-thread #'test-writes :arguments n))))
    (sb-thread:signal-semaphore *semaphore* 4)
    (mapcar (lambda (x)
              (ignore-errors (sb-thread:join-thread x)))
            threads)))
{% endcodeblock %}

Let's try this!
===============

First, the serial case. As expected, all the transactions succeed, in
6.929 seconds total (6.628 without GC time).  With one writer and two
readers, all the writes succeed (as expected), and 98.5% of reads do as
well; all that in 4.186 non-GC seconds, a 65% speed up.  Finally, with
two writers and two readers, 76% of writes and 98.5% of reads complete in
4.481 non-GC seconds.  That 7% slowdown compared to the single-writer
case is pretty good: my laptop only has two cores, so I would expect
more aborts on reads and a lot more contention with, e.g., a spinlock.

    CL-USER> (gc :full t) (time (test-serial 1000000))
    Evaluation took:
      6.929 seconds of real time
      6.944531 seconds of total run time (6.750770 user, 0.193761 system)
      [ Run times consist of 0.301 seconds GC time, and 6.644 seconds non-GC time. ]
      100.23% CPU
      11,063,956,432 processor cycles
      3,104,014,784 bytes consed
      
    (10000000 10000000 1000000 1000000)
    CL-USER> (gc :full t) (time (test-single-writer 1000000))
    Evaluation took:
      4.429 seconds of real time
      6.465016 seconds of total run time (5.873936 user, 0.591080 system)
      [ Run times consist of 0.243 seconds GC time, and 6.223 seconds non-GC time. ]
      145.97% CPU
      6,938,703,856 processor cycles
      2,426,404,384 bytes consed
      
    (9863611 9867095 1450000)
    CL-USER> (gc :full t) (time (test-multiple-writers 1000000))
    Evaluation took:
      4.782 seconds of real time
      8.573603 seconds of total run time (7.644405 user, 0.929198 system)
      [ Run times consist of 0.301 seconds GC time, and 8.273 seconds non-GC time. ]
      179.30% CPU
      7,349,757,592 processor cycles
      3,094,950,400 bytes consed
      
    (9850173 9853102 737722 730614)
    
How does a straight mutex do with four threads?
{% codeblock %}
(defun test-mutex (n)
  (let ((mutex (sb-thread:make-mutex))
        (semaphore (sb-thread:make-semaphore))
        (a 0)
        (b 0))
    (flet ((reader (n)
             (sb-thread:wait-on-semaphore semaphore)
             (loop repeat n do
               (sb-thread:with-mutex (mutex)
                 (assert (eql a b)))))
           (writer (n)
             (sb-thread:wait-on-semaphore semaphore)
             (loop repeat n do
               (sb-thread:with-mutex (mutex)
                 (incf a)
                 (incf b)))))
      (let ((threads
              (list (sb-thread:make-thread #'reader
                                           :arguments (* 10 n))
                    (sb-thread:make-thread #'reader
                                           :arguments (* 10 n))
                    (sb-thread:make-thread #'writer
                                           :arguments (ceiling (* .75 n)))
                    (sb-thread:make-thread #'writer
                                           :arguments (ceiling (* .75 n))))))
        (sb-thread:signal-semaphore semaphore 4)
        (mapc #'sb-thread:join-thread threads)))))
{% endcodeblock %}

    CL-USER> (gc :full t) (time (test-mutex 1000000))
    Evaluation took:
      5.814 seconds of real time
      11.226734 seconds of total run time (11.169670 user, 0.057064 system)
      193.10% CPU
      9,248,370,000 processor cycles
      1,216 bytes consed
      
    (#<SB-THREAD:THREAD FINISHED values: NIL {1003A6E1F3}>
     #<SB-THREAD:THREAD FINISHED values: NIL {1003A6E383}>
     #<SB-THREAD:THREAD FINISHED values: NIL {1003A6E513}>
     #<SB-THREAD:THREAD FINISHED values: NIL {1003A6E6A3}>)

There's almost no allocation (there's no write record), but the lack
of read parallelism makes locks about 20% slower than the lock-free
sequence lock.  A reader-writer lock would probably close that gap.
The difference is that the lock-free sequence lock has stronger
guarantees in the worst case: no unlucky preemption (or crash, with
shared memory IPC) can cause the whole system to stutter or even halt.

The results above correspond to my general experience.  Lock-free
algorithms aren't always (or even regularly) more efficient than well
thought out locking schemes; however, they are more robust and easier
to reason about.  When throughput is more than adequate, it makes
sense to eliminate locks, not to improve the best or even the average
case, but rather to eliminate a class of worst cases -- including
deadlocks.

P.S., here's a sketch of the horrible cross-modifying code hack.  It
turns out that the instruction cache is fully coherent on (post-586)
x86oids; the prefetch queue will even reset itself based on the linear
(virtual) address of writes.  With a single atomic byte write, we can
turn a `xchg (%rax), %rcx` into `xchg (%rbx), %rcx`, where `%rbx`
points to a location that's safe to mutate arbitrarily.  That's an
atomic store predicated on the value of a control word elsewhere
(hidden in the instruction stream itself, in this case).  We can then
dedicate one sequence of machine to each transaction and reuse them
via some
[Safe Memory Reclamation mechanism (PDF)](http://www.cs.toronto.edu/~tomhart/papers/tomhart_thesis.pdf).

There's one issue: even without preemption (if a writer is pre-empted,
it should see the modified instruction upon rescheduling), stores
can take pretty long to execute: in the worst case, the CPU has to
translate to a physical address and wait for the bus lock.  I'm pretty
sure there's a bound on how long a `xchg m, r64` can take, but I
couldn't find any documentation on hard figure.  If we knew that `xchg
m, r64` never lasts more than, e.g., 10k cycles, a program could wait
that many cycles before enqueueing a new write.  That wait is bounded
and, as long as writes are disabled very rarely, should improve
the worst-case behaviour without affecting the average throughput.
