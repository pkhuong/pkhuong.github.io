---
layout: post
title: "Tabasco Sort: a super-optimal merge sort"
date: 2012-08-27 10:30
comments: true
categories: 
---

EDIT: 2012-08-29: I added a section to compare comparison counts with
known bounds for general comparison sorts and sorting networks.

In an
[earlier post](http://pvk.ca/Blog/2012/08/13/engineering-a-list-merge-sort/),
I noted how tedious coding unrolled sorts can be.  Frankly, that's the
main reason I stopped at leaf sorts of size three.  Recently,
[Neil Toronto](http://blog.racket-lang.org/2012/08/fully-inlined-merge-sort.html)
wrote a nice post on the generation of size-specialised merge sorts.
The post made me think about that issue a bit more, and I now have a
neat way to generate unrolled/inlined merge sorts that are
significantly smaller than the comparison and size "-optimal" inlined
merge sorts.

The code is up as a
[single-file library](http://discontinuity.info/~pkhuong/tabasco-sort.lisp),
and sorts short vectors faster than SBCL's inlined heapsort by a
factor of two to three… and compiles to less machine code.  The
generator is a bit less than 100 LOC, so I'm not sure I want to
include it in the mainline yet.  If someone wants to add support for
more implementations, I'd be happy to extend Tabasco sort, and might
even consider letting it span multiple files ;)

Differently-optimal sorts
-------------------------

The inlined merge sort for three values (`a`, `b`, and `c`) is copied
below.  It has to detect between \\(3! = 6\\) permutation, and does so
with an optimal binary search tree.  That scheme leads to code with
\\(n! - 1\\) comparisons to sort n values, and for which each
execution only goes through two or three comparisons (\\(\approx \lg n!\\)).

{% codeblock "optimal" inlined merge sort (n = 3) %}
(if (< b c)
    (if (< a b)
        (values a b c)
        (if (< a c)
            (values b a c)
            (values b c a)))
    (if (< a c)
        (values a c b)
        (if (< a b)
            (values c a b)
            (values c b a))))
{% endcodeblock %}

An optimal sorting network for three values needs only three
comparisons, and always executes those three comparisons.

{% codeblock optimal sorting network (n = 3) %}
(progn
  (when (< c b)
    (rotatef b c))
  (when (< b a)
    (rotatef a b))
  (when (< c b)
    (rotatef b c)))
{% endcodeblock %}

Finally, the leaf sort I used in SBCL is smaller than the inlined
merge sort (three comparisons), but sometimes executes fewer than
three comparisons.  It's superoptimal ;)

{% codeblock "super-optimal" inlined merge sort (n = 3) %}
(progn
  (when (< c b)
    (rotatef b c))
  (if (< b a)
      (if (< c a)
          (values b c a)
          (values b a c))
      (values a b c)))
{% endcodeblock %}

The optimal merge sort is larger than the optimal sorting network, and
the optimal sorting network performs potentially more comparisons than
the optimal merge sort…

Each implementation is optimal for different search spaces: the
optimal merge sort never merges continuations, and the sorting
network's only control dependencies are in the conditional swaps.

The "super-optimal" merge sort does better by allowing itself both
assignments (or tail-calls) and non-trivial control flow: it's smaller
than the inlined merge sort (but performs more data movement), and
potentially executes fewer comparisons than the sorting network (with
a larger total code size).  And, essential attribute in practice, it's
easy to generate.  This contrasts with optimal sorting networks, for
which we do not have any generation method short of brute force
search; in fact, in practice, sorting networks tend to exploit
suboptimal (by a factor of \\(\log n\\)) schemes like
[bitonic sort](http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm)
or
[odd-even merge sort](http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/networks/oemen.htm).
Then again, we're only concerned with tiny sorts, and asymptotics can
be misleading: Batcher's odd-even merge sort happens to be optimal for
\\(n\leq 8\\).  The issue with sorting networks remains:
data-oblivious control flow pessimises their comparison count.

Generalising from size 3
------------------------

What the last merge sort does is to first sort both halves of the
values (`a` is trivially sorted, and `b c` needs one conditional
swap), and then, assuming that each half (`a` and `b c`) is sorted,
find the right permutation with which to merge them.  Rather than
\\(n!\\) permutations, a merge only needs to distinguish between
\\(C(n, \lfloor n/2\rfloor) = \frac{n!}{\lfloor n/2\rfloor!\lceil n/2\rceil!}\\)
permutations, and the recursive sorts are negligible compared to the
merge step.  That's a huge reduction in code size!

A simple merge generator fits in half a
[screenful](http://wry.me/~darius/hacks/screenfuls/screen3.html).

{% codeblock unrolled merge generator %}
(defun emit-permute (destinations sources)
  ;; (setf values) is parallel assignment
  `(setf (values ,@destinations) (values ,@sources)))

(defun emit-merge-1 (destinations left right acc)
  "Build a search tree to determine the right permutation to
   merge LEFT and RIGHT, given that each is pre-sorted."
  (cond ((null left)
         (emit-permute destinations (append (reverse acc) right)))
        ((null right)
         (emit-permute destinations (append (reverse acc) left)))
        (t
         `(if (< ,(first right) ,(first left)) ; stable sort
              ,(emit-merge-1 destinations
                             left (rest right)
                             (cons (first right) acc))
              ,(emit-merge-1 destinations
                             (rest left) right
                             (cons (first left) acc))))))

(defun emit-merge (left right)
  (emit-merge-1 (append left right) left right nil))
{% endcodeblock %}

Given two lists of sorted variables, `emit-merge` calls `emit-merge-1`
to generate code that finds the right permutation, and executes it at
the leaf.  A binary search tree is generated by keeping track of the
merged list in a reverse-order (to enable tail-sharing) accumulator of
variable names.  As expected, when merging a list of length one with
another of length two, we get pretty much the code I wrote by hand
earlier.

    CL-USER> (emit-merge '(a) '(b c))
    (if (< b a)
        (if (< c a)
            (setf (values a b c) (values b c a))
            (setf (values a b c) (values b a c)))
        (setf (values a b c) (values a b c)))

There's one striking weakness: we generate useless code for the
identity permutation.  We could detect that case, or, more generally,
we could find the cycle decomposition of each permutation and use it
to minimise temporary values; that'd implicitly take care of cases
like `(setf (values a b c) (values b a c))`, in which some values are
left unaffected.

A smarter permutation generator
-------------------------------

I'll represent permutations as associative lists, from source to
destination.  Finding a cycle is easy: just walk the permutation from
an arbitrary value until we loop back.

{% codeblock extract a single cycle from a linearly-represented permutation %}
(defun find-cycle (mapping)
  "Extract an arbitrary cycle from a non-empty mapping,
   returning both the cycle and the rest of the mapping."
  (assert mapping)
  (let* ((head  (pop mapping))
         (cycle (list (cdr head))))
    (loop
     (let* ((next-source (first cycle))
            (pair        (assoc next-source mapping)))
       (cond (pair
              (push (cdr pair) cycle)
              ;; if this sucks enough to matter, the output
              ;; will be humongous anyway
              (setf mapping (remove pair mapping)))
             (t
              (assert (eql next-source (first head)))
              (return (values cycle mapping))))))))
{% endcodeblock %}

To generate the code corresponding to a permutation, I can extract all
the cycles, execute each cycle with a `rotatef`.

{% codeblock cycle-decomposition-based permute generator %}
(defun emit-permute (destinations sources)
  "Emit a [destinations <- sources] permutation via its
   cycle decomposition"
  ;; source -> destination alist, minus trivial pairs
  (let ((mapping (remove-if (lambda (pair)
                              (eql (car pair) (cdr pair)))
                            (pairlis sources destinations))))
    `(progn
       ,@(loop while mapping
               collect
               (multiple-value-bind (cycle new-mapping)
                   (find-cycle mapping)
                 (setf mapping new-mapping)
                 `(rotatef ,@cycle))))))
{% endcodeblock %}

The merge step for a sort of size three is now a bit more explicit,
but likely compiles to code that uses fewer registers as well. It
probably doesn't matter on good SSA-based backends, but those are the
exception rather than the norm in the Lisp world.

    CL-USER> (emit-merge '(a) '(b c))
    (if (< b a)
        (if (< c a)
            (progn (rotatef a b c))
            (progn (rotatef a b)))
        (progn))

Adding the recursive steps
--------------------------

The only thing missing for a merge sort is to add base cases and
recursion.  The base case is easy: lists of length one are sorted.
Inlining recursion is trivial, as is usually the case when generating
Lisp code.

{% codeblock "super-optimal" inlined merge sort %}
(defun emit-sort-1 (values length)
  (when (> length 1)
    (let* ((split (truncate length 2))
           (left  (subseq values 0 split))
           (right (subseq values split)))
      `(progn
         ,(emit-sort-1 left  split)
         ,(emit-sort-1 right (- length split))
         ,(emit-merge left right)))))

(defun emit-sort (values)
  (emit-sort-1 values (length values)))

(defmacro inline-sort (&rest values)
  (let* ((pairs (loop for value in values
                      collect `(,(gensym "TEMP") ,value)))
         (temps (mapcar #'first pairs)))
    `(let ,pairs
       ,(emit-sort temps)
       (values ,@temps))))
{% endcodeblock %}

The resulting three-value sorter looks good; there are some
redundancies with nested or empty `progn`s, but any half-decent
compiler will take care of that.  Python certainly does a goob job on
that code.

    CL-USER> (emit-sort '(a b c))
    (progn
     nil
     (progn
      nil
      nil
      (if (< c b)
          (progn (rotatef b c))
          (progn)))
     (if (< b a)
         (if (< c a)
             (progn (rotatef a b c))
             (progn (rotatef a b)))
         (progn)))
    CL-USER> (disassemble (lambda (a b c)
                            (declare (type fixnum a b c))
                            (inline-sort a b c)))
    ; disassembly for (lambda (a b c))
    ; 0E88F150:       498BD0           mov rdx, r8                ; no-arg-parsing entry point
    ;       53:       498BC9           mov rcx, r9
    ;       56:       498BDA           mov rbx, r10
    ;       59:       4D39CA           cmp r10, r9
    ;       5C:       7C30             jl L3
    ;       5E: L0:   4C39C1           cmp rcx, r8
    ;       61:       7D0B             jnl L1
    ;       63:       4C39C3           cmp rbx, r8
    ;       66:       7C1B             jl L2
    ;       68:       488BD1           mov rdx, rcx
    ;       6B:       498BC8           mov rcx, r8
    ;       6E: L1:   488BF9           mov rdi, rcx
    ;       71:       488BF3           mov rsi, rbx
    ;       74:       488D5D10         lea rbx, [rbp+16]
    ;       78:       B906000000       mov ecx, 6
    ;       7D:       F9               stc
    ;       7E:       488BE5           mov rsp, rbp
    ;       81:       5D               pop rbp
    ;       82:       C3               ret
    ;       83: L2:   488BD1           mov rdx, rcx
    ;       86:       488BCB           mov rcx, rbx
    ;       89:       498BD8           mov rbx, r8
    ;       8C:       EBE0             jmp L1
    ;       8E: L3:   498BCA           mov rcx, r10
    ;       91:       498BD9           mov rbx, r9
    ;       94:       EBC8             jmp L0

I thought about generating calls to `values` in the final merge,
rather than permuting, but decided against: I know SBCL doesn't
generate clever code when permuting registers, and that'd result in
avoidable spills.  I also considered generating code in CPS rather
than emitting assignments; again, I decided against because I can't
depend on SBCL to emit clever permutation code.  The transformation
would make sense in a dialect with weaker support (both compiler and
social) for assignment.

How good is the generated code?
-------------------------------

Both this inline merge sort and the original, permutation-free (except
at the leaves), one actually define the exact same algorithms.  For
any input, both (should) execute the same comparisons in the same
order: the original inline merge sort simply inlines the whole set of
execution traces, without even merging control flow.

The permutation-free sort guarantees that it never performs redundant
comparisons.  Whether it performs the strict minimum number of
comparisons, either on average or in the worst case, is another question.
At first, I thought that \\(\lg n!\\) should be a good estimate, since
the search tree seems optimally balanced.  The problem is \\(n!\\)
tends to have many other prime factors than 2, and we can thus expect
multiple comparisons to extract less than 1 bit of information, for
each execution.  The lower bound can thus be fairly far from the
actual value… Still, this question is only practically relevant for
tiny sorts, so the discrepancy shouldn't be too large.

A simple way to get the minimum, average or maximum comparison count
would be to annotate the permutation-free generator to compute the
shortest, average and longest path as it generates the search tree.

I'll instead mimic the current generator.

The first step is to find the number of comparisons to perform a
merge of `m` and `n` values.

If either `m` or `n` is zero, merging the sequences is trivial.

Otherwise, the minimum number of comparisons is `(min m n)`: the
sequences are pre-sorted, and the shortest sequence comes first.  The
maximum is `(1- (+ m n))`.  I couldn't find a simple expression for
the average over all permutations.  Instead, I iterate over all
possible combinations and count the number of comparisons until either
subsequence is exhausted.

{% codeblock compute the number of comparisons during merges %}
(defun merge-count (m n)
  ;; return min, expected, max comparison to merge
  ;; presorted subsequences of m and n values
  (if (zerop (min m n))
      (values 0 0 0)
      (let ((comparisons 0)
            (count 0)
            (min   most-positive-fixnum)
            (max   0))
        (dotimes (i (ash 1 (+ m n))
                    (values min
                            (/ comparisons
                               count)
                            max))
          ;; only consider combinations with m ones
          ;; (and n zeros)
          (when (= (logcount i) m)
            (let ((cmp (1- (+ m n)))
                  (mask i))
              ;; no more comparison needed until two consecutive
              ;; elements from distinct subsequences
              (loop while (eql (logbitp 0 mask)
                               (logbitp 1 mask))
                    do (decf cmp)
                       (setf mask (ash mask -1)))
              (setf min (min min cmp)
                    max (max max cmp))
              (incf comparisons cmp)
              (incf count)))))))
{% endcodeblock %}

Counting the number of comparisons in sorts is then trivial, with a
recursive function.  I didn't even try to memoise repeated
computations: the generated code is ludicrously long when sorting as
few as 13 or 14 values.

{% codeblock compute the number of comparisons in merge sort %}
(defun sort-count (n)
  (if (<= n 1)       ; trivially sorted
      (values 0 0 0)
      (let ((min 0)
            (avg 0)
            (max 0))
        (flet ((inc (min- avg- max-)
                 (incf min min-)
                 (incf avg avg-)
                 (incf max max-)))
          ;; accumulate min/avg/max count from sorting the left and
          ;; right subsequences, and merging
          (multiple-value-call #'inc (sort-count (floor n 2)))
          (multiple-value-call #'inc (sort-count (ceiling n 2)))
          (multiple-value-call #'inc (merge-count (floor n 2)
                                                  (ceiling n 2))))
        (values min avg max))))
{% endcodeblock %}

    CL-USER> (loop for i from 2 upto 16
                   do (multiple-value-bind (min avg max)
                          (sort-count i)
                        (format t "~4D ~6,2F ~6D ~6,2F ~6D~%"
                                i (log (! i) 2d0)
                                min (float avg) max)))
    ;; n  lg(n!)   min   avg     max ;   best   network
       2   1.00      1   1.00      1 ;   1      1
       3   2.58      2   2.67      3 ;   3      3
       4   4.58      4   4.67      5 ;   5      5
       5   6.91      5   7.17      8 ;   7      9
       6   9.49      7   9.83     11 ;   10     12
       7  12.30      9  12.73     14 ;   13     16
       8  15.30     12  15.73     17 ;   16     19
       9  18.47     13  19.17     21 ;   19     25?
      10  21.79     15  22.67     25 ;   22     29?
      11  25.25     17  26.29     29 ;   26     35?
      12  28.84     20  29.95     33 ;   30     39?
      13  32.54     22  33.82     37 ;   34     45?
      14  36.34     25  37.72     41 ;   38     51?
      15  40.25     28  41.69     45 ;   42     56?
      16  44.25     32  45.69     49 ;   46?    60?

I annotated the output with comments (marked with semicolons).  The
columns are, from left to right, the sort size, the theoretical lower
bound (on the average or maximum number of comparisons), the minimum
number of comparisons (depending on the input permutation), the
average (over all input permutations), and the maximum count.  I added
two columns by hand: the optimal worst-case (maximum) comparison
counts (over all sorting methods, copied from the
[OEIS](http://oeis.org/A036604)), and the optimal size for sorting
networks, when known (lifted from a table
[here [pdf]](http://nn.cs.utexas.edu/downloads/papers/valsalam.utcstr11.pdf)).
Inexact (potentially higher than the optimum) bounds are marked with a
question mark.

For the input size the inline merge sort can reasonably tackle (up to
ten or so), its worst-case is reasonably close to the best possible,
and its average case tends to fall between the lower bound and the
best possible.  Over all these sizes, the merge sort's worst case
performs fewer comparisons than the optimal or best-known sorting
networks.  The current best upper bounds on the minimal worst-case
comparison count seem to be based on insertion sort passes that
minimise the number of comparisons with a binary search.  I don't
believe that's directly useful for the current use case, but a similar
trick might be useful to reduce the number of comparisons, at the
expense of reasonably more data movement.

Making it CL-strength
---------------------

That's already a decent proof of concept.  It's also far too plain to
fit in the industrial-strength Common Lisp way.  An inline sorting
macro worthy of CL should be parameterised on both comparator and key
functions, and work with arbitrary places rather than only variables.

Parameterising the comparator is trivial.  The key could be handled by
calling it at each comparison, but that's wasteful.  We're generating
code; might as well go for glory.  Just like in the list merge sort,
I'll cache calls to the key function in the merge tree.  I'll also use
special variables instead of passing a half-dozen parameters around in
the generator.

{% codeblock key/comparator-aware merge generator %}
(defvar *inline-sort-comparator*)
(defvar *inline-sort-key*)
(defvar *inline-sort-destinations*)
(defvar *inline-sort-left-head*)
(defvar *inline-sort-right-head*)

(defun emit-merge-1 (left right acc)
  "Build a search tree to determine the right permutation to
   merge LEFT and RIGHT, given that each is pre-sorted."
  ;; stability trickery
  `(if (funcall ,*inline-sort-comparator* ,*inline-sort-right-head*
                                          ,*inline-sort-left-head*)
       ,(let* ((acc        (cons (first right) acc))
               (right      (rest right)))
          ;; pop from RIGHT, and recurse if RIGHT isn't empty.
          (if right
              `(let ((,*inline-sort-right-head*
                       (funcall ,*inline-sort-key* ,(first right))))
                 ,(emit-merge-1 left right acc))
              (emit-permute *inline-sort-destinations*
                            (append (reverse acc) left))))
       ;; same
       ,(let* ((acc  (cons (first left) acc))
               (left (rest left)))
          (if left
              `(let ((,*inline-sort-left-head*
                       (funcall ,*inline-sort-key* ,(first left))))
                 ,(emit-merge-1 left right acc))
              (emit-permute *inline-sort-destinations*
                            (append (reverse acc) right))))))

(defun emit-merge (left right)
  "Caching calls to KEY means we have to special-case empty lists
   (which doesn't happen when we sort, anyway)"
  (cond ((null left)
         (emit-permute right right))
        ((null right)
         (emit-permute left left))
        (t
         (let ((*inline-sort-destinations* (append left right))
               (*inline-sort-left-head*  (gensym "LEFT-HEAD-KEY"))
               (*inline-sort-right-head* (gensym "RIGHT-HEAD-KEY")))
           `(let ((,*inline-sort-left-head*  (funcall ,*inline-sort-key*
                                                      ,(first left)))
                  (,*inline-sort-right-head* (funcall ,*inline-sort-key*
                                                      ,(first right))))
              ,(emit-merge-1 left right nil))))))

(defun emit-sort-1 (values length)
  "Unrolled and inlined recursive merge sort generator.
   Lists of length 1 or less are trivially sorted; recurse
   on the rest."
  (when (> length 1)
    (let* ((split (truncate length 2))
           (left  (subseq values 0 split))
           (right (subseq values split)))
      `(progn
         ,(emit-sort-1 left  split)
         ,(emit-sort-1 right (- length split))
         ,(emit-merge left right)))))

(defun emit-sort (values *inline-sort-comparator* *inline-sort-key*)
  (emit-sort-1 values (length values)))
{% endcodeblock %}

Finally, handling arbitrary places, thus letting the macro take care
of writing results back to the places, is just regular macrology.

{% codeblock CL-style inline sort macro %}
(defmacro inline-sort ((comparator &key key (overwrite t))
                       &body values
                       &environment env)
  "Sorts all VALUES in increasing order with respect to COMPARATOR and
   KEY.  COMPARATOR should be a strict order, like CL:<, and KEY defaults
   to NIL (which is interpreted as the identity).  By default, the result
   is written back to the places; that's skipped if OVERWRITE is NIL. A
   literal NIL value for overwrite will avoid generating any write.
   The SORT form always evaluates to the sorted values, in order."
  (let (vars vals
        store-vars writer-forms
        reader-forms
        temps
        (_comparator (gensym "COMPARATOR"))
        (_key        (gensym "KEY"))
        (_overwrite  (gensym "OVERWRITE")))
    (loop for value in (reverse values) do
      (push (gensym "TEMP") temps)
      ;; only use the setf expansion if we might write to the place.
      (if (not overwrite)
          (push value reader-forms)
          (multiple-value-bind (var val store-var writer reader)
              (get-setf-expansion value env)
            (setf vars (append var vars)
                  vals (append val vals))
            (push store-var store-vars)
            (push writer writer-forms)
            (push reader reader-forms))))
    `(let* ((,_comparator ,comparator)
            (,_comparator (if (functionp ,_comparator)
                              ,_comparator
                              (symbol-function ,_comparator)))
            (,_key        ,(or key '#'identity))
            (,_key        (if (functionp ,_key)
                              ,_key
                              (symbol-function ,_key)))
            (,_overwrite  ,overwrite)
            ,@(mapcar 'list vars vals)
            ,@(mapcar 'list temps reader-forms))
       (declare (ignorable ,_comparator ,_key ,_overwrite))
       ,(emit-sort temps _comparator _key)
       ,(and overwrite
             `(when ,_overwrite
                ,@(loop
                    for value in values
                    for store-var-list in store-vars
                    for writer in writer-forms
                    for temp in temps
                    collect
                    (progn
                      (unless (= 1 (length store-var-list))
                        (error "Can't sort multiple-value place ~S" value))
                      `(let ((,(first store-var-list) ,temp))
                         ,writer)))))
       (values ,@temps))))
{% endcodeblock %}

Now, the macro can be used to sort, e.g., vectors of double floats
"in-place" (inasmuch as copying everything to registers can be
considered in-place).

    CL-USER> (macroexpand-1 `(inline-sort (#'< :key #'-)
                               (aref array 0) (aref array 1) (aref array 2)))
    (let* ((#:comparator1184 #'<)
           (#:comparator1184
            (if (functionp #:comparator1184)
                #:comparator11184
                (symbol-function #:comparator1184)))
           (#:key1185 #'-)
           (#:key1185
            (if (functionp #:key1185)
                #:key1185
                (symbol-function #:key1185)))
           (#:overwrite1186 t)
           (#:array1195 array)
           (#:array1192 array)
           (#:array1189 array)
           (#:temp1193 (aref #:array1195 0))
           (#:temp1190 (aref #:array1192 1))
           (#:temp1187 (aref #:array1189 2)))
      (declare (ignorable #:comparator1184 #:key1185 #:overwrite1186))
      (progn
       nil
       (progn
        nil
        nil
        (let ((#:left-head-key1196 (funcall #:key1185 #:temp1190))
              (#:right-head-key1197 (funcall #:key1185 #:temp1187)))
          (if (funcall #:comparator1184 #:right-head-key1197 #:left-head-key1196)
              (progn (rotatef #:temp1190 #:temp1187))
              (progn))))
       (let ((#:left-head-key1198 (funcall #:key1185 #:temp1193))
             (#:right-head-key1199 (funcall #:key1185 #:temp1190)))
         (if (funcall #:comparator1184 #:right-head-key1199 #:left-head-key1198)
             (let ((#:right-head-key1199 (funcall #:key1185 #:temp1187)))
               (if (funcall #:comparator1184 #:right-head-key1199
                            #:left-head-key1198)
                   (progn (rotatef #:temp1193 #:temp1190 #:temp1187))
                   (progn (rotatef #:temp1193 #:temp1190))))
             (progn))))
      (when #:overwrite1186
        (let ((#:new1194 #:temp1193))
          (sb-kernel:%aset #:array1195 0 #:new1194))
        (let ((#:new1191 #:temp1190))
          (sb-kernel:%aset #:array1192 1 #:new1191))
        (let ((#:new1188 #:temp1187))
          (sb-kernel:%aset #:array1189 2 #:new1188)))
      (values #:temp1193 #:temp1190 #:temp1187))
    t
    CL-USER> (disassemble (lambda (array)
                            (declare (type (simple-array double-float (3)) array))
                            (inline-sort (#'< :key #'-)
                              (aref array 0) (aref array 1) (aref array 2))
                            array))
    ; disassembly for (lambda (array))
    ; 0C5A5661:       F20F105201       movsd XMM2, [rdx+1]        ; no-arg-parsing entry point
    ;      666:       F20F104209       movsd XMM0, [rdx+9]
    ;      66B:       F20F104A11       movsd XMM1, [rdx+17]
    ;      670:       660F28E0         movapd XMM4, XMM0
    ;      674:       660F5725A4000000 xorpd XMM4, [rip+164]
    ;      67C:       660F28D9         movapd XMM3, XMM1
    ;      680:       660F571D98000000 xorpd XMM3, [rip+152]
    ;      688:       660F2FDC         comisd XMM3, XMM4
    ;      68C:       7A02             jp L0
    ;      68E:       7267             jb L3
    ;      690: L0:   660F28DA         movapd XMM3, XMM2
    ;      694:       660F571D84000000 xorpd XMM3, [rip+132]      ; negate double-float
    ;      69C:       660F28E0         movapd XMM4, XMM0
    ;      6A0:       660F572578000000 xorpd XMM4, [rip+120]
    ;      6A8:       660F2FE3         comisd XMM4, XMM3
    ;      6AC:       7A26             jp L1
    ;      6AE:       7324             jnb L1
    ;      6B0:       660F28E1         movapd XMM4, XMM1
    ;      6B4:       660F572564000000 xorpd XMM4, [rip+100]
    ;      6BC:       660F2FE3         comisd XMM4, XMM3
    ;      6C0:       7A27             jp L2
    ;      6C2:       7325             jnb L2
    ;      6C4:       660F28DA         movapd XMM3, XMM2
    ;      6C8:       660F28D0         movapd XMM2, XMM0
    ;      6CC:       660F28C1         movapd XMM0, XMM1
    ;      6D0:       660F28CB         movapd XMM1, XMM3
    ;      6D4: L1:   F20F115201       movsd [rdx+1], XMM2
    ;      6D9:       F20F114209       movsd [rdx+9], XMM0
    ;      6DE:       F20F114A11       movsd [rdx+17], XMM1
    ;      6E3:       488BE5           mov rsp, rbp
    ;      6E6:       F8               clc
    ;      6E7:       5D               pop rbp
    ;      6E8:       C3               ret
    ;      6E9: L2:   660F28DA         movapd XMM3, XMM2
    ;      6ED:       660F28D0         movapd XMM2, XMM0
    ;      6F1:       660F28C3         movapd XMM0, XMM3
    ;      6F5:       EBDD             jmp L1
    ;      6F7: L3:   660F28D8         movapd XMM3, XMM0
    ;      6FB:       660F28C1         movapd XMM0, XMM1
    ;      6FF:       660F28CB         movapd XMM1, XMM3
    ;      703:       EB8B             jmp L0

Bonus: Hooking in SBCL
----------------------

The inline sort supports the same options as `CL:SORT`, so it'd be
really interesting to opportunistically compile calls to the latter
into size-specialised inline sort.  The usual, portable, way to code
that sort of macro qua source-to-source optimiser in CL is with
compiler macros; compiler macros have access to all the usual
macroexpansion-time utility, but the function definition is left in
place.  That way the user can still use the function as a first-class
function, and the compiler-macro can decline the transformation if a
regular call would work better (and the compiler can ignore any
compiler macro).  That's not enough for our needs, though… and there
can only be one compiler macro per function, so adding one to code we
don't own is a bad idea.

Python's first internal representation (ir1) is optimised by
iteratively deriving tighter type information, and (mostly) pattern
matching on the type of function calls.  Its DEFTRANSFORM form lets us
add new rules, and there may be an arbitrary number of such rules for
each function.

{% codeblock Hooking our sort generator in SBCL %}
(in-package "SB-C")
(defvar *unrolled-vector-sort-max-length* 8)

(defun maybe-emit-unrolled-merge-sort (node sequence key)
  (unless (policy node (> speed space))
    (give-up-ir1-transform))
  (let* ((sequence-type (lvar-type sequence))
         (dimensions (array-type-dimensions-or-give-up
                      sequence-type)))
    (unless (typep dimensions '(cons number null))
      (give-up-ir1-transform
       "~@<sequence argument isn't a vector of known length~:@>"))
    (let ((length (first dimensions)))
      (when (> length *unrolled-vector-sort-max-length*)
        (give-up-ir1-transform
         "~@<sequence argument too long for unrolled sort ~
              (length ~S greater than ~S)~:@>"
         length *unrolled-vector-sort-max-length*))
      (if (<= length 1)
          'sequence
          `(with-array-data ((array sequence)
                             (start)
                             (end))
             (declare (optimize (insert-array-bounds-checks 0))
                      (ignore end))
             (inline-sort
                 ((%coerce-callable-to-fun predicate)
                  :key ,(if key
                            '(%coerce-callable-to-fun key)
                            '#'identity))
               ,@(loop for i below length
                       collect `(aref array (+ start ,i))))
             sequence)))))

(deftransform sort ((sequence predicate &key key)
                    * * :node node)
  "unroll sort of short vectors"
  (maybe-emit-unrolled-merge-sort node sequence key))

(deftransform stable-sort ((sequence predicate &key key)
                           * * :node node)
  "unroll stable-sort of short vectors"
  (maybe-emit-unrolled-merge-sort node sequence key))
{% endcodeblock %}

The two deftransforms at the end define new rules that match on calls
to `CL:SORT` and `CL:STABLE-SORT`, with arbitrary argument types and
return types: basic type checks are performed elsewhere, and
`maybe-emit-unrolled-merge-sort` does the rest.  Transforms are
identified by the docstring (which also improve compiler notes), and
the argument and return types, so the forms are reevaluation-safe.

All the logic lies in `maybe-emit-unrolled-merge-sort`.  The `policy`
form checks that the optimisation policy at the call node has `speed`
greater than `space`, and gives up on the transformation otherwise.
The next step is to make sure the sequence argument is an array, and
that its dimensions are known and define a vector (its dimension list
is a list of one number).  The final guard makes sure we only
specialise on small sorts (at most
`*unrolled-vector-sort-max-length*`).

Finally, we get to code generation itself.  A vector of length 1 or 0
is trivially pre-sorted.  I could also directly emit the inner
`inline-sort` form, but SBCL has some stuff to hoist out computations
related to hairy arrays.  `with-array-data` takes a potentially
complex array (e.g. displaced, or not a vector), and binds the
relevant variables to the underlying simple array of rank 1, and the
start and end indices corresponding to the range we defined
(defaulting to the whole range of the input array).  Bound checks are
eliminated because static information ensures the accesses are safe
(or the user lied and asked not to insert type checks earlier), and
the `start` index is declared to be small enough that we can add to it
without overflow -- Python doesn't implement the sort of sophisticated
shape analyses that could figure that out.  Finally, a straight
`inline-sort` form can be emitted.

That machinery means we'll get quicker and shorter inline sort code
when the size is known ahead of time.  For example, a quick
disassembly shows the following is about one fourth the size of the
size-generic inline code (with `(simple-array double-float (*))`, and
`(optimize speed (space 0))`.
    CL-USER> (lambda (x)
               (declare (type (simple-array double-float (4)) x)
                        (optimize speed))
               (sort x #'<))
    #<FUNCTION (lambda (x)) {100BFF457B}>
    CL-USER> (disassemble *)
    ; disassembly for (lambda (x))
    ; 0BFF45CF:       F20F105A01       movsd XMM3, [rdx+1]        ; no-arg-parsing entry point
    ;      5D4:       F20F104209       movsd XMM0, [rdx+9]
    ;      5D9:       F20F104A11       movsd XMM1, [rdx+17]
    ;      5DE:       F20F105219       movsd XMM2, [rdx+25]
    ;      5E3:       660F2FC3         comisd XMM0, XMM3
    ;      5E7:       7A0E             jp L0
    ;      5E9:       730C             jnb L0
    ;      5EB:       660F28E3         movapd XMM4, XMM3
    ;      5EF:       660F28D8         movapd XMM3, XMM0
    ;      5F3:       660F28C4         movapd XMM0, XMM4
    [...]

Even for vectors of length 8 (the default limit), the specialised
merge sort is shorter than SBCL's inlined heapsort, and about three
times as fast on shuffled vectors.

That's it
---------

It took me much longer to write this up than to code the generator,
but I hope this can be useful to other people.  One thing I'd like to
note is that sorting networks are much harder to get right than this
generator, and pessimise performance: without branches, there must be
partially redundant computations on non-power-of-two sizes.  In the
absence of solid compiler support for conditional swaps, I doubt the
additional overhead of *optimal* sorting networks can be compensated
by the simpler control flow, never mind the usual odd-even or bitonic
networks.
