---
layout: post
title: "Fresh in SBCL 1.1.8: SSE intrinsics!"
date: 2013-06-05 17:02
comments: true
categories: 
---

<blockquote class="twitter-tweet"><p>Huge month for <a href="https://twitter.com/search/%23SBCL">#SBCL</a>: devious bugs fixed, new optimisations, new features. Help us find regressions in HEAD!<a href="http://t.co/GNBITGg8Y4" title="http://sbcl.git.sourceforge.net/git/gitweb.cgi?p=sbcl/sbcl.git">sbcl.git.sourceforge.net/git/gitweb.cgi…</a></p>&mdash; Paul Khuong (@pkhuong) <a href="https://twitter.com/pkhuong/status/338013156238565376">May 24, 2013</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>


One feature I'm particularly excited about is the basic support for
SSE intrinsics on x86-64.  It's only the fundamental infrastructure
needed for the compiler and runtime to manipulate and compile
operations on SIMD packs (the platform-independent name suggested by
[Kevin Reid](http://kpreid.livejournal.com/)).  However, now that it's
merged in, we can easily add new functionality that maps (more or
less) directly to SSE assembly in a running image, like the
sb-rotate-byte contrib does for bitwise rotations.

There is currently no such contrib in SBCL, although Alexander
Gavrilov has been maintaining
[cl-simd](https://github.com/angavrilov/cl-simd), an extension for
SBCL and ECL (he also kept an old SSE/SBCL branch of mine on life
support since 2008).  I'm really not sure what the right interface
will look like for Common Lisp, so I decided to only provide the
mechanism to implement such an interface; when something emerges that
looks sane and has been used in anger, we can merge it in.

In the meantime, we get to define our own interface and see where that
experience leads us.  In this post, I'll use a classic example to
guide my first stab at an interface: the
[Mandelbrot set](http://en.wikipedia.org/wiki/Mandelbrot_set).  I'll
only define as much of an interface as I need, and adapt it along the
way if I see the need for more sophistication; I prefer to build
programs and libraries bottom-up and iteratively, rather than trying
to divine my requirements ahead of time.  A consequence of this
approach is that I have to/am able to maintain a dynamic development
style, even when working with (and working *on*) SSE intrinsics.  I
know many people use REPLs as desktop calculators; maybe others can be
persuaded to also use SBCL as an SIMD calculator, to doodle on SSE code
sequences (:

Packed floating-point arithmetic
--------------------------------

{% codeblock definitions %}
(defpackage "MANDELBROT"
  (:use "CL" "SB-EXT" "SB-C")) ; SB-EXT for SIMD-PACK, SB-C for DEFKNOWN etc

(in-package "MANDELBROT")

(defknown (f4+ f4* f4-) ((simd-pack single-float) (simd-pack single-float))
    (simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)
{% endcodeblock %}

The last form adds three new functions (`f4+`, `f4*` and `f4-`) to the
compiler's database (fndb).  Each function accepts two packs of
single floats as arguments, and returns another single float pack (the
result of element-wise addition, multiplication or subtraction).
Moreover, the functions can be reordered and flushed as dead code by
the compiler, and, whenever they appear in code, they should
ultimately be translated to assembly code.  The last bit is so the
runtime system silently overwrites the database if the functions
are already there, rather than warning at load-time.

Next, we add a template (VOP) to convert calls to `f4+` into assembly
code.  This task is so deeply tied with the internals that it makes
sense to just do it from SB-VM.

{% codeblock vops %}
(in-package "SB-VM")

(define-vop (mandelbrot::f4+)
  (:translate mandelbrot::f4+)
  (:policy :fast-safe)
  (:args (x :scs (single-sse-reg) :target r) (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator 4
    (cond ((location= r y)
           (inst addps y x))
          (t
           (move r x)
           (inst addps r y)))))
{% endcodeblock %}

The first line defines a VOP with name `mandelbrot::f4+` (it's an
arbitrary symbol and the important bit is only to avoid collisions,
but good naming is useful when reading compiler traces or optimisation
notes).

Then, we specify that it's to be automatically considered when there
is a call to `mandelbrot::f4+`, and that it can be used both in fast
and in safe code.

The template converts a call with two arguments, `x` and `y`, that
must both be in SSE registers of single floats.  Both value must be
represented as single floats in SIMD packs: when defining VOPs,
types refer to primitive types, and primitive types are concerned with
representation (like C types) rather than only sets of values (a
given CL value or type can be mapped to many primitive types
depending on the context).  We also declare a preference for `x` to be
allocated in the same register as the result `r`, if possible.

The template has a single result, which is also an SIMD pack of single
floats allocated in an SSE register of single floats.

Finally, we have code to emit assembly.  If `r` and `y` were packed
(register-allocated) in the same register, we exploit commutativity to
add `x` into `y`.  Otherwise, we move `x` into `r` if necessary (the
`move` function takes care of checking for equivalent locations), and
emit a packed single float addition (`addps`) of `y` into `r` (which
has just been overwritten with the contents of `x`).

The reason we have to specify that the SSE registers hold single
float values (rather than doubles or integers) is for functions like
`move` to emit a move of FP SSE registers rather than integer, for
example.  However, this additional information doesn't really affect
register allocation: the three storage classes (SC) --
`single-sse-reg`, `double-sse-reg` and `int-sse-reg` -- all map to the
same storage base (SB), and the same SSE register can be used for an
`simd-pack-single` value at one program point, an `simd-pack-int` at
another, and a `double-float` at yet another.

This VOP is the bare minimum for a useful packed addition of single
floats: in the future, it will probably make sense to add support for
a constant argument, which could be directly loaded from a
RIP-relative address to save a register (SBCL isn't clever enough to
determine when it makes sense to keep a constant in a register across
a basic block or a loop).

We do the same for multiplication and subtraction.  There's one last
twist for `f4-`.  We can't exploit commutativity there, so we have to
make sure `y` and `r` aren't packed in the same register; this is
achieved by extending the live range of `r` from the end of the
(default) live range for `x`.  There are obvious opportunities for
macro-isation, but, again, I can refactor when I have a clear need
(e.g. when I decide to add a dozen more intrinsics).

{% codeblock vops %}
(define-vop (mandelbrot::f4*)
  (:translate mandelbrot::f4*)
  (:policy :fast-safe)
  (:args (x :scs (single-sse-reg) :target r) (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator 4
    (cond ((location= r y)
           (inst mulps y x))
          (t
           (move r x)
           (inst mulps r y)))))

(define-vop (mandelbrot::f4-)
  (:translate mandelbrot::f4-)
  (:policy :fast-safe)
  (:args (x :scs (single-sse-reg) :target r) (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (r :scs (single-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-single)
  (:generator 4
    (move r x)
    (inst subps r y)))
{% endcodeblock %}

Now that we've declared the functions and defined one way to convert
each to machine code, we can define stubs to interact with them from
interpreted code or as normal first-class functions.  The compiler
already has all the information needed to convert any call to `f4+` et
al to machine code; however, no such function is actually defined.  We
can define them with what looks like infinite recursion: the
`defknown` will ensure the compiler inserts type checks for
`(simd-pack single-float)` as needed and infers that the result is
also an `(simd-pack single-float)`, so the templates will be
applicable.

{% codeblock intrinsics %}
(in-package "MANDELBROT")

(macrolet ((define-stub (name)
             `(defun ,name (x y)
                (,name x y))))
  (define-stub f4+)
  (define-stub f4*)
  (define-stub f4-))
{% endcodeblock %}

Now, we can call our new functions at the REPL, pass them to
higher-order functions, etc., like any normal function.

{% codeblock %}
MANDELBROT> (%make-simd-pack-single 1.0 2.0 3.0 4.0) ; from sb-ext
#<SIMD-PACK 1.0000000e+0 2.0000000e+0 3.0000000e+0 4.0000000e+0>
MANDELBROT> (f4+ * *)
#<SIMD-PACK 2.0000000e+0 4.0000000e+0 6.0000000e+0 8.0000000e+0>
{% endcodeblock %}

The first form makes an `simd-pack` from four single float values.  We
must already track at compile-time whether each SIMD pack value is a
pack of singles, doubles or integers, so it makes sense to do the same
at runtime: that way we preserve type information when constant
folding, and that we can print them nicely.  Thus, we can call `f4+`
on that value, and see that `1.0 + 1.0 = 2.0`.  We can also
disassemble the function `f4*` to confirm that it is a normal function
that can be passed around (in this case to `disassemble`), and that it
doesn't endlessly recurse.

{% codeblock %}
MANDELBROT> (disassemble #'f4*)
; disassembly for F4*                                         ; type checks hidden in the prologue
; Size: 112 bytes
; 08581B29:       0F28D0           MOVAPS XMM2, XMM0          ; no-arg-parsing entry point
;       2C:       0F59D1           MULPS XMM2, XMM1
;       2F:       49896C2440       MOV [R12+64], RBP          ; allocate a boxed return value
;       34:       4D8B5C2418       MOV R11, [R12+24]
;       39:       498D5320         LEA RDX, [R11+32]
;       3D:       4939542420       CMP [R12+32], RDX
;       42:       7641             JBE L2
;       44:       4989542418       MOV [R12+24], RDX
;       49:       498D530F         LEA RDX, [R11+15]
;       4D: L0:   48C742F165030000 MOV QWORD PTR [RDX-15], 869
;       55:       48C742F902000000 MOV QWORD PTR [RDX-7], 2
;       5D:       0F295201         MOVAPS [RDX+1], XMM2
;       61:       49316C2440       XOR [R12+64], RBP
;       66:       7403             JEQ L1
;       68:       0F0B09           BREAK 9                    ; pending interrupt trap
;       6B: L1:   488BE5           MOV RSP, RBP
;       6E:       F8               CLC
;       6F:       5D               POP RBP
;       70:       C3               RET
{% endcodeblock %}

A first stab at the Mandelbrot inner loop
-----------------------------------------

The inner loop body to approximate the Mandelbrot set is simply `z' =
z*z + c`, where `z` and `c` are complex values.  If we expand the
complexes into their real and imaginary parts (`zr + i zi`, `cr + i
ci`), we find `z*z+c = (zr*zr - zi*zi + cr) + i(2*zr*zi + ci)`.  That's easily
vectorised if we have a pack of four `zr` values, another for the
`zi`, and similarly for `cr` and `ci`.

{% codeblock intrinsics %}
(deftype f4 ()
  '(simd-pack single-float))

(declaim (inline %mandelbrot-iter replicate-float))
(defun replicate-float (x)
  (%make-simd-pack-single x x x x))
{% endcodeblock %}

{% codeblock mandelbrot %}
(defun %mandelbrot-iter (zr zi cr ci)
  (declare (optimize speed)
           (type f4 zr zi cr ci))
  (let* ((r (f4- (f4* zr zr)
                 (f4* zi zi)))
         (i/2 (f4* zr zi))
         (i   (f4+ i/2 i/2)))
    (values (f4+ r cr)
            (f4+ i ci))))
{% endcodeblock %}

This is a straight translation of the scalar formula above, but it
computes four values in parallel (that's why struct of array layouts
lead to easy to vectorisation).  Crucially, a disassembly reveals we
get the code we expect:

{% codeblock %}
MANDELBROT> (disassemble '%mandelbrot-iter)
; disassembly for %MANDELBROT-ITER
; Size: 248 bytes
; 0BCEACD5:       0F28E0           MOVAPS XMM4, XMM0          ; no-arg-parsing entry point
;      CD8:       0F59E0           MULPS XMM4, XMM0
;      CDB:       0F28E9           MOVAPS XMM5, XMM1
;      CDE:       0F59E9           MULPS XMM5, XMM1
;      CE1:       0F5CE5           SUBPS XMM4, XMM5
;      CE4:       0F59C1           MULPS XMM0, XMM1
;      CE7:       0F58C0           ADDPS XMM0, XMM0
;      CEA:       0F58E2           ADDPS XMM4, XMM2
;      CED:       0F58C3           ADDPS XMM0, XMM3
[... allocate boxed return values]
{% endcodeblock %}

The beauty of Python's representation selection capabilities is that
we can specify multiple representations (unboxed in a register or on
stack, or boxed as a generic pointer to a heap-allocated value) for
SIMD packs, and the right (usually…) one will be chosen depending on
context.  In this case, the function receives boxed SIMD packs,
unboxes them into SSE registers in the prologue, performs its
FP arithmetic only on SSE registers, and converts the results back
into boxed SIMD packs.

Even better: it seems to compute the right thing (:
{% codeblock %}
MANDELBROT> (let ((z (complex 1.0 2.0))
                  (c (complex 3.0 4.0)))
              (+ (* z z) c))
#C(0.0 8.0)
MANDELBROT> (replicate-float 1.0)
#<SIMD-PACK 1.0000000e+0 1.0000000e+0 1.0000000e+0 1.0000000e+0>
MANDELBROT> (let ((zr (replicate-float 1.0))
                  (zi (replicate-float 2.0))
                  (cr (replicate-float 3.0))
                  (ci (replicate-float 4.0)))
              (%mandelbrot-iter zr zi cr ci))
#<SIMD-PACK 0.0000000e+0 0.0000000e+0 0.0000000e+0 0.0000000e+0>
#<SIMD-PACK 8.0000000e+0 8.0000000e+0 8.0000000e+0 8.0000000e+0>
{% endcodeblock %}

The first return value is a pack of real components, and the second a
pack of imaginary components, and the values do correspond to the
scalar computation in normal complex arithmetic.

Comparisons and packed integers
-------------------------------

The next step is to compute the (squared) norm of complex values, in
order to detect escape from the orbit.
{% codeblock mandelbrot %}
(declain (inline %norm^2))
(defun %norm^2 (r i)
  (declare (optimize speed) (type f4 r i))
   (f4+ (f4* r r) (f4* i i)))
{% endcodeblock %}

Again, a couple smoke tests, and a disassembly
{% codeblock %}
MANDELBROT> (apply '%norm^2 /)
#<SIMD-PACK 6.4000000e+1 6.4000000e+1 6.4000000e+1 6.4000000e+1>
MANDELBROT> (%norm^2 (%make-simd-pack-single 1.0 2.0 3.0 4.0)
                     (%make-simd-pack-single -5.0 -6.0 -7.0 -9.0))
#<SIMD-PACK 2.6000000e+1 4.0000000e+1 5.8000000e+1 9.7000000e+1>
MANDELBROT> (disassemble '%norm^2)
; disassembly for %NORM^2
; Size: 115 bytes
; 05FA04D9:       0F59C0           MULPS XMM0, XMM0           ; no-arg-parsing entry point
;      4DC:       0F59C9           MULPS XMM1, XMM1
;      4DF:       0F58C1           ADDPS XMM0, XMM1
[... boxing ...]
{% endcodeblock %}

Finally, we have to compare floats (squared norms) against a limit
(4.0), and perform some integer arithmetic to count the number of
iterations until escape.  `f4-sign-mask` is the first instance of a
function that accepts arbitrary SIMD pack types: singles, integers, or
even doubles, it'll extract sign bits from the four 32-bit chunks.

{% codeblock definitions %}
(defknown f4<= ((simd-pack single-float) (simd-pack single-float))
    (simd-pack (signed-byte 32))
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown i4- ((simd-pack (signed-byte 32)) (simd-pack (signed-byte 32)))
    (simd-pack (signed-byte 32))
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f4-sign-mask (simd-pack) (unsigned-byte 4)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)
{% endcodeblock %}

The VOPs are really straightforward and somewhat repetitive, again: we
only want the bare minimum to get something working, and fancy special
cases can wait until they actually show up.

{% codeblock vops %}
(define-vop (mandelbrot::f4<=)
  (:translate mandelbrot::f4<=)
  (:policy :fast-safe)
  (:args (x :scs (single-sse-reg) :target r) (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (r :scs (int-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-int)
  (:generator 4
    (move r x)
    (inst cmpps :le r y)))

(define-vop (mandelbrot::i4-)
  (:translate mandelbrot::i4-)
  (:policy :fast-safe)
  (:args (x :scs (int-sse-reg) :target r) (y :scs (int-sse-reg)))
  (:arg-types simd-pack-int simd-pack-int)
  (:results (r :scs (int-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-int)
  (:generator 4
    (move r x)
    (inst psubd r y)))

(define-vop (mandelbrot::f4-sign-mask)
  (:translate mandelbrot::f4-sign-mask)
  (:policy :fast-safe)
  (:args (x :scs (int-sse-reg single-sse-reg double-sse-reg)))
  (:arg-types simd-pack)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst movmskps r x)))
{% endcodeblock %}

Again, we add stubs to easily work at the REPL
{% codeblock intrinsics %}
(macrolet ((define-stub (name)
             `(defun ,name (x y)
                (,name x y))))
  (define-stub f4+)
  (define-stub f4*)
  (define-stub f4-)
  (define-stub f4<=)
  (define-stub i4-))

(defun f4-sign-mask (x)
  (f4-sign-mask x))
{% endcodeblock %}

And some smoke tests
{% codeblock %}
MANDELBROT> (f4<= (%make-simd-pack-single 1.0 2.0 0.0 4.0)
                  (%make-simd-pack-single 1.0 1.0 -0.0 5.0))
#<SIMD-PACK  FF FF FF FF  00 00 00 00  FF FF FF FF  FF FF FF FF>
MANDELBROT> (i4- (%make-simd-pack-ub32 0 0 0 0) *)
#<SIMD-PACK  01 00 00 00  00 00 00 00  01 00 00 00  01 00 00 00> ; little-endian
MANDELBROT> (f4-sign-mask **)
13
MANDELBROT> (format t "~2,4,'0r~%" *)
1101 ; the 4 bits
NIL
MANDELBROT> (%simd-pack-ub32s ***)
1 ; least significant word
0
1
1
{% endcodeblock %}

`f4<=` compares two packs of single floats element by element, and
returns a pack of integers with all 32 bits set to 1 for pairs that
are `<=`, and 0 otherwise.  `i4-` subtracts 32-bit signed integers
element-wise; subtracting -1 (`#xffff...`) is equivalent to adding 1.
Finally, `f4-sign-mask` extracts the topmost (sign) bit from each
chunk of 32 bits, and returns the 4 bits as a normal integer (i.e. in
a general purpose register).  The function `sb-ext:%simd-pack-ub32s`
is useful to extract the four 32 bit unsigned values from an SIMD pack
-- in this case the result of subtracting the comparison masks from
zero -- and work on them in a scalar context.

A complete Mandelbrot inner loop
--------------------------------

The idea is that we'll want to compare the result of `%norm^2` with
the limit (4.0), and stop iterating when the maximal iteration count
is reached, or when all four norms are greater than 4.0 (all sign bits
are zero).  Until then, we can subtract from the counter to increment
the number of unescaped iterations.  When we're done, we can easily
extract individual iteration counts from the packed counters.

This yields
{% codeblock mandelbrot %}
(declaim (maybe-inline mandelbrot-escape))
(defun mandelbrot-escape (cr ci n)
  (declare (type f4 cr ci)
           (type (and unsigned-byte fixnum) n))
  (let* ((zr cr)
         (zi ci)
         (limit zr)
         (counts (%make-simd-pack-ub32 0 0 0 0)))
    (setf limit (replicate-float 4f0)) ; bamboozle the compiler into
    (dotimes (i n)                     ; keeping that in a register
      (setf (values zr zi) (%mandelbrot-iter zr zi cr ci))
      (let ((still-in-orbit (f4<= (%norm^2 zr zi) limit)))
        (setf counts (i4- counts still-in-orbit))
        (when (zerop (f4-sign-mask still-in-orbit))
          (return))))
    (%simd-pack-ub32s counts)))
{% endcodeblock %}

We can run a couple random tests and compare with a straightforward
scalar version:

{% codeblock mandelbrot %}
(defun scalar-mandelbrot (c n)
  (let ((z c))
    (dotimes (i n n)
      (setf z (+ (* z z) c))
      (when (> (abs z) 2)
        (return i)))))

(defun random-test-mandelbrot (n)
  (let* ((z (complex (- (random 4.0) 2.0)
                     (- (random 4.0) 2.0)))
         (value (scalar-mandelbrot z n)))
    (flet ((almost-replicate (x)
             (%make-simd-pack-single 0.0 x x 10.0)))
      (multiple-value-bind (v1 v2 v3 v4)
          (mandelbrot-escape (almost-replicate (realpart z))
                             (almost-replicate (imagpart z))
                             n)
        (declare (ignore v1 v4))
        (assert (= value v2 v3))))))
{% endcodeblock %}

{% codeblock %}
MANDELBROT> (mandelbrot-escape (%make-simd-pack-single 1.0 -1.0 3.0 0.0)
                               (replicate-float 0.0)
                               256)
1 ; ok: sequence 1, 2, 5, 26 escapes after two iterations
256 ; doesn't escape
0 ; escapes at the first iteration
256 ; doesn't escape
MANDELBROT> (dotimes (i 10000)
              (random-test-mandelbrot 256))
NIL ; no discrepancy
MANDELBROT> (disassemble 'mandelbrot-escape)
; disassembly for MANDELBROT-ESCAPE
; Size: 311 bytes
; 0CABDFD1:       0F28C3           MOVAPS XMM0, XMM3          ; no-arg-parsing entry point
;     DFD4:       0F28CC           MOVAPS XMM1, XMM4
;     DFD7:       0F28E8           MOVAPS XMM5, XMM0
;     DFDA:       660FEFD2         PXOR XMM2, XMM2
;     DFDE:       0F282D0B010000   MOVAPS XMM5, [RIP+267]
;     DFE5:       31C9             XOR ECX, ECX
;     DFE7:       EB4F             JMP L1
;     DFE9:       0F1F8000000000   NOP
;     DFF0: L0:   0F28F0           MOVAPS XMM6, XMM0          ; Mandelbrot loop
;     DFF3:       0F59F0           MULPS XMM6, XMM0
;     DFF6:       0F28F9           MOVAPS XMM7, XMM1
;     DFF9:       0F59F9           MULPS XMM7, XMM1
;     DFFC:       0F5CF7           SUBPS XMM6, XMM7
;     DFFF:       0F59C1           MULPS XMM0, XMM1
;     E002:       0F28C8           MOVAPS XMM1, XMM0
;     E005:       0F58C8           ADDPS XMM1, XMM0
;     E008:       0F28C6           MOVAPS XMM0, XMM6
;     E00B:       0F58C3           ADDPS XMM0, XMM3
;     E00E:       0F58CC           ADDPS XMM1, XMM4
;     E011:       0F28F0           MOVAPS XMM6, XMM0
;     E014:       0F59F0           MULPS XMM6, XMM0
;     E017:       0F28F9           MOVAPS XMM7, XMM1
;     E01A:       0F59F9           MULPS XMM7, XMM1
;     E01D:       0F58F7           ADDPS XMM6, XMM7
;     E020:       0FC2F502         CMPLEPS XMM6, XMM5
;     E024:       660FFAD6         PSUBD XMM2, XMM6
;     E028:       480F50C6         MOVMSKPS RAX, XMM6         ; test for early exit
;     E02C:       48D1E0           SHL RAX, 1
;     E02F:       4885C0           TEST RAX, RAX
;     E032:       7409             JEQ L2
;     E034:       4883C102         ADD RCX, 2
;     E038: L1:   4C39C1           CMP RCX, R8                ; iteration limit
;     E03B:       7CB3             JL L0
;     E03D: L2:   66480F7ED0       MOVD RAX, XMM2
;     E042:       660F6FC2         MOVDQA XMM0, XMM2
;     E046:       660F73D808       PSRLDQ XMM0, 8             ; extract 32 bit components
;     E04B:       66480F7EC1       MOVD RCX, XMM0
;     E050:       488D1400         LEA RDX, [RAX+RAX]
;     E054:       482315A5000000   AND RDX, [RIP+165]         ; #x1FFFFFFFE
;     E05B:       488BF8           MOV RDI, RAX
;     E05E:       48C1EF20         SHR RDI, 32
;     E062:       48D1E7           SHL RDI, 1
;     E065:       488D3409         LEA RSI, [RCX+RCX]
;     E069:       48233590000000   AND RSI, [RIP+144]         ; #x1FFFFFFFE
;     E070:       48C1E920         SHR RCX, 32
;     E074:       48D1E1           SHL RCX, 1
;     E077:       48894DF0         MOV [RBP-16], RCX          ; return them as four fixnums
;     E07B:       488D5D10         LEA RBX, [RBP+16]
;     E07F:       B908000000       MOV ECX, 8
;     E084:       F9               STC
;     E085:       488D65F0         LEA RSP, [RBP-16]
;     E089:       488B6D00         MOV RBP, [RBP]
;     E08D:       FF73F8           PUSH QWORD PTR [RBX-8]
;     E090:       C3               RET
[... error traps ...]
{% endcodeblock %}

So, there's some register allocation oddities (mostly because the VOPs
don't handle the case when both arguments are the same, I think), but
it's pretty good.  The one issue that bothers me is `(zerop
(f4-sign-mask still-in-orbit))`: it's a common operation, and there's
a slight miscompile (the result of `f4-sign-mask` is converted to a
fixnum before comparison… an instance of bad representation
selection).  These two reasons -- factoring code out and improving
code generation for a medium-level operation -- are enough for me to
add a specialised function to test if all the sign bits are zero.

A new predicate
---------------

We define predicates simply as functions that return boolean
values (this one is prefixed with `f4-` because the instruction
officially works on single floats).
{% codeblock definitions %}
(defknown f4-sign-all-zero (simd-pack) boolean
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)
{% endcodeblock %}

However, VOPs for such functions shouldn't return values; instead,
they can define how to determine that their result is true.  In this
case, if the `:z` (zero) flag is set.  Code generation will take care of
using that information in conditional branches or conditional moves.

{% codeblock vops %}
(define-vop (mandelbrot::f4-sign-all-zero)
  (:translate mandelbrot::f4-sign-all-zero)
  (:policy :fast-safe)
  (:args (x :scs (int-sse-reg single-sse-reg double-sse-reg)))
  (:arg-types simd-pack)
  (:temporary (:sc unsigned-reg) bits)
  (:conditional :z)
  (:generator 4
    (inst movmskps bits x)
    (inst test bits bits)))
{% endcodeblock %}

For example, in this stub, the compiler will emit the template,
followed by a conditional move to return T or NIL.
{% codeblock intrinsics %}
(defun f4-sign-all-zero (x)
  (f4-sign-all-zero x))
{% endcodeblock %}

{% codeblock %}
MANDELBROT> (disassemble 'f4-sign-all-zero)
; disassembly for F4-SIGN-ALL-ZERO
; Size: 40 bytes
; 0CB11978:       480F50C0         MOVMSKPS RAX, XMM0         ; no-arg-parsing entry point
;       7C:       4885C0           TEST RAX, RAX
;       7F:       BA17001020       MOV EDX, 537919511
;       84:       41BB4F001020     MOV R11D, 537919567
;       8A:       490F44D3         CMOVEQ RDX, R11
;       8E:       488BE5           MOV RSP, RBP
;       91:       F8               CLC
;       92:       5D               POP RBP
;       93:       C3               RET
{% endcodeblock %}

Wrapping up
-----------

Now, we can slightly adjust `mandelbrot-escape` to exploit this new
function:
{% codeblock mandelbrot %}
(defun mandelbrot-escape (cr ci n)
  (declare (type (simd-pack single-float) cr ci)
           (type (and unsigned-byte fixnum) n))
  (let* ((zr cr)
         (zi ci)
         (limit zr)
         (counts (%make-simd-pack-ub32 0 0 0 0)))
    (setf limit (replicate-float 4f0))
    (dotimes (i n)
      (setf (values zr zi) (%mandelbrot-iter zr zi cr ci))
      (let ((still-in-orbit (f4<= (%norm^2 zr zi) limit)))
        (setf counts (i4- counts still-in-orbit))
        (when (f4-sign-all-zero still-in-orbit) ; ** change here
          (return))))
    (%simd-pack-ub32s counts)))
{% endcodeblock %}

{% codeblock %}
MANDELBROT> (disassemble 'mandelbrot-escape)
; disassembly for MANDELBROT-ESCAPE
; Size: 311 bytes
[...]
;      B90:       0FC2F502         CMPLEPS XMM6, XMM5
;      B94:       660FFAD6         PSUBD XMM2, XMM6
;      B98:       480F50C6         MOVMSKPS RAX, XMM6
;      B9C:       4885C0           TEST RAX, RAX
;      B9F:       7409             JEQ L2
;      BA1:       4883C102         ADD RCX, 2
;      BA5: L1:   4C39C1           CMP RCX, R8
;      BA8:       7CB6             JL L0
[...]
MANDELBROT> (mandelbrot-escape (%make-simd-pack-single 1.0 -1.0 3.0 0.0)
                               (replicate-float 0.0)
                               256)
1    ;; all four values are correct
256
0
256
MANDELBROT> (dotimes (i 10000)
              (random-test-mandelbrot 256))
NIL ; all OK
{% endcodeblock %}

This all looks like a lot of work, but the vast majority of it is
reusable code to define SSE operations: 22 LOC of defknown, 83 LOC for
assembly templates, 20 LOC in stubs and utility code. There's only 31
LOC for the SIMD mandelbrot loop (`%mandelbrot-iter`, `%norm^2` and
`mandelbrot-escape`), 6 for the naïve scalar loop, and 12 for the
random test harness.  I uploaded all the files on
[github](https://github.com/pkhuong/sse-mandelbrot) to simplify
further experimentation; just fork and play around!  The function
names could certainly be improved, for one.

Miscellaneous notes
-------------------

My request for extra-hard bug shaking in SBCL 1.1.8 didn't function as
well as I'd wish: only a few hours after release (and after a week of
code freeze), we received a new bug report (with fixes committed very
rapidly, at least).  We observed the same phenomenon in 1.1.6, and
wound up being extremely conservative for 1.1.7.  I'm not sure what's
the best way to encourage testing (binary release candidates during
freeze?), but I am starting to see the point of the old odd/even
policy for Linux.  For now, the result is that 1.1.8 offers a lot of
exciting improvements, but also more than its share of bugs; for
production code, it's probably best to either build an early
1.1.8.git-commit, or wait for 1.1.9.

Long-time readers might remember that I first blogged about SSE
intrinsics in
[2008](http://pvk.ca/Blog/Lisp/hacking_SSE_intrinsics-part_1.html).
The problem is that I hit some issues with representing type
information (whether an SIMD pack contains integer, single float or
double float data), and didn't manage to find a nice resolution.  I
also had trouble with type operations (intersection, union, negation)
on `(simd-pack [type])`.  For the first problem, I bit the bullet and
created many storage classes and primitive types; as shown above, a
function can accept arbitrary `simd-pack`, and so can VOPs: when
converting to primitive types, unspecialised `simd-pack` are
treated like integer `simd-pack`.  The second, I solved by moving to
a type upgrading system inspired by array types.  There are really only
three specialised `simd-pack` types: on integers, single floats and
double floats.  Any specialised `simd-pack` type specifier must be
specialised on a recognisable subtype of one of these three types.
It's a hack, but all type operations become trivial and easy to think
about.

The reason I finally sat down, re-read my code and thought hard for a
few days is Google's Summer of Code.  A student proposed to build on
that work to vectorise standard sequence operations in CL, and I
really didn't want to be blocking their work.  In the end we were only
allotted two slots, and had to select a pair of extremely strong
proposals from ten interesting submissions… more on that soon!

In the meantime, I hope the recent burst of activity and outside
interest can motivate others to contribute to SBCL, even without
Google support.  We already have one person working on a contrib to
exploit GMP for bignum and rational arithmetic, instead of our
hand-rolled routines; I expect to see it merged in 1.1.9 or in 1.1.10.

P.S. I recently stumbled on
[Glassman's FFT](http://www.sciencedirect.com/science/article/pii/0898122182900165?np=y)
and it's really nifty: an in-order general-size FFT (not so fast when
prime factors are huge) in approximately 50 LOC.  The performance
isn't awesome on cached architectures, but shouldn't be atrocious
either… and it handles arbitrary DFT sizes.  There are issues with
numerical stability when computing the twiddle factors on the fly, and
the access patterns could be improved, but it's a very simple and
interesting foundation… especially given that the inner loop is
naturally vectorisable.
