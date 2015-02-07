---
layout: post
title: "How to define new intrinsics in SBCL"
date: 2014-08-16 22:55
comments: true
categories: 
---

This
[Stack Overflow](http://stackoverflow.com/questions/25078285/replacing-a-32-bit-loop-count-variable-with-64-bit-introduces-crazy-performance)
post points out an obscure and undocumented weakness in Intel's
implementation of the POPCNT instruction: although the population
count (number of bits equal to 1) is only a function of the source
argument, hardware schedules it as though it also depended on the
destination.  GCC, clang and MSVC all fail to take this issue into
account.

Until a new patched version of my favourite C compiler is released,
there aren't many tasteful workarounds for this performance bug.  I'd
have to switch to inline asm, and either force the compiler to
allocate the same register to the input and the result, or force
different registers and clear the spurious dependency with a xor.
Ideally, I wouldn't impose any additional constraint on the register
allocator and only insert a xor if the destination and source
registers don't match.

SBCL easily supports this use case, without having to re-release or
even recompile the implementation: VOPs (virtual operations) execute
arbitrary CL code during code generation and they can be defined at
runtime.

The first step is to make sure that SBCL's assembler knows how to emit
popcnt: the assembler can also be extended at runtime, but that's more
hairy and a topic for another post.  Instruction encodings are defined
in `src/compiler/$ARCH/insts.lisp`, and a quick grep reveals
`(define-instruction popcnt (segment dst src) ...)`: the x86-64
backend learned about popcnt in May 2013 (thanks to Douglas Katzman).

We define VOPs via `define-vop`, a macro that exposes many options.
Most of the time, it's easiest to look at a pre-existing definition
for an operation that's similar to the one we want to add.  Popcount
looks like integer negation: it has a single (machine integer)
argument and returns another integer.  Integer negation is defined in
`src/compiler/$ARCH/arith.lisp`:

{% codeblock %}
;;;; unary operations

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))

(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg) :target res))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg) :target res))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 64) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (move res x)
    (inst neg res)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (move res x)
    (inst neg res)))

(define-vop (fast-negate/unsigned signed-unop)
  (:args (x :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:translate %negate)
  (:generator 3
    (move res x)
    (inst neg res)))
{% endcodeblock %}

The code snippet above includes a bit of boilerplate to factor out
commonalities via inheritance.  The first definition introduces
`fast-safe-arith-op`, VOPs that apply in both high speed and high
safety settings (the rest is copy/pasted noise from earlier ports that
sport a scheduler); the second one extends `fast-safe-arith-op` to
define `fixnum-unop`, a base definition for single-argument operations
on fixnums, while the third one is the same, but for machine integers.
The last three definitions fill in the blanks so the compiler can
compile `%negate` of fixnum, signed and unsigned integers.  The
`(:translate %negate)` bit means that these VOPs can be emitted
instead of calls to `%negate`.  The integer after `:generator` defines
the "cost" of each variant; the compiler will choose the (applicable)
variant with the least cost and execute the code sequence that follows
to convert a call to `%negate` into machine code.

This kind of implementation inheritance is fine for an SBCL backend,
where we define many VOPs and expect developers to understand the
system.  I doubt it's a didactic win.  Let's do something simpler for
`popcnt`.  In the interest of simplicity, I'll also completely
disregard powerful details in `define-vop` that are rarely relevant
when defining intrinsics that map directly to machine instructions.

First, we need to tell the compiler that we're about to do special
things to a function named `popcnt` (and to blow away any pre-existing
information if the `defknown` form is re-evaluated).

{% codeblock %}
(defpackage "POPCNT"
  (:use "CL")
  (:export "POPCNT"))

(in-package "POPCNT")

(sb-c:defknown popcnt ((unsigned-byte 64)) (integer 0 64)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)
{% endcodeblock %}

This says that `popcnt` accepts a 64-bit unsigned integer and returns
an integer between 0 and 64 (inclusively), and that the function can
be constant-folded, flushed (eliminated as dead code) and moved around
(it's pure).

Now, to define a VOP that implements `popcnt`:
{% codeblock %}
(in-package "SB-VM")

(define-vop (popcnt:popcnt)
  (:policy :fast-safe)
  (:translate popcnt:popcnt)
  (:args (x :scs (unsigned-reg) :target r))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 3
    (unless (location= r x) ; only break the spurious dep. chain
      (inst xor r r))       ; if r isn't the same register as x.
    (inst popcnt r x)))
{% endcodeblock %}

We define a new VOP named `popcnt:popcnt` (the name is arbitrary, as
long as it doesn't collide with another VOP) that is applicable at all
optimization policies (both high speed and high debug level), and that
implements `popcnt:popcnt`.  Its first and only argument, `x`, is an
`unsigned-num`, an unsigned machine integer, that can only be stored
in a register.  Moreover, if possible, we'd like `x` to be allocated
the same register as the result, `r`.  There's only one result (`r`)
and it's an unsigned machine integer in a register, just like `x`.
The generator, of cost 3 (a common default for arithmetic operations),
breaks any dependency chain in `r` if necessary, and stores the
population count of `x` in `r`.

At first sight, the `defknown` form seems to conflict with the VOP.
We declare that the return value of `popcnt` is a small integer,
clearly a fixnum, and then define a VOP that returns a machine
integer.  The subtlety is that `defknown` is concerned with IR1, the
higher level intermediate representation, which works on CL types
(i.e, types as sets) and abstract values.  VOPs, on the other hand,
are defined for the lower level IR2, where types describe concrete
representations (like C).  It is perfectly meaningful to say that a
small integer will be represented as an untagged machine integer.

The next step isn't strictly necessary, but helps people who like
their REPL.  The compiler knows how to compile calls to `popcnt`, so
we can define `popcnt`... as a call to `popcnt`.  Our new function is
now a first-class value that can be called from interpreted code and
passed to higher-order functions, like the compiler's constant-folding
pass.

{% codeblock %}
(in-package "POPCNT")

(defun popcnt (x)
  (popcnt x))
{% endcodeblock %}

    CL-USER> (disassemble 'popcnt:popcnt)
    ; disassembly for POPCNT:POPCNT
    ; Size: 25 bytes
    ; 07FCDB6E:       4831D2           XOR RDX, RDX               ; no-arg-parsing entry point
    ;       71:       F3480FB8D1       POPCNT RDX,RCX
    ;       76:       48D1E2           SHL RDX, 1
    ;       79:       488BE5           MOV RSP, RBP
    ;       7C:       F8               CLC
    ;       7D:       5D               POP RBP
    ;       7E:       C3               RET
    [ error trap noise ]
    CL-USER> (popcnt:popcnt 42)
    3

The disassembly shows that we get the code that we expect, including
the dependency-breaking workaround, and the smoke test passes.
There's one interesting detail: we only defined a VOP that returns a
machine integer.  However, `popcnt` returns a tagged value (a fixnum),
and does so with an efficient shift.  IR2 takes care of inserting any
coercion needed between VOPs (e.g., between `popcnt` and the VOP used to
return boxed values from functions), and the IR1 `defknown` guarantees
that the result of `popcnt`, despite being *represented* in an
unsigned machine integer, is small enough for a fixnum.

Let's see what happens when we feed arithmetic into `popcnt`, e.g.:

    CL-USER> (disassemble (lambda (x y)
                            (declare (type (unsigned-byte 32) x y))
                            (popcnt:popcnt (+ x y))))
    ; disassembly for (LAMBDA (X Y))
    ; Size: 55 bytes
    ; 0752BD59:       4801FA           ADD RDX, RDI               ; no-arg-parsing entry point
    ;       5C:       48D1FA           SAR RDX, 1
    ;       5F:       F3480FB8D2       POPCNT RDX,RDX
    ;       64:       48D1E2           SHL RDX, 1
    ;       67:       488BE5           MOV RSP, RBP
    ;       6A:       F8               CLC
    ;       6B:       5D               POP RBP
    ;       6C:       C3               RET

After adding two fixnums, an automatic coercion unboxes the resulting
fixnum into a machine integer which is then passed to `popcnt`
(note the lack of dependency-breaking `xor` now that the source and
destination are the same register).

That's pretty good code, but we can do better: fixnums are tagged with
0, so we can simply pass fixnums to `popcnt` without untagging.

This is where the cost parameter to `:generator` comes in: we can
define another VOP for `popcnt` of fixnums and bias the compiler to
prefer the fixnum VOP.

{% codeblock %}
(in-package "SB-VM")

(define-vop (popcnt/fx)
  (:policy :fast-safe)
  (:translate popcnt:popcnt)
  (:args (x :scs (any-reg) :target r))
  (:arg-types positive-fixnum)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 2 ; 2 is lower than 3, so popcnt/fx is preferable to popcnt
    (unless (location= r x)
      (inst xor r r))
    (inst popcnt r x)))
{% endcodeblock %}

    CL-USER> (disassemble (lambda (x y)
                            (declare (type (unsigned-byte 32) x y))
                            (popcnt:popcnt (+ x y))))
    ; disassembly for (LAMBDA (X Y))
    ; Size: 47 bytes
    ; 07BEABE9:       4801FA           ADD RDX, RDI               ; no-arg-parsing entry point
    ;      BEC:       F3480FB8D2       POPCNT RDX,RDX
    ;      BF1:       48D1E2           SHL RDX, 1
    ;      BF4:       488BE5           MOV RSP, RBP
    ;      BF7:       F8               CLC
    ;      BF8:       5D               POP RBP
    ;      BF9:       C3               RET

Unlike many low-level languages, CL includes a standard function for
population count, `logcount`.  SBCL includes a VOP for `logcount`
(with a cost of 14), which we can supersede with our own `popcnt`-based
VOPs: we only have to replace `(:translate popcnt:popcnt)` with 
`(:translate logcount)`.  That's an easy improvement but isn't in trunk
because `popcnt` is a recent x86 extension.

Adding VOPs for (ad-hoc) polymorphic or otherwise generic functions
can be surprising: a VOP will only be considered if the arguments *and
the return values* are known to have CL types that are compatible with
the VOP's representation specification.  For `popcnt`, we guarantee
that the return value is a positive integer between 0 and 64; for
`cl:logcount`, the `defknown` guarantees that the return value is a
positive fixnum.  In both cases, the return values can always be
represented as an unsigned machine integer, so our new VOPs will
always match if the argument fits in positive fixnums or unsigned
machine integers (and will have priority over the generic x86-64 VOP
because their cost is lower).  More complex cases depend on
`derive-type` optimizers, but that's rarely necessary when defining
instrinsics for low-level code.
