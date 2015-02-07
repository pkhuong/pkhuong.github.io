---
layout: post
title: "SBCL: the ultimate assembly code breadboard"
date: 2014-03-15 19:56
comments: true
categories: 
---

_EDIT: Lutz Euler points out that the `NEXT` sequence (used to) encode
an effective address with an index register but no base.  The mistake
doesn't affect the meaning of the instruction, but forces a wasteful
encoding.  The difference in machine code are as follows._

_Before (14 bytes):_
{% codeblock %}
;       03:       8B043D00000000   MOV EAX, [RDI] ; _5_ useless bytes!
;       0A:       4883C704         ADD RDI, 4
;       0E:       4801F0           ADD RAX, RSI
;       11:       FFE0             JMP RAX
{% endcodeblock %}

_Now (9 bytes):_
{% codeblock %}
;       93:       8B07             MOV EAX, [RDI]
;       95:       4883C704         ADD RDI, 4
;       99:       4801F0           ADD RAX, RSI
;       9C:       FFE0             JMP RAX
{% endcodeblock %}

_I fixed the definition of `NEXT`, but not the disassembly snippets
below; they still show the old machine code._

Earlier this week, I took another look at the
[F18](http://www.greenarraychips.com/).  As usual with Chuck Moore's
work, it's hard to tell the difference between insanity and mere
brilliance ;) One thing that struck me is how small the stack is: 10
slots, with no fancy overflow/underflow trap.  The rationale is that,
if you need more slots, you're doing it wrong, and that silent
overflow is useful when you know what you're doing.  That certainly
jibes with my experience on the HP-41C and with x87.  It also reminds
me of a
[post of djb's decrying our misuse of x87's rotating stack](http://cr.yp.to/qhasm/20050210-fxch.txt):
his thesis was that, with careful scheduling, a "free" `FXCH` makes
the stack equivalent -- if not superior -- to registers.  The post
ends with a (non-pipelined) loop that wastes no cycle on shuffling
data, thanks to the x87's implicit stack rotation.

That lead me to wonder what implementation techniques become available
for stack-based VMs that restrict their stack to, e.g., 8 slots.
Obviously, it would be ideal to keep everything in registers.
However, if we do that naïvely, push and pop become a lot more
complicated; there's a reason why Forth engines usually cache only the
top 1-2 elements of the stack.

I decided to mimic the x87 and the F18 (EDIT: modulo the latter's two
TOS cache registers): pushing/popping doesn't cause any data movement.
Instead, like the drawing below shows, they decrement/increment a modular
counter that points to the top of the stack (TOS).  That would still be
slow in software (most ISAs can't index registers).  The key is that
the counter can't take too many values: only 8 values if there are 8
slots in the stack.  Stack VMs already duplicate primops for performance
reasons (e.g., to help the BTB by spreading out execution of the same
primitive between multiple addresses), so it seems reasonable to specialise
primitives for all 8 values the stack counter can take.

{% img center /images/2014-03-15-sbcl-the-ultimate-assembly-code-breadboard/rotating-stack.jpg %}

In a regular direct threaded VM, most primops would end with a code
sequence that jumps to the next one (`NEXT`), something like
    add rsi, 8  ; increment virtual IP before jumping
    jmp [rsi-8] ; jump to the address RSI previously pointed to
where `rsi` is the virtual instruction pointer, and VM instructions
are simply pointers to the machine code for the relevant primitive.

I'll make two changes to this sequence.  I don't like hardcoding
addresses in bytecode, and 64 bits per virtual instruction is overly
wasteful.  Instead, I'll encode offsets from the primop code block:
    mov eax, [rsi]
    add rsi, 4
    add rax, rdi
    jmp rax
where `rdi` is the base address for primops.
 
I also need to dispatch based on the new value of the implicit stack
counter.  I decided to make the dispatch as easy as possible by
storing the variants of each primop at regular intervals (e.g. one
page).  I rounded that up to `64 * 67 = 4288` bytes to minimise
aliasing accidents.  `NEXT` becomes something like
    mov eax, [rsi]
    add rsi, 4
    lea rax, [rax + rdi + variant_offset]
    jmp rax

The trick is that `variant_offset = 4288 * stack_counter`, and the
stack counter is (usually) known when the primitive is compiled.  If
the stack is left as is, so is the counter; pushing a value decrements
the counter and popping one increments it.

That seems reasonable enough.  Let's see if we can make it work.

Preliminaries
=============

I want to explore a problem for which I'll emit a lot of repetitive
machine code.  SLIME's REPL and SBCL's assembler are perfect for the
task!  (I hope it's clear that I'm using unsupported internals; if it
breaks, you keep the pieces.)

The basic design of the VM is:

* `r8`-`r15`: stack slots (32 bits);
* `rsi`: base address for machine code primitives;
* `rdi`: virtual instruction pointer (points to the *next* instruction);
* `rax`,`rbx`,`rcx`,`rdx`: scratch registers;
* `rsp`: (virtual) return stack pointer.

{% codeblock %}
(import '(sb-assem:inst sb-vm::make-ea)) ; we'll use these two a lot

;; The backing store for our stack
(defvar *stack* (make-array 8 :initial-contents (list sb-vm::r8d-tn
                                                      sb-vm::r9d-tn
                                                      sb-vm::r10d-tn
                                                      sb-vm::r11d-tn
                                                      sb-vm::r12d-tn
                                                      sb-vm::r13d-tn
                                                      sb-vm::r14d-tn
                                                      sb-vm::r15d-tn)))

;; The _primop-generation-time_ stack pointer
(defvar *stack-pointer*)

;; (@ 0) returns the (current) register for TOS, (@ 1) returns
;; the one just below, etc.
(defun @ (i)
  (aref *stack* (mod (+ i *stack-pointer*) (length *stack*))))

(defvar *code-base* sb-vm::rsi-tn)
(defvar *virtual-ip* sb-vm::rdi-tn)

(defvar *rax* sb-vm::rax-tn)
(defvar *rbx* sb-vm::rax-tn)
(defvar *rcx* sb-vm::rax-tn)
(defvar *rdx* sb-vm::rax-tn)

;; Variants are *primitive-code-offset* bytes apart
(defvar *primitive-code-offset* (* 64 67))

;; Each *stack-pointer* value gets its own code page
(defstruct code-page
  (alloc 0) ; index of the next free byte.
  (code (make-array *primitive-code-offset* :element-type '(unsigned-byte 8))))
{% endcodeblock %}

The idea is that we'll define functions to emit assembly code for each
primitive; these functions will be implicitly parameterised on
`*stack-pointer*` thanks to `@`.  We can then call them as many times
as needed to cover all values of `*stack-pointer*`.  The only
complication is that code sequences will differ in length, so we must
insert padding to keep everything in sync.  That's what `emit-code`
does:

{% codeblock %}
(defun emit-code (pages emitter)
  ;; there must be as many code pages as there are stack slots
  (assert (= (length *stack*) (length pages)))
  ;; find the rightmost starting point, and align to 16 bytes
  (let* ((alloc (logandc2 (+ 15 (reduce #'max pages :key #'code-page-alloc))
                          15))
         (bytes (loop for i below (length pages)
                      for page = (elt pages i)
                      collect (let ((segment (sb-assem:make-segment))
                                    (*stack-pointer* i))
                                ;; assemble the variant for this value
                                ;; of *stack-pointer* in a fresh code
                                ;; segment
                                (sb-assem:assemble (segment)
                                  ;; but first, insert padding
                                  (sb-vm::emit-long-nop segment (- alloc (code-page-alloc page)))
                                  (funcall emitter))
                                ;; tidy up any backreference
                                (sb-assem:finalize-segment segment)
                                ;; then get the (position-independent) machine
                                ;; code as a vector of bytes
                                (sb-assem:segment-contents-as-vector segment)))))
    ;; finally, copy each machine code sequence to the right code page
    (map nil (lambda (page bytes)
               (let ((alloc (code-page-alloc page)))
                 (replace (code-page-code page) bytes :start1 alloc)
                 (assert (<= (+ alloc (length bytes)) (length (code-page-code page))))
                 (setf (code-page-alloc page) (+ alloc (length bytes)))))
         pages bytes)
    ;; and return the offset for that code sequence
    alloc))
{% endcodeblock %}

This function is used by `emit-all-code` to emit the machine code for
a bunch of primitives, while tracking the start offset for each
primitive.

{% codeblock %}
(defun emit-all-code (&rest emitters)
  (let ((pages (loop repeat (length *stack*)
                     for page = (make-code-page)
                     ;; prefill everything with one-byte NOPs
                     do (fill (code-page-code page) #x90)
                     collect page)))
    (values (mapcar (lambda (emitter)
                      (emit-code pages emitter))
                    emitters)
            pages)))
{% endcodeblock %}

Now, the pièce de résistance:
{% codeblock %}
(defun next (&optional offset)
  (setf offset (or offset 0)) ; accommodate primops that frob IP
  (let ((rotation (mod *stack-pointer* (length *stack*))))
    (inst movzx *rax* (make-ea :dword :base *virtual-ip*
                                      :disp offset))
    (unless (= -4 offset)
      (inst add *virtual-ip* (+ 4 offset)))
    (if (zerop rotation)
        (inst add *rax* *code-base*)
        (inst lea *rax* (make-ea :qword :base *code-base*
                                        :index *rax*
                                        :disp (* rotation *primitive-code-offset*))))
    (inst jmp *rax*)))
{% endcodeblock %}

First steps
===========
Let's add a few simple primitives.

{% codeblock %}
(defun swap ()
  (inst xchg (@ 0) (@ 1)) ; exchange top of stack and stack[1]
  (next))

(defun dup ()
  (decf *stack-pointer*) ; grow stack (which grows down)
  (inst mov (@ 0) (@ 1)) ; and overwrite TOS
  (next))

(defun drop (&optional offset)
  (incf *stack-pointer*) ; just shrink the stack
  (next offset))

(defun add ()
  (inst add (@ 1) (@ 0)) ; second element becomes TOS
  (drop))

(defun sub ()
  (inst sub (@ 1) (@ 0))
  (drop))
{% endcodeblock %}

{% codeblock %}
CL-USER> (setf *print-length* 100)
100
CL-USER> (emit-all-code 'swap 'dup 'drop 'add 'sub)
(0 32 64 96 128)
(#S(CODE-PAGE
    :ALLOC 152
    :CODE #(69 135 193 139 4 61 0 0 0 0 72 131 199 4 72 1 240 255 224 102 15 31
            132 0 0 0 0 0 15 31 64 0 69 139 248 139 4 61 0 0 0 0 72 131 199 4
            72 141 132 6 64 117 0 0 255 224 15 31 132 0 0 0 0 0 139 4 61 0 0 0
            0 72 131 199 4 72 141 132 6 192 16 0 0 255 224 102 15 31 132 0 0 0
            0 0 102 144 69 1 193 139 ...))
 ...)
CL-USER> (defparameter *code0* (code-page-code (first (second /))))
*CODE0*
CL-USER> (defparameter *code1* (code-page-code (second (second //))))
*CODE1*
{% endcodeblock %}

The code for `swap` lives between bytes 0 and 32.  Let's take a look
at the version for `*stack-pointer* = 0` and `*stack-pointer* = 1`.

{% codeblock %}
CL-USER> (sb-sys:with-pinned-objects (*code0*)
           (sb-disassem:disassemble-memory (sb-sys:vector-sap *code0*)
                                           32))
; Size: 32 bytes
; 0669C700:       4587C1           XCHG R8D, R9D
;       03:       8B043D00000000   MOV EAX, [RDI]
;       0A:       4883C704         ADD RDI, 4
;       0E:       4801F0           ADD RAX, RSI
;       11:       FFE0             JMP RAX
;       13:       660F1F840000000000 NOP  ; padding NOPs
;       1C:       0F1F4000         NOP
NIL
CL-USER> (sb-sys:with-pinned-objects (*code1*)
           (sb-disassem:disassemble-memory (sb-sys:vector-sap *code1*)
                                           32))
; Size: 32 bytes
; 0669D810:       4587CA           XCHG R9D, R10D
;       13:       8B043D00000000   MOV EAX, [RDI]
;       1A:       4883C704         ADD RDI, 4
;       1E:       488D8406C0100000 LEA RAX, [RSI+RAX+4288]
;       26:       FFE0             JMP RAX
;       28:       0F1F840000000000 NOP
NIL
{% endcodeblock %}

`dup` is at 32-64, and `sub` at 128-152:
{% codeblock %}
CL-USER> (sb-sys:with-pinned-objects (*code0*)
           (sb-disassem:disassemble-memory (sb-sys:sap+ (sb-sys:vector-sap *code0*) 32)
                                           32))
; Size: 32 bytes
; 0669C720:       458BF8           MOV R15D, R8D
;       23:       8B043D00000000   MOV EAX, [RDI]
;       2A:       4883C704         ADD RDI, 4
;       2E:       488D840640750000 LEA RAX, [RSI+RAX+30016]
;       36:       FFE0             JMP RAX
;       38:       0F1F840000000000 NOP
NIL
CL-USER> (sb-sys:with-pinned-objects (*code0*)
           (sb-disassem:disassemble-memory (sb-sys:sap+ (sb-sys:vector-sap *code0*) 128)
                                           24))
; Size: 24 bytes
; 0669C780:       4529C1           SUB R9D, R8D
;       83:       8B043D00000000   MOV EAX, [RDI]
;       8A:       4883C704         ADD RDI, 4
;       8E:       488D8406C0100000 LEA RAX, [RSI+RAX+4288]
;       96:       FFE0             JMP RAX
NIL
{% endcodeblock %}

These are relatively tight.  I certainly like how little data
shuffling there is; the `NEXT` sequence is a bit hairy, but the
indirect branch is likely its weakest (and least avoidable) point.

Control flow primops
====================

A VM without control flow isn't even a toy.  First, unconditional
relative jumps.  These can be encoded as `[jmp] [offset]`, with the 32
bit offset relative to the end of `offset`.  We just overwrite
`*virtual-ip*` with the new address.

{% codeblock %}
(defun jmp ()
  (inst movsx *rax* (make-ea :dword :base *virtual-ip*))
  (inst lea *virtual-ip* (make-ea :dword :base *virtual-ip* :index *rax*
                                         :disp 4))
  (next))
{% endcodeblock %}

Call and return are at the heart of Forth-like engines.  `ret` is
easy: just pop from the control stack into `*virtual-ip*`.
{% codeblock %}
(defun ret ()
  (inst pop *virtual-ip*)
  (next))
{% endcodeblock %}

Call is a bit more complicated. It's like `jmp`, but pushes the
address of the *next* instruction to the control stack:
{% codeblock %}
(defun call ()
  (inst movsx *rax* (make-ea :dword :base *virtual-ip*))
  (inst add *virtual-ip* 4)
  (inst push *virtual-ip*)
  (inst add *virtual-ip* *rax*)
  (next))
{% endcodeblock %}

Let's look at the resulting machine code.
{% codeblock %}
CL-USER> (emit-all-code 'jmp 'ret 'call)
(0 32 64)
(#S(CODE-PAGE
    :ALLOC 91
    :CODE #(72 99 7 72 141 124 7 4 139 4 61 0 0 0 0 72 131 199 4 72 1 240 255
            224 15 31 132 0 0 0 0 0 95 139 4 61 0 0 0 0 72 131 199 4 72 1 240
            255 224 102 15 31 132 0 0 0 0 0 102 15 31 68 0 0 72 99 7 72 131 199
            4 87 72 1 199 139 4 61 0 0 0 0 72 131 199 4 72 1 240 255 224 144
            144 144 144 144 144 144 144 144 ...))
 ...)
CL-USER> (let ((code (code-page-code (first (second /)))))
           (sb-sys:with-pinned-objects (code)
             (sb-disassem:disassemble-memory (sb-sys:vector-sap code) 91)))
; Size: 91 bytes
; 08395200:       486307           MOVSXD RAX, DWORD PTR [RDI] ; jmp
;       03:       488D7C0704       LEA RDI, [RDI+RAX+4]
;       08:       8B043D00000000   MOV EAX, [RDI]
;       0F:       4883C704         ADD RDI, 4
;       13:       4801F0           ADD RAX, RSI
;       16:       FFE0             JMP RAX
;       18:       0F1F840000000000 NOP
;       20:       5F               POP RDI                     ; ret
;       21:       8B043D00000000   MOV EAX, [RDI]
;       28:       4883C704         ADD RDI, 4
;       2C:       4801F0           ADD RAX, RSI
;       2F:       FFE0             JMP RAX
;       31:       660F1F840000000000 NOP
;       3A:       660F1F440000     NOP
;       40:       486307           MOVSXD RAX, DWORD PTR [RDI] ; call
;       43:       4883C704         ADD RDI, 4
;       47:       57               PUSH RDI
;       48:       4801C7           ADD RDI, RAX
;       4B:       8B043D00000000   MOV EAX, [RDI]
;       52:       4883C704         ADD RDI, 4
;       56:       4801F0           ADD RAX, RSI
;       59:       FFE0             JMP RAX
{% endcodeblock %}

FFI from SBCL
============

We finally almost have enough for interesting demos.  The only
important thing that's still missing is calls from CL into the VM.
I'll assume that the caller takes care of saving any important
register, and that the primop (`rsi`) and virtual IP (`rdi`) registers
are setup correctly.  The stack will be filled on entry, by copying
values from the buffer `rax` points to, and written back on exit.

{% codeblock %}
(defun enter ()
  (inst push sb-vm::rbp-tn)
  (inst mov sb-vm::rbp-tn sb-vm::rsp-tn) ; setup a normal frame
  (inst push *rax*) ; stash rax
  (dotimes (i 8)    ; copy the stack in
    (inst mov (@ i) (make-ea :dword :base *rax* :disp (* 4 i))))
  (next))

(defun leave ()
  ;; restore RAX
  (inst mov *rax* (make-ea :qword :base sb-vm::rbp-tn :disp -8))
  ;; overwrite the output stack
  (dotimes (i 8)
    (inst mov (make-ea :dword :base *rax* :disp (* 4 i))
              (@ i)))
  ;; and unwind the frame
  (inst mov sb-vm::rsp-tn sb-vm::rbp-tn)
  (inst pop sb-vm::rbp-tn)
  (inst ret))
{% endcodeblock %}

The CL-side interlocutor of `enter` follows, as a VOP:
{% codeblock %}
(sb-c:defknown %enter-vm (system-area-pointer system-area-pointer system-area-pointer) (values)
    (sb-c:any) :overwrite-fndb-silently t)

(in-package "SB-VM")
(sb-vm::define-vop (cl-user::%enter-vm)
  (:translate cl-user::%enter-vm)
  (:policy :fast-safe)
  (:args (stack :scs (sap-reg) :target rax)
         (code :scs (sap-reg) :target rdi)
         (primitives :scs (sap-reg) :target rsi))
  (:arg-types system-area-pointer system-area-pointer system-area-pointer)
  (:results)
  (:temporary (:sc sap-reg :offset rax-offset :from (:argument 0)) rax)
  (:temporary (:sc sap-reg :offset rbx-offset :from :eval) rbx)
  (:temporary (:sc sap-reg :offset rcx-offset :from :eval) rcx)
  (:temporary (:sc sap-reg :offset rdx-offset :from :eval) rdx)
  (:temporary (:sc sap-reg :offset rdi-offset :from (:argument 1)) rdi)
  (:temporary (:sc sap-reg :offset rsi-offset :from (:argument 2)) rsi)
  (:ignore rbx rcx rdx)
  (:generator 0
    (inst push r8-tn)  ;; the stack was just too painful to declare
    (inst push r9-tn)  ;; as temporary... and it overwrites the
    (inst push r10-tn) ;; thread-base TN, which regalloc disregards.
    (inst push r11-tn)
    (inst push r12-tn)
    (inst push r13-tn)
    (inst push r14-tn)
    (inst push r15-tn)
    
    (move rax stack)
    (move rdi code)
    (move rsi primitives)
    (inst call rsi)   ;; assume the code page starts with ENTER

    (inst pop r15-tn)
    (inst pop r14-tn)
    (inst pop r13-tn)
    (inst pop r12-tn)
    (inst pop r11-tn)
    (inst pop r10-tn)
    (inst pop r9-tn)
    (inst pop r8-tn)))
(in-package "CL-USER")

(defun %enter-vm (stack-sap bytecode-sap primitives-sap)
  (declare (type system-area-pointer stack-sap bytecode-sap primitives-sap))
  (%enter-vm stack-sap bytecode-sap primitives-sap))

(defun vm (stack-designator bytecode primitives)
  (declare (type (simple-array (unsigned-byte 32) 1) bytecode)
           (type system-area-pointer primitives))
  ;; initialise the stack with the designator.
  (let ((stack (make-array 8 :element-type '(unsigned-byte 32)
                             :initial-element 0)))
    (if (typep stack-designator 'sequence)
        (map-into stack (lambda (x)
                          (ldb (byte 32 0) x))
                  stack-designator)
        (fill stack (ldb (byte 32 0) stack-designator)))
    (sb-sys:with-pinned-objects (stack bytecode)
      (time (%enter-vm (sb-sys:vector-sap stack) (sb-sys:vector-sap bytecode)
                       primitives)))
    stack))
{% endcodeblock %}

The only thing missing is to store the machine code for our primop in
a range of memory that's executable.

{% codeblock %}
;; first, get an executable range of memory
(defvar *code-page* (sb-posix:mmap nil (* (length *stack*) *primitive-code-offset*)
                                   (logior sb-posix:prot-read sb-posix:prot-write sb-posix:prot-exec)
                                   (logior sb-posix:map-anon sb-posix:map-private)
                                   -1 0))

;; our list of primop names
(defvar *primops* '(enter leave
                    swap dup drop
                    add sub
                    jmp
                    call ret))

(defun assemble-primops ()
  (multiple-value-bind (offsets pages)
      (apply 'emit-all-code *primops*)
    (loop for page in pages
          for offset upfrom 0 by *primitive-code-offset*
          do (sb-kernel:copy-ub8-to-system-area
              (code-page-code page) 0
              *code-page* offset
              *primitive-code-offset*))
    (mapcar 'cons *primops* offsets)))

;; this alist maps primop names to offsets
(defparameter *primops-offsets* (assemble-primops))
{% endcodeblock %}

{% codeblock %}
CL-USER> *primops-offsets*
((ENTER . 0) (LEAVE . 64) (SWAP . 112) (DUP . 144) (DROP . 176) (ADD . 208)
 (SUB . 240) (JMP . 272) (CALL . 304) (RET . 336))
{% endcodeblock %}

Let's execute `add sub` (and finish with `leave`).
{% codeblock %}
CL-USER> (vm '(3 2 10) (coerce '(208 240 64) '(simple-array (unsigned-byte 32) 1)) *code-page*)
Evaluation took:
  0.000 seconds of real time
  0.000003 seconds of total run time (0.000002 user, 0.000001 system)
  100.00% CPU
  2,288 processor cycles
  0 bytes consed
  
#(5 0 0 0 0 0 3 5)
{% endcodeblock %}

And, indeed, `10 - (3 + 2) = 5`.

We should also test function calls
{% codeblock %}
CL-USER> (vm '(3 2 10) (coerce '(304 8 ; call F
                                 240
                                 64
                                 ;; F:
                                 208
                                 336)
                               '(simple-array (unsigned-byte 32) 1))
             *code-page*)
Evaluation took:
  0.000 seconds of real time
  0.000005 seconds of total run time (0.000003 user, 0.000002 system)
  100.00% CPU
  2,640 processor cycles
  0 bytes consed
  
#(5 0 0 0 0 0 3 5)
{% endcodeblock %}

Instead of executing `add` directly, this bytecode sequence calls to
whatever is 8 bytes (2 dwords) after the call instruction; in our
case, `add ret`.

Writing bytecode by hand is annoying.  This tiny functions takes care
of the stupid stuff.

{% codeblock %}
(defun assemble (opcodes)
  (map '(simple-array (unsigned-byte 32) 1)
       (lambda (opcode)
         (if (integerp opcode)
             (ldb (byte 32 0) opcode)
             (cdr (assoc opcode *primops-offsets*))))
       opcodes))
{% endcodeblock %}

We can now write
{% codeblock %}
CL-USER> (vm '(3 2 10) (assemble '(call 8
                                   sub
                                   leave
                                   
                                   add
                                   ret))
             *code-page*)
{% endcodeblock %}

Conditionals
============

We can now either write (basically) straightline code or infinite
loops.  We need conditionals.  Their implementation is much like
`jmp`, with a tiny twist.  Let's start with jump if (top of stack is)
non-zero and jump if zero.

{% codeblock %}
(defun jcc (cc)
  ;; next word is the offset if the condition is met, otherwise,
  ;; fall through.
  (inst movsx *rax* (make-ea :dword :base *virtual-ip*))
  (inst lea *rax* (make-ea :dword :base *virtual-ip* :index *rax*
                                  :disp 4))
  (inst add *virtual-ip* 4)
  (inst test (@ 0) (@ 0))
  ;; update *virtual-ip* only if zero/non-zero
  (inst cmov cc *virtual-ip* *rax*)
  (next))

(defun jnz ()
  (jcc :nz))

(defun jz ()
  (jcc :z))
{% endcodeblock %}

Immediates
==========

It's hard to write programs without immediate values.  Earlier control
flow primitives already encode immediate data in the virtual
instruction stream.  We'll do the same for `lit`, `inc`, and `dec`:

{% codeblock %}
(defun lit ()
  (decf *stack-pointer*) ; grow the stack
  (inst mov (@ 0) (make-ea :dword :base *virtual-ip*)) ; load the next word
  (next 4)) ; and skip to the next instruction

(defun inc ()
  (inst add (@ 0) (make-ea :dword :base *virtual-ip*))
  (next 4))

(defun dec ()
  (inst sub (@ 0) (make-ea :dword :base *virtual-ip*))
  (next 4))
{% endcodeblock %}

My first loop
=============

Finally, we have enough for a decent-looking (if useless) loop.  First,
update the primop code page:
{% codeblock %}
;; C-M-x to force re-evaluation, or defparameter
(defvar *primops* '(enter leave lit
                    swap dup drop
                    add sub inc dec
                    jmp jnz jz
                    call ret))

(defvar *primops-offsets* (assemble-primops))
{% endcodeblock %}

{% codeblock %}
CL-USER> (vm '(1000000) (assemble '(lit 1 sub jnz -20 leave))
             *code-page*)
Evaluation took:
  0.009 seconds of real time
  0.009464 seconds of total run time (0.009384 user, 0.000080 system)
  100.00% CPU
  14,944,792 processor cycles
  0 bytes consed
  
#(0 0 0 0 0 0 0 1)
{% endcodeblock %}

One million iterations of this stupid loop that only decrements a
counter took 15M cycles. 15 cycles/iteration really isn't that
bad… especially considering that it executes an indirect jump after
loading 1, after subtracting, and after comparing with 0.

We can do better by fusing `lit sub` into `dec`:

{% codeblock %}
CL-USER> (vm '(1000000) (assemble '(dec 1 jnz -16 leave))
             *code-page*)
Evaluation took:
  0.007 seconds of real time
  0.006905 seconds of total run time (0.006848 user, 0.000057 system)
  100.00% CPU
  11,111,128 processor cycles
  0 bytes consed
  
#(0 0 0 0 0 0 0 0)
{% endcodeblock %}

Fuse all the things!
====================

Decrementing a counter and jumping if non zero is a common operation
(old x86 even implemented that in hardware, with `loop`).  Let's add
decrement and jump if non-zero (`djn`) to the VM:

{% codeblock %}
(defun djn ()
  (inst movsx *rax* (make-ea :dword :base *virtual-ip*))
  (inst lea *rax* (make-ea :dword :base *virtual-ip* :index *rax*
                                  :disp 4))
  (inst add *virtual-ip* 4)
  (inst sub (@ 0) 1)
  (inst cmov :nz *virtual-ip* *rax*)
  (next))

(defvar *primops* '(enter leave lit
                    swap dup drop
                    add sub inc dec
                    jmp jnz jz djn
                    call ret))

(defvar *primops-offsets* (assemble-primops))
{% endcodeblock %}

{% codeblock %}
CL-USER> (vm '(1000000) (assemble '(djn -8 leave))
             *code-page*)
Evaluation took:
  0.005 seconds of real time
  0.005575 seconds of total run time (0.005542 user, 0.000033 system)
  120.00% CPU
  8,823,896 processor cycles
  0 bytes consed
  
#(0 0 0 0 0 0 0 0)
{% endcodeblock %}

That's better… But I'm really not convinced by the conditional move.
The branch will usually be predictable, so it makes sense to expose
that to the hardware and duplicate the `NEXT` sequence.

{% codeblock %}
(defun djn2 ()
  (sb-assem:assemble ()
    (inst sub (@ 0) 1)
    (inst jmp :z fallthrough)
    (inst movsx *rax* (make-ea :dword :base *virtual-ip*))
    (inst lea *virtual-ip* (make-ea :dword :base *virtual-ip* :index *rax*
                                           :disp 8))
    (next -4) ; might as well pre-increment *virtual-ip*
    fallthrough ; ASSEMBLE parses for labels, like TAGBODY
    (next 4)))
{% endcodeblock %}

The resulting code isn't too large, and the two indirect jumps are 16
bytes apart.
{% codeblock %}
; Size: 64 bytes
; 00510220:       4183E801         SUB R8D, 1
;       24:       7414             JEQ L0
;       26:       486307           MOVSXD RAX, DWORD PTR [RDI]
;       29:       488D7C0708       LEA RDI, [RDI+RAX+8]
;       2E:       8B043DFCFFFFFF   MOV EAX, [RDI-4]
;       35:       4801F0           ADD RAX, RSI
;       38:       FFE0             JMP RAX
;       3A: L0:   8B043D04000000   MOV EAX, [RDI+4]
;       41:       4883C708         ADD RDI, 8
;       45:       4801F0           ADD RAX, RSI
;       48:       FFE0             JMP RAX
;       4A:       660F1F840000000000 NOP
;       53:       660F1F840000000000 NOP
;       5C:       0F1F4000         NOP
{% endcodeblock %}

This alternative implementation does work better on our stupid loop.
{% codeblock %}
CL-USER> (vm '(1000000) (assemble '(djn2 -8 leave))
             *code-page*)
Evaluation took:
  0.004 seconds of real time
  0.004034 seconds of total run time (0.003913 user, 0.000121 system)
  100.00% CPU
  6,183,488 processor cycles
  0 bytes consed
  
#(0 0 0 0 0 0 0 0)
{% endcodeblock %}

Let's see how that compares to straight assembly code.
{% codeblock %}
(defun ubench ()
  (sb-assem:assemble ()
    head
    (inst sub (@ 0) 1)
    (inst jmp :nz head)
    (next)))
{% endcodeblock %}

{% codeblock %}
CL-USER> (vm '(1000000) (assemble '(ubench leave))
             *code-page*)
Evaluation took:
  0.000 seconds of real time
  0.000629 seconds of total run time (0.000628 user, 0.000001 system)
  100.00% CPU
  1,001,904 processor cycles
  0 bytes consed
  
#(0 0 0 0 0 0 0 0)
{% endcodeblock %}

My slow macbook air gets 1 iteration/cycle on a loop that's 100%
control overhead. With `djn2`, a good implementation of a reasonable
specialised operator, the loop is about 6x as slow as native code.  A
worse implementation of `djn` is still only 8x as slow as pure native
code, and horribly unspecialised bytecode is 11-15x as slow as native
code.

Verdict
=======

Specialising primops to a virtual stack pointer is feasible in
practice, when the stack is restricted to a small size.  It also seems
to have a reasonable runtime overhead for threaded interpreters.  I'm
not actually interested in straight stack languages; however, I
believe that a fixed stack VM makes a nice runtime IR, when coupled
with a mechanism for local variables.  We'll see if I find time to
translate a high level language into superoperators for such a VM.
Fused operators would reduce the importance of `NEXT`; in constrast,
simpler function calls (because there's less shuffling of items to
stack them up in the right position) would remain as useful.

SBCL has definitely proven itself to be a good companion to explore
the generation of domain-specific machine code. I don't know of any
other language implementation with that kind of support for
interactive programming and machine code generation (and inspection).
FWIW, I believe LuaJIT + dynasm will soon be comparable.

Steel Bank Common Lisp: because sometimes C abstracts away too much ;)
