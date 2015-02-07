---
layout: post
title: "On the importance of keeping microbenchmarks honest"
date: 2012-08-23 13:01
comments: true
categories: 
---

[A recent post](http://tapoueh.org/blog/2012/08/20-performance-the-easiest-way.html) tried to extract information from a microbenchmark, but the author
absolutely did not care whether the programs computed the right, or
even the same, thing.

The result? Pure noise.

`(expt 10 10)` overflows 32 bit *signed* integers, so the C version
wound up going through 1410065408 iterations instead.  In fact, signed
overflow is undefined in C, so a sufficiently devious compiler could
cap the iteration count to 65536 and still be standard compliant.

On SBCL/x86-64, we can do the following and explicitly ask for machine
unsigned arithmetic:

    CL-USER> (lambda (max)
               (declare (type (unsigned-byte 64) max)
                        (optimize speed))
               (let ((sum 0))
                 (declare (type (unsigned-byte 64) sum))
                 (dotimes (i max sum)
                   (setf sum (ldb (byte 64 0) (+ sum i))))))
    #<FUNCTION (LAMBDA (MAX)) {1004DA3D6B}>
    CL-USER> (disassemble *)
    ; disassembly for (LAMBDA (MAX))
    ; 04DA3E02:       31C9             XOR ECX, ECX               ; no-arg-parsing entry point
    ;       04:       31C0             XOR EAX, EAX
    ;       06:       EB0E             JMP L1
    ;       08:       0F1F840000000000 NOP
    ;       10: L0:   4801C1           ADD RCX, RAX
    ;       13:       48FFC0           INC RAX
    ;       16: L1:   4839D0           CMP RAX, RDX
    ;       19:       72F5             JB L0
    [ function epilogue ]
    
Now that `ldb` *portably* ensures modular arithmetic, we
virtually get the exact same thing as what GCC outputs, down to
alignment.  It's still slower than the C version because it goes
through 1e10 iterations of the lossy sum, rather than
1.4e9.

Microbenchmarks are useful to improve our understanding of complex
systems.  Microbenchmarks whose results we completely discard not so
much: if there's nothing keeping us or the compiler honest, we might
as well get them to compile to no-ops.
