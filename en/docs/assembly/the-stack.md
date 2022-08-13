---
title: "The Stack"
slug: "the-stack"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

The stack of computers is like a stack of books. _PUSH_ adds one to the top and _POP_ takes the uppermost away. Like in real life the stack cannot be endless, so it has maximum size. The stack can be used for sorting algorithms, to handle a bigger amount of data or to safe values of registers while doing another operation.

## Zilog Z80 Stack
The register `sp` is used as *stack pointer*, pointing to the last stored value into stack ("top" of stack). So `EX (sp),hl` will exchange value of `hl` with the value on top of stack.

Contrary to "top" word, the stack grows in memory by decreasing the `sp`, and releases ("pops") values by increasing `sp`.

For `sp` = `$4844` with values `1`, `2`, `3` stored on stack (the `3` being pushed onto stack as last value, so being at top of it), the memory will look like this:

    |    address | value bytes | comment (btw, all numbers are in hexadecimal)
    | ---------- | ----------- | ---------------------------------
    |       4840 | ?? ??       | free stack spaces to be used by next push/call
    |       4842 | ?? ??       | or by interrupt call! (don't expect values to stay here)
    | sp -> 4844 | 03 00       | 16 bit value "3" on top of stack
    |       4846 | 02 00       | 16 bit value "2"
    |       4848 | 01 00       | 16 bit value "1"
    |       484A | ?? ??       | Other values in stack (up to it's origin)
    |       484C | ?? ??       | like for example return address for RET instruction

Examples, how instructions work with stack:

        LD   hl,$0506
        EX   (sp),hl           ; $0003 into hl, "06 05" bytes at $4844
        POP  bc                ; like: LD c,(sp); INC sp; LD b,(sp); INC sp
                               ; so bc is now $0506, and sp is $4846
        XOR  a                 ; a = 0, sets zero and parity flags
        PUSH af                ; like: DEC sp; LD (sp),a; DEC sp; LD (sp),f
                               ; so at $4844 is $0044 (44 = z+p flags), sp is $4844
        CALL $8000             ; sp is $4842, with address of next ins at top of stack
                               ; pc = $8000 (jumping to sub-routine)
                               ; after RET will return here, the sp will be $4844 again
        LD   (L1+1),sp         ; stores current sp into LD sp,nn instruction (self modification)
        DEC  sp                ; sp is $4843
    L1  LD   sp,$1234          ; restores sp to $4844 ($1234 was modified)
        POP  de                ; de = $0044, sp = $4846
        POP  ix                ; ix = $0002, sp = $4848
        ...

        ...
        ORG  $8000
        RET                    ; LD pc,(sp); INC sp; INC sp
                               ; jumps to address at top of stack, "returning" to caller

*Summary*: `PUSH` will store value on top of stack, `POP` will fetch value from top of stack, it's a *LIFO* (last in, first out) queue. `CALL` is same as `JP`, but it also pushes address of next instruction after `CALL` at top of stack. `RET` is similar to `JP` also, popping the address from stack and jumping to it.

*Warning*: when interrupts are enabled, the `sp` must be valid during interrupt signal, with enough free space reserved for interrupt handler routine, as the interrupt signal will store the return address (actual `pc`) before calling handler routine, which may store further data on stack as well. Any value ahead of `sp` may be thus modified "unexpectedly", if interrupt happens.

*Advanced trick*: on Z80 with `PUSH` taking 11 clock cycles (11t) and `POP` taking 10t, the unrolled `POP`/`PUSH` trough all registers, including `EXX` for shadow variants, was the fastest way to copy block of memory, even faster than unrolled `LDI`. But you had to time the copy in between interrupt signals to avoid memory corruption. Also unrolled `PUSH` was the fastest way to fill memory with particular value on ZX Spectrum (again with the risk of corruption by Interrupt, if not timed properly, or done under `DI`).

