---
title: "Flow Control"
slug: "flow-control"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Every piece of non-trivial software needs flow-control structures to divert program flow according to conditions. 

Assembly being the lowest-level programming language provides only *primitives* for control structures. Typically, machine operations affect *flags* in the CPU, and *conditional branches/jumps* implement the flow control. In assembly, all higher-level control structures need to be constructed from such primitives.



## FOR ... NEXT in Z80 Assembly
The Z80 has a specific instruction to implement loop counts: `DJNZ`standing for "decrement B register and jump if not zero". So, B is the register of choice to implement loops on this processor. FOR...NEXT needs to be implemented "backwards", because the register counts down to zero. Other CPUs (like the 8086, this CPU uses the CX register as loop counter) might have similar specific loop counter registers and instructions, some other CPUs allow loop commands with arbitrary registers (m68k has a DBRA instruction that works with any data register).

    ; Trivial multiplication (by repeated adding, ignores zero in factors, so 
    ; not recommended for general use)
    ;
    ; inputs:    A = Factor 1
    ;            B = Factor 2
    ;
    ; output:    A = Factor 1 * Factor 2
    ;
    ; Pseudo code
    ; C = A : A = 0 : FOR B = Factor 2 DOWNTO 0 : A = A + C : NEXT B

    mul:
         LD    C,A        ; Save Factor 1 in C register
         XOR   A          ; Clear accumulator
    mLoop:
         ADD   A,C        ; Add Factor 1 to accumulator
         DJNZ  mLoop      ; Do this Factor 2 times
         RET              ; return to caller

## Trivial IF-THEN-ELSE in m68k Assembly
    ; IF d0 == 10 GO TO ten, ELSE GO TO other
        CMP    #10,d0        ; compare register contents to immediate value 10
                             ; instruction affects the zero flag
        BEQ    ten           ; branch if zero flag set
    other:
        ; do whatever needs to be done for d0 != 10
        BRA    afterother    ; unconditionally jump across IF case 
    ten:
        ; do whatever needs to be done for d0 == 10
    afterother:
        ; continue normal common program flow

Which instructions are affecting which flags, and which conditional branches (that might also be based on specific *combinations of flags*) are available, depends very much on your chosen CPU and should be looked up in the manuals.

## If-statement in Intel-syntax assembly
    
    section .data
        msg_eq db 'Equal', 10
        len_eq equ $ - msg_eq
    
        msg_le db 'Less than', 10
        len_le equ $ - msg_le
    
        msg_gr db 'Greater than', 10
        len_gr equ $ - msg_gr ; Length of msg_gr
    section .text
        global _main ; Make the _main label global for linker
    _main:
        cmp 4, 5 ; Compare 4 and 5
        je _equal ; je = jump if equal
        jl _less ; jl = jump if less
        jg _greater ; jg = jump if greater
    exit:
        ret ; Return
    _equal:
        ; Whatever code here
        mov rax, 0x2000004 ; sys_write, 4 for linux
        mov rdi, 1 ; STDOUT
        mov rsi, msg_eq
        mov rdi, len_eq
        
        syscall
        
        jmp exit ; Exit
    _less:
        ; Whatever code here
        mov rax, 0x2000004
        mov rdi, 1
        mov rsi, msg_le
        mov rdi, len_le
        
        syscall
        
        jmp exit
    _greater:
        ; Whatever code here
        
        mov rax, 0x2000004
        mov rdi, 1
        mov rsi, msg_gr
        mov rdi, len_gr
        
        syscall
        jmp exit

## Loop while condition is true in Intel syntax assembly
    section .data
        msg db 'Hello, world!', 0xA
        len equ $ - msg
    section .text
    global _main
    _main:
        mov rax, 0 ; This will be the current number
        mov rcx, 10 ; This will be the last number
        
    _loop:
        cmp rax, rcx
        jl .loopbody ; Jump to .loopbody if rax < rcx
        jge _exit ; Jump to _exit if rax ≥ rcx
    .loopbody:
        push rax ; Store the rax value for later use
    
        mov rax, 0x2000004 ; 4 for Linux
        mov rdi, 1 ; STDOUT
        mov rsi, msg
        mov rdx, len
    
        syscall

        pop rax ; Take it back to rax
    
        inc rax ; Add 1 to rax. This is required since the loop must have an ending.    
    
        jmp _loop ; Back to loop
    _exit:
        ret    ; Return
This will execute `.loopbody` as long as `rax < rcx`.

