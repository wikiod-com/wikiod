---
title: "Assemblers"
slug: "assemblers"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Netwide Assembler - NASM
NASM is by far the most ported assembler for the x86 architecture - it's available for practically every Operating System based on the x86 (even being included with MacOS), and is available as a cross-platform assembler on other platforms.

This assembler uses Intel syntax, but it is different from others because it focuses heavily on its own "macro" language - this permits the programmer to build up more complex expressions using simpler definitions, allowing new "instructions" to be created.

Unfortunately this powerful feature comes at a cost: the type of the data gets in the way of generalised instructions, so data typing is not enforced.

    response:    db       'Y'     ; Character that user typed

                 cmp      response, 'N' ; *** Error! Unknown size!
                 cmp byte response, 'N' ; That's better!
                 cmp      response, ax  ; No error!

However, NASM introduced one feature that others lacked: scoped symbol names. When you define a symbol in other assemblers, that name is available throughout the rest of the code - but that "uses up" that name, "polluting" the global name space with symbols.

For example (using NASM syntax):

           STRUC     Point
    X      resw      1
    Y      resw      1
           ENDSTRUC

After this definition, X and Y are forevermore defined. To avoid "using up" the names `X` and `Y`, you needed to use more definite names:

           STRUC     Point
    Pt_X   resw      1
    Pt_Y   resw      1
           ENDSTRUC

But NASM offers an alternative. By leveraging its "local variable" concept, you can define structure fields that require you to nominate the containing structure in future references:

           STRUC      Point
    .X     resw       1
    .Y     resw       1
           ENDSTRUC
 
    Cursor ISTRUC     Point
           ENDISTRUC
 
           mov        ax,[Cursor+Point.X]
           mov        dx,[Cursor+Point.Y]

Unfortunately, because NASM doesn't keep track of types, you can't use the more natural syntax:

           mov        ax,[Cursor.X]
           mov        dx,[Cursor.Y]



## Microsoft Assembler - MASM
Given that the 8086/8088 was used in the IBM PC, and the Operating System on that was most often from Microsoft, Microsoft's assembler MASM was the de facto standard for many years. It followed Intel's syntax closely, but permitted some convenient but "loose" syntax that (in hindsight) only caused confusion and errors in code.

A perfect example is as follows:

    MaxSize      EQU     16          ; Define a constant
    Symbol       DW      0x1234      ; Define a 16-bit WORD called Symbol to hold 0x1234

                 MOV     AX, 10      ; AX now holds 10
                 MOV     BX, MaxSize ; BX now holds 16
                 MOV     CX, Symbol  ; ????

Does the last `MOV` instruction put the _contents_ of `Symbol` into `CX`, or the _address_ of `Symbol` into `CX`? Does `CX` end up with `0x1234` or `0x0102` (or whatever)? It turns out that `CX` ends up with `0x1234` - if you want the address, you need to use the `OFFSET` specifier

                 MOV     AX, [Symbol]      ; Contents of Symbol
                 MOV     CX, OFFSET Symbol ; Address of Symbol



## AT&T assembler - as
Although the 8086 was most used in IBM PCs along with Microsoft, there were a number of other computers and Operating Systems that used it too: most notably Unix. That was a product of AT&T, and it already had Unix running on a number of other architectures. Those architectures used more conventional assembly syntax - especially that two-operand instructions specified them in `source`, `dest` order.

So AT&T assembler conventions overrode the conventions dictated by Intel, and a whole new dialect was introduced for the x86 range:

* Register names were prefixed by `%`:  
`%al`, `%bx` etc.
* Immediate values were prefied by `$`:  
`$4`
* Operands were in `source`, `dest` order
* Opcodes included their operand sizes:  
`movw $4, %ax  ; Move word 4 into AX`


## Intel Assembler
Intel wrote the specification of the 8086 assembly language, a derivative of the earlier 8080, 8008 and 4004 processors. As such, the assembler they wrote followed their own syntax precisely. However, this assembler wasn't used very widely.

Intel defined their opcodes to have either zero, one or two operands. The two-operand instructions were defined to be in the `dest`, `source` order, which was different from other assemblers at the time. But some instructions used implicit registers as operands - you just had to know what they were. Intel also used the concept of "prefix" opcodes - one opcode would affect the next instruction.

    ; Zero operand examples
    NOP             ; No parameters
    CBW             ; Convert byte in AL into word in AX
    MOVSB           ; Move byte pointed to by DS:SI to byte pointed to by ES:DI
                    ; SI and DI are incremented or decremented according to D bit

    ; Prefix examples
    REP   MOVSB     ; Move number of bytes in CX from DS:SI to ES:DI
                    ; SI and DI are incremented or decremented according to D bit

    ; One operand examples
    NOT      AX     ; Replace AX with its one's complement
    MUL      CX     ; Multiply AX by CX and put 32-bit result in DX:AX

    ; Two operand examples
    MOV      AL, [0x1234] ; Copy the contents of memory location DS:0x1234 into AL register

Intel also broke a convention used by other assemblers: for each opcode, a different mnemonic was invented. This required subtly- or distinctly-different names for similar operations: e.g. `LDM` for "Load from Memory" and `LDI` for "Load Immediate". Intel used the one mnemonic `MOV` - and expected the assembler to work out which opcode to use from context. That caused many pitfalls and errors for programmers in the future when the assembler couldn't intuit what the programmer actually wanted...

## Borland's Turbo Assembler - TASM
Borland started out with a Pascal compiler that they called "Turbo Pascal". This was followed by compilers for other languages: C/C++, Prolog and Fortran. They also produced an assembler called "Turbo Assembler", which, following Microsoft's naming convention, they called "TASM".

TASM tried to fix some of the problems of writing code using MASM (see above), by providing a more strict interpretation of the source code under a specified `IDEAL` mode. By default it assumed `MASM` mode, so it could assemble MASM source directly - but then Borland found that they had to be bug-for-bug compatible with MASM's more "quirky" idiosyncracies - so they also added a `QUIRKS` mode.

Since TASM was (much) cheaper than MASM, it had a large user base - but not many people used IDEAL mode, despite its touted advantages.

## GNU assembler - gas
When the GNU project needed an assembler for the x86 family, they went with the AT&T version (and its syntax) that was associated with Unix rather than the Intel/Microsoft version.

## Yet Another Assembler - YASM
[YASM][1] is a complete rewrite of NASM, but is compatible with both Intel and AT&T syntaxes.

  [1]: https://en.wikipedia.org/wiki/Yasm

