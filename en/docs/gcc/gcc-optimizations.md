---
title: "GCC Optimizations"
slug: "gcc-optimizations"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The GNU compiler offers various levels of optimizations for the compilation process. These optimizations are used to improve the code performance and/or code size. Compiling a code with optimizations on, typically takes longer to complete.

This command tells you what optimizations are available on your system : 
      $gcc -Q --help=optimizations

Here is a detailed documentation of options to control optimizations:

https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html


## Differnce between codes compiled with O0 and O3
I wrote a simple C code foo.c

    int main()
    {
        int i = 0;
        int j = 0;
        for (i = 0; i < 5; i++) {
                j = i + 1;
        }
        return 0;
    }

When compiled with -O0 i.e. by disabling all compiler optimizations

    $ gcc -o foo.S foo.c -O0 -S

I got this:

        .file   "foo.c"
        .text
        .globl  main
        .type   main, @function
    main:
    .LFB0:
        .cfi_startproc
        pushq   %rbp
        .cfi_def_cfa_offset 16
        .cfi_offset 6, -16 
        movq    %rsp, %rbp
        .cfi_def_cfa_register 6
        movl    $0, -4(%rbp)
        movl    $0, -8(%rbp)
        movl    $0, -4(%rbp)
        jmp     .L2
    .L3:
        movl    -4(%rbp), %eax
        addl    $1, %eax
        movl    %eax, -8(%rbp)
        addl    $1, -4(%rbp)
    .L2:
        cmpl    $4, -4(%rbp)
        jle     .L3 
        movl    $0, %eax
        popq    %rbp
        .cfi_def_cfa 7, 8
        ret
        .cfi_endproc
    .LFE0:
        .size   main, .-main
        .ident  "GCC: (GNU) 6.2.0"
        .section        .note.GNU-stack,"",@progbits

GCC took all the pain to convert my code into assembly language verbatim.

But when I compiled my code with O3 i.e. with the highest level of optimizations

I got this:

    .file   "foo.c"
    .section        .text.startup,"ax",@progbits
    .p2align 4,,15
    .globl  main
    .type   main, @function
    main:
    .LFB11:
        .cfi_startproc
        xorl    %eax, %eax
        ret
        .cfi_endproc
    .LFE11:
        .size   main, .-main
        .ident  "GCC: (GNU) 6.2.0"
        .section        .note.GNU-stack,"",@progbits
    
GCC understood that I was just doodling and doing nothing important with the variables and the loop. So it left me a blank stub with no code.

DAYUM!





