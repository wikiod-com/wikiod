---
title: "Optimization"
slug: "optimization"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

The x86 family has been around for a long time, and as such there are many tricks and techniques that have been discovered and developed that are public knowledge - or maybe not so public.

Most of these tricks take advantage of the fact that many instructions effectively do the same thing - but different versions are quicker, or save memory, or don't affect the Flags.

Herein are a number of tricks that have been discovered. Each have their Pros and Cons, so should be listed.

When in doubt, you can always refer to the pretty comprehensive [Intel 64 and IA-32 Architectures Optimization Reference Manual][1], which is a great resource from the company behind the x86 architecture itsself.


  [1]: http://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-optimization-manual.pdf

## Zeroing a register
The obvious way to zero a register is to `MOV` in a `0`—for example:

    B8 00 00 00 00    MOV eax, 0

Notice that this is a 5-byte instruction.

If you are willing to clobber the flags (`MOV` never affects the flags), you can use the `XOR` instruction to bitwise-XOR the register with itself:

    33 C0             XOR eax, eax

This instruction requires only 2 bytes and [executes faster on all processors](http://stackoverflow.com/questions/33666617/what-is-the-best-way-to-set-a-register-to-zero-in-x86-assembly-xor-mov-or-and/33668295#33668295).

## Test a register for 0
# Background
To find out if a register holds a zero, the naïve technique is to do this:

        cmp   eax, 0

But if you look at the opcode for this, you get this:

    83 F8 00      cmp   eax, 0

# Use `test`

        test   eax, eax      ; Equal to zero?

Examine the opcode you get:

    85 c0         test   eax, eax

## Pros
* Only two bytes!

## Cons
* Opaque to a reader unfamiliar with the technique

You can also have a look into the [Q&A Question on this technique][1].


  [1]: http://stackoverflow.com/questions/33721204/test-whether-a-register-is-zero-with-cmp-reg-0-vs-or-reg-reg

## Moving Carry flag into a register
# Background

If the Carry (`C`) flag holds a value that you want to put into a register, the naïve way is to do something like this:

        mov  al, 1
        jc   NotZero
        mov  al, 0
    NotZero:

# Use 'sbb'

A more direct way, avoiding the jump, is to use "Subtract with Borrow":

        sbb  al,al    ; Move Carry to al

If `C` is zero, then `al` will be zero. Otherwise it will be `0xFF` (`-1`). If you need it to be `0x01`, add:

        and  al, 0x01 ; Mask down to 1 or 0

## Pros
* About the same size
* Two or one fewer instructions
* No expensive jump

## Cons
* It's opaque to a reader unfamiliar with the technique
* It alters other Flags



## Linux system calls with less bloat
In 32-bit Linux, system calls are usually done by using the sysenter instruction (I say usually because older programs use the now deprecated `int 0x80`) however, this can take up quite alot of space in a program and so there are ways that one can cut corners in order to shorten and speed things up.  
This is usually the layout of a system call on 32-bit Linux:  

    mov eax, <System call number>
    mov ebx, <Argument 1> ;If applicable
    mov ecx, <Argument 2> ;If applicable
    mov edx, <Argument 3> ;If applicable
    push <label to jump to after the syscall>
    push ecx
    push edx
    push ebp
    mov ebp, esp
    sysenter

That's massive right! But there are a few tricks we can pull to avoid this mess.  
The first is to set ebp to the value of esp decreased by the size of 3 32-bit registers, that is, 12 bytes. This is great so long as you are ok with overwriting ebp, edx and ecx with garbage (such as when you will be moving a value into those registers directly after anyway), we can do this using the LEA instruction so that we do not need to affect the value of ESP itself.  

    mov eax, <System call number>
    mov ebx, <Argument 1>
    mov ecx, <Argument 2>
    mov edx, <Argument 3>
    push <label to jump to after the syscall>
    lea ebp, [esp-12]
    sysenter

However, we're not done, if the system call is sys_exit we can get away with not pushing anything at all to the stack!

    mov eax, 1
    xor ebx, ebx ;Set the exit status to 0
    mov ebp, esp
    sysenter



## Multiply by 3 or 5
# Background

To get the product of a register and a constant and store it in another register, the naïve way is to do this:

        imul ecx, 3      ; Set ecx to 5 times its previous value
        imul edx, eax, 5 ; Store 5 times the contend of eax in edx

# Use `lea`

Multiplications are expensive operations. It's faster to use a combination of shifts and adds. For the particular case of muliplying the contend of a 32 or 64 bit register that isn't `esp` or `rsp` by 3 or 5, you can use the lea instruction. This uses the address calculation circuit to calculate the product quickly.

        lea ecx, [2*ecx+ecx] ; Load 2*ecx+ecx = 3*ecx into ecx
        lea edx, [4*edx+edx] ; Load 4*edx+edx = 5*edx into edx

Many assemblers will also understand

        lea ecx, [3*ecx]
        lea edx, [5*edx]

For all possible multiplicands other them `ebp` or `rbp`, the resulting instruction lengh is the same as with using `imul`.

## Pros
* Executes much faster

## Cons
* If your multiplicand is `ebp` or `rbp` it takes one byte more them using `imul`
* More to type if your assembler dosn't support the shortcuts
* Opaque to a reader unfamiliar with the technique



