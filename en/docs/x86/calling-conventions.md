---
title: "Calling Conventions"
slug: "calling-conventions"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

# Resources

Overviews/comparisons:  [Agner Fog's nice calling convention guide][2].   Also, [x86 ABIs (wikipedia)][1]: calling conventions for functions, including x86-64 Windows and System V (Linux).  

---

 * [SystemV x86-64 ABI (official standard)][3].  Used by all OSes but Windows.  ([This github wiki page][4], kept up to date by H.J. Lu, has links to 32bit, 64bit, and x32.  Also links to the official forum for ABI maintainers/contributors.)  Also note that [clang/gcc sign/zero extend narrow args to 32bit][5], even though the ABI as written doesn't require it.  Clang-generated code depends on it.
 * [SystemV 32bit (i386) ABI (official standard)](https://01.org/sites/default/files/file_attach/intel386-psabi-1.0.pdf)  , used by Linux and Unix.  ([old version](http://sco.com/developers/devspecs/abi386-4.pdf)).
 * [OS X 32bit x86 calling convention, with links to the others](https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/LowLevelABI/130-IA-32_Function_Calling_Conventions/IA32.html).  The 64bit calling convention is System V.  Apple's site just links to a FreeBSD pdf for that.

 * [Windows x86-64 `__fastcall`](https://msdn.microsoft.com/en-us/library/ms235286.aspx) calling convention
 * [Windows `__vectorcall`](https://msdn.microsoft.com/en-us/library/dn375768.aspx): documents the 32bit and 64bit versions
 * [Windows 32bit `__stdcall`](https://msdn.microsoft.com/en-us/library/zxk0tw93.aspx): used used to call Win32 API functions.  That page links to the other calling convention docs (e.g. `__cdecl`).
 * [Why does Windows64 use a different calling convention from all other OSes on x86-64?](http://stackoverflow.com/questions/4429398/why-does-windows64-use-a-different-calling-convention-from-all-other-oses-on-x86): some interesting history, esp. for the SysV ABI where the mailing list archives are public and go back before AMD's release of first silicon.


  [1]: https://en.wikipedia.org/wiki/X86_calling_conventions
  [2]: http://www.agner.org/optimize/
  [3]: http://web.archive.org/web/20160123183036/http://www.x86-64.org/documentation.html
  [4]: https://github.com/hjl-tools/x86-psABI/wiki/X86-psABI
  [5]: http://stackoverflow.com/questions/36706721/is-a-sign-or-zero-extension-required-when-adding-a-32bit-offset-to-a-pointer-for/36760539#36760539

## 32-bit cdecl
cdecl is a Windows 32-bit function calling convention which is *very* similar to the calling convention used on many POSIX operating systems (documented in the [i386 System V ABI][1]).  One of the differences is in returning small structs.

# Parameters

Parameters are passed on the stack, with the first argument at the lowest address on the stack at the time of the call (pushed last, so it's just above the return address on entry to the function).  The caller is responsible for popping parameters back off the stack after the call.

# Return Value

For scalar return types, the return value is placed in EAX, or EDX:EAX for 64bit integers.  Floating-point types are returned in st0 (x87).  Returning larger types like structures is done by reference, with a pointer passed as an implicit first parameter.  (This pointer is returned in EAX, so the caller doesn't have to remember what it passed).

# Saved and Clobbered Registers

EBX, EDI, ESI, EBP, and ESP (and FP / SSE rounding mode settings) must be preserved by the callee, such that the caller can rely on those registers not having been changed by a call.

All other registers (EAX, ECX, EDX, FLAGS (other than DF), x87 and vector registers) may be freely modified by the callee; if a caller wishes to preserve a value before and after the function call, it must save the value elsewhere (such as in one of the saved registers or on the stack).


  [1]: https://01.org/sites/default/files/file_attach/intel386-psabi-1.0.pdf
  [2]: https://github.com/hjl-tools/x86-psABI/wiki/X86-psABI

## 32-bit stdcall
stdcall is used for 32-bit Windows API calls.

# Parameters

Parameters are passed on the stack, with the first parameter closest to the top of the stack.  The callee will pop these values off of the stack before returning.

# Return Value

Scalar return values are placed in EAX.

# Saved and Clobbered Registers

EAX, ECX, and EDX may be freely modified by the callee, and must be saved by the caller if desired.  EBX, ESI, EDI, and EBP must be saved by the callee if modified and restored to their original values on return.

## 32-bit, cdecl — Dealing with Floating Point
# As parameters (float, double)

Floats are 32 bits in size, they are passed naturally on the stack.  
Doubles are 64 bits in size, they are passed, on the stack, respecting the Little Endian convention<sup>1</sup>,
pushing first the upper 32 bits and than the lower ones.

    //C prototype of callee
    double foo(double a, float b);
    
    foo(3.1457, 0.241);
    
    ;Assembly call
    
    ;3.1457 is 0x40092A64C2F837B5ULL
    ;0.241 is 0x3e76c8b4
    
    push DWORD  3e76c8b4h        ;b, is 32 bits, nothing special here
    push DWORD 0c2f837b5h        ;a, is 64 bits, Higher part of 3.1457
    push DWORD  40092a64h        ;a, is 64 bits, Lower part of 3.1457
    call foo
    add esp, 0ch
    
    ;Call, using the FPU
    ;ST(0) = a, ST(1) = b
    sub esp, 0ch
    fstp QWORD PTR [esp]        ;Storing a as a QWORD on the stack
    fstp DWORD PTR [esp+08h]    ;Storing b as a DWORD on the stack
    call foo
    add esp, 0ch

# As parameters (long double)

Long doubles are 80 bits<sup>2</sup> wide, while on the stack a TBYTE could be stored with
two 32 bits pushes and one 16 bit push (for 4 + 4 + 2 = 10), to keep the stack aligned on
4 bytes, it ends occupying 12 bytes, thus using three 32 bits pushes.  
Respecting Little Endian convention, bits 79-64 are pushed first<sup>3</sup>, then bits 63-32
followed by bits 31-0.



    //C prototype of the callee
    void  __attribute__((cdecl)) foo(long double a);
    
    foo(3.1457);
    
    ;Call to foo in assembly
    ;3.1457 is 0x4000c9532617c1bda800
    
    push DWORD 4000h        ;Bits 79-64, as 32 bits push
    push DWORD 0c9532617h        ;Bits 63-32
    push DWORD 0c1bda800h        ;Bits 31-0
    call foo
    add esp, 0ch
    
    ;Call to foo, using the FPU
    ;ST(0) = a
    
    sub esp, 0ch
    fstp TBYTE PTR [esp]        ;Store a as ten byte on the stack
    call foo
    add esp, 0ch

# As return value

A floating point values, whatever its size, is returned in `ST(0)`<sup>4</sup>.  

    //C
    float one() { return 1; }
    
    ;Assembly
    fld1            ;ST(0) = 1
    ret
    
    //C
    double zero() { return 0; }
    
    ;Assembly
    fldz            ;ST(0) = 0
    ret
    
    //C
    long double pi() { return PI; }
    
    ;Assembly
    fldpi            ;ST(0) = PI
    ret

---

<sup>1</sup> Lower DWORD at lower address.
 
<sup>2</sup> Known as TBYTE, from Ten Bytes.  

<sup>3</sup> Using a full width push with any extension, higher WORD is not used. 

<sup>4</sup> Which is TBYE wide, note that contrary to the integers, FP are always
returned with more precision that it is required.



## 32-bit, cdecl — Dealing with Integers
# As parameters (8, 16, 32 bits)

8, 16, 32 bits integers are always passed, on the stack, as full width 32 bits values<sup>1</sup>.   
No extension, signed or zeroed, is needed.  
The callee will just use the lower part of
the full width values.

    //C prototype of the callee
    void  __attribute__((cdecl)) foo(char a, short b, int c, long d);
    
    foo(-1, 2, -3, 4);
    
    
    ;Call to foo in assembly
    
    push DWORD 4             ;d, long is 32 bits, nothing special here
    push DWORD 0fffffffdh    ;c, int is 32 bits, nothing special here
    push DWORD 0badb0002h    ;b, short is 16 bits, higher WORD can be any value
    push DWORD 0badbadffh    ;a, char is 8 bits, higher three bytes can be any value
    call foo
    add esp, 10h             ;Clean up the stack

# As parameters (64 bits)

64 bits values are passed on the stack using two pushes, respecting the littel endian
convention<sup>2</sup>, pushing first the higher 32 bits then the lower ones.

    //C prototype of the callee
    void  __attribute__((cdecl)) foo(char a, short b, int c, long d);
    
    foo(0x0123456789abcdefLL);
    
    ;Call to foo in assembly
    
    push DWORD 89abcdefh        ;Higher DWORD of 0123456789abcdef
    push DWORD 01234567h        ;Lower DWORD of 0123456789abcdef
    call foo
    add esp, 08h

---

# As return value

8 bits integers are returned in `AL`, eventually clobbering the whole `eax`.  
16 bits integers are returned in `AX`, eventually clobbering the whole `eax`.   
32 bits integers are returned in `EAX`.  
64 bits integers are returned in `EDX:EAX`, where `EAX` holds the lower 32 bits and `EDX` the upper ones. 

    //C
    char foo() { return -1; }
    
    ;Assembly
    mov al, 0ffh
    ret
    
    //C
    unsigned short foo() { return 2; }
    
    ;Assembly
    mov ax, 2
    ret
    
    //C
    int foo() { return -3; }
    
    ;Assembly
    mov eax, 0fffffffdh
    ret
    
    //C
    int foo() { return 4; }
    
    ;Assembly
    xor edx, edx            ;EDX = 0
    mov eax, 4            ;EAX = 4
    ret

---



<sup>1</sup> This keep the stack aligned on 4 bytes, the natural word size.
Also an x86 CPU can only push 2 or 4 bytes when not in long mode. 

<sup>2</sup> Lower DWORD at lower address
 





## 64-bit Windows
# Parameters

The first 4 parameters are passed in (in order) RCX, RDX, R8 and R9. 
XMM0 to XMM3 are used to pass floating point parameters.

Any further parameters are passed on the stack.

Parameters larger than 64bit are passed by address.

**Spill Space**

Even if the function uses less than 4 parameters the caller always provides space for 4 QWORD sized parameters on the stack. The callee is free to use them for any purpose, it is common to copy the parameters there if they would be spilled by another call.

# Return Value

For scalar return types, the return value is placed in RAX. If the return type is larger than 64bits (e.g. for structures) RAX is a pointer to that.

# Saved and Clobbered Registers

All registers used in parameter passing (RCX, RDX, R8, R9 and XMM0 to XMM3), RAX, R10, R11, XMM4 and XMM5 can be spilled by the callee. All other registers need to be preserved by the caller (e.g. on the stack).

# Stack alignment 

The stack must be kept 16-byte aligned. Since the "call" instruction pushes an 8-byte return address, this means that every non-leaf function is going to adjust the stack by a value of the form 16n+8 in order to restore 16-byte alignment.  
It is the callers job to clean the stack after a call.

---
Source: [The history of calling conventions, part 5: amd64](https://blogs.msdn.microsoft.com/oldnewthing/20040114-00/?p=41053/) Raymond Chen

## 32-bit, cdecl — Dealing with Structs
# Padding

Remember, members of a struct are usually padded to ensure they are aligned on their natural boundary:

<!-- language: lang-c -->

    struct t
    {
        int a, b, c, d;    // a is at offset 0, b at 4, c at 8, d at 0ch
        char e;            // e is at 10h
        short f;           // f is at 12h (naturally aligned)
        long g;            // g is at 14h
        char h;            // h is at 18h
        long i;            // i is at 1ch (naturally aligned)
    };


# As parameters (pass by reference)

When passed by reference, a pointer to the struct in memory is passed as the first argument on the stack. This is equivalent to passing a natural-sized (32-bit) integer value; see https://www.wikiod.com/x86/calling-conventions#32-bit cdecl for specifics.

# As parameters (pass by value)

When passed by value, structs are entirely copied on the stack, respecting the original memory layout (*i.e.*, the first member will be at the lower address). 

<!-- language: lang-c -->

    int __attribute__((cdecl)) foo(struct t a);
    
    struct t s = {0, -1, 2, -3, -4, 5, -6, 7, -8};
    foo(s);

<!-- language: lang-asm -->    

    ; Assembly call
    
    push DWORD 0fffffff8h    ; i (-8)
    push DWORD 0badbad07h    ; h (7), pushed as DWORD to naturally align i, upper bytes can be garbage
    push DWORD 0fffffffah    ; g (-6)
    push WORD 5              ; f (5)
    push WORD 033fch         ; e (-4), pushed as WORD to naturally align f, upper byte can be garbage
    push DWORD 0fffffffdh    ; d (-3)
    push DWORD 2             ; c (2)
    push DWORD 0ffffffffh    ; b (-1)
    push DWORD 0             ; a (0)
    call foo
    add esp, 20h

# As return value

Unless they are trivial<sup>1</sup>, structs are copied into a caller-supplied buffer before returning. This is equivalent to having an hidden first parameter `struct S *retval` (where `struct S` is the type of the struct).

The function must return with this pointer to the return value in `eax`; The caller is allowed to depend on `eax` holding the pointer to the return value, which it pushed right before the `call`.

<!-- language: lang-c -->

    struct S
    {
        unsigned char a, b, c;
    };
    
    struct S foo();         // compiled as struct S* foo(struct S* _out)

The hidden parameter is not added to the parameter count for the purposes of stack clean-up, since it must be handled by the callee.

<!-- language: lang-asm -->

    sub esp, 04h        ; allocate space for the struct

    ; call to foo
    push esp            ; pointer to the output buffer
    call foo
    add esp, 00h        ; still as no parameters have been passed

In the example above, the structure will be saved at the top of the stack.

<!-- language: lang-c -->

    struct S foo()
    {
        struct S s;
        s.a = 1; s.b = -2; s.c = 3;
        return s;
    }

<!-- language: lang-asm -->    

    ; Assembly code
    push ebx
    mov eax, DWORD PTR [esp+08h]   ; access hidden parameter, it is a pointer to a buffer
    mov ebx, 03fe01h               ; struct value, can be held in a register
    mov DWORD [eax], ebx           ; copy the structure into the output buffer 
    pop ebx
    ret 04h                        ; remove the hidden parameter from the stack
                                   ; EAX = pointer to the output buffer

---

<sup>1</sup> A "trivial" struct is one that contains only one member of a non-struct, non-array type (up to 32 bits in size). For such structs, the value of that member is simply returned in the `eax` register. (This behavior has been observed with GCC targeting Linux)

The Windows version of cdecl is different from the System V ABI's calling convention:  A "trivial" struct is allowed to contain up to two members of a non-struct, non-array type (up to 32 bits in size). These values are returned in `eax` and `edx`, just like a 64-bit integer would be. (This behavior has been observed for MSVC and Clang targeting Win32.)

## 64-bit System V
This is the default calling convention for 64-bit applications on many POSIX operating systems.

# Parameters

The first eight scalar parameters are passed in (in order) RDI, RSI, RDX, RCX, R8, R9, R10, R11.  Parameters past the first eight are placed on the stack, with earlier parameters closer to the top of the stack.  The caller is responsible for popping these values off the stack after the call if no longer needed.

# Return Value

For scalar return types, the return value is placed in RAX.  Returning larger types like structures is done by conceptually changing the signature of the function to add a parameter at the beginning of the parameter list that is a pointer to a location in which to place the return value.

# Saved and Clobbered Registers

RBP, RBX, and R12–R15 are preserved by the callee.  All other registers may be modified by the callee, and the caller must preserve a register’s value itself (e.g. on the stack) if it wishes to use that value later.

