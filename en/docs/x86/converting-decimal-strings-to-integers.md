---
title: "Converting decimal strings to integers"
slug: "converting-decimal-strings-to-integers"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Converting strings to integers is one of common tasks.

Here we'll show how to convert decimal strings to integers.

Psuedo code to do this is:

    function string_to_integer(str):
        result = 0
        for (each characters in str, left to right):
            result = result * 10
            add ((code of the character) - (code of character 0)) to result
        return result

Dealing with hexadecimal strings is a bit more difficult because character codes are typically not continuous when dealing with multiple character types such as digits(0-9) and alphabets(a-f and A-F).
Character codes are typically continuous when dealing with only one type of characters (we'll deal with digits here), so we'll deal with only environments in which character codes for digit are continuous.


## IA-32 assembly, GAS, cdecl calling convention
    # make this routine available outside this translation unit
    .globl string_to_integer

    string_to_integer:
        # function prologue
        push %ebp
        mov %esp, %ebp
        push %esi

        # initialize result (%eax) to zero
        xor %eax, %eax
        # fetch pointer to the string
        mov 8(%ebp), %esi

        # clear high bits of %ecx to be used in addition
        xor %ecx, %ecx
        # do the conversion
    string_to_integer_loop:
        # fetch a character
        mov (%esi), %cl
        # exit loop when hit to NUL character
        test %cl, %cl
        jz string_to_integer_loop_end
        # multiply the result by 10
        mov $10, %edx
        mul %edx
        # convert the character to number and add it
        sub $'0', %cl
        add %ecx, %eax
        # proceed to next character
        inc %esi
        jmp string_to_integer_loop
    string_to_integer_loop_end:

        # function epilogue
        pop %esi
        leave
        ret

This GAS-style code will convert decimal string given as first argument, which is pushed on the stack before calling this function, to integer and return it via `%eax`.
The value of `%esi` is saved because it is callee-save register and is used.

Overflow/wrapping and invalid characters are not checked in order to make the code simple.

In C, this code can be used like this (assuming `unsigned int` and pointers are 4-byte long):

    #include <stdio.h>

    unsigned int string_to_integer(const char* str);

    int main(void) {
        const char* testcases[] = {
            "0",
            "1",
            "10",
            "12345",
            "1234567890",
            NULL
        };
        const char** data;
        for (data = testcases; *data != NULL; data++) {
            printf("string_to_integer(%s) = %u\n", *data, string_to_integer(*data));
        }
        return 0;
    }

Note: in some environments, two `string_to_integer` in the assembly code have to be changed to `_string_to_integer` (add underscore) in order to let it work with C code.

## MS-DOS, TASM/MASM function to read a 16-bit unsigned integer
Read a 16-bit unsigned integer from input.
------------------------------------------

This function uses the interrupt service [Int 21/AH=0Ah](http://www.ctyme.com/intr/rb-2563.htm) for reading a buffered string.  
The use of a buffered string let the user review what they had typed before passing it to the program for processing.  
Up to six digits are read (as 65535 = 2<sup>16</sup> - 1 has six digits). 

Besides performing the standard conversion from *numeral* to *number* this function also detects invalid input and overflow (number too big to fit 16 bits).

Return values
------------

The function return the number read in `AX`.  The flags `ZF`, `CF`, `OF` tell if the operation completed successfully or not and why.

|Error| AX | ZF | CF |  OF |
| ------ | ------ | ------ | ------ | ------ |
| *None*| The 16-bit integer | Set   | Not Set | Not Set |
| Invalid input | The partially converted number, up to the last valid digit encountered| Not Set   | Set | Not Set |
|Overflow|  7fffh | Not Set   | Set | Set |

The `ZF` can be used to quickly tell valid vs invalid inputs apart.

Usage
------------

    call read_uint16
    jo _handle_overflow            ;Number too big (Optional, the test below will do)
    jnz _handle_invalid            ;Number format is invalid

    ;Here AX is the number read

Code
----

    ;Returns:
      ;
      ;If the number is correctly converted:
      ;   ZF = 1, CF = 0, OF = 0
      ;   AX = number
      ;
      ;If the user input an invalid digit:
      ;   ZF = 0, CF = 1, OF = 0
      ;   AX = Partially converted number
      ;
      ;If the user input a number too big
      ;   ZF = 0, CF = 1, OF = 1
      ;   AX = 07fffh
      ;
      ;ZF/CF can be used to discriminate valid vs invalid inputs
      ;OF can be used to discrimate the invalid inputs (overflow vs invalid digit)
      ;
      read_uint16:
        push bp   
        mov bp, sp
    
        ;This code is an example in Stack Overflow Documentation project.
        ;x86/Converting Decimal strings to integers


        ;Create the buffer structure on the stack
    
        sub sp, 06h                ;Reserve 6 byte on the stack (5 + CR)
        push 0006h                 ;Header
    
        push ds
        push bx
        push cx
        push dx
    
        ;Set DS = SS
    
        mov ax, ss
        mov ds, ax                           
                      
    
        ;Call Int 21/AH=0A
    
        lea dx, [bp-08h]            ;Address of the buffer structure
        mov ah, 0ah
        int 21h
    
        ;Start converting
    
        lea si, [bp-06h]
        xor ax, ax
        mov bx, 10
        xor cx, cx
    
       _r_ui16_convert:
    
        ;Get current char
    
        mov cl, BYTE PTR [si]
        inc si
    
        ;Check if end of string
    
        cmp cl, CR_CHAR
        je _r_ui16_end                      ;ZF = 1, CF = 0, OF = 0
       
        ;Convert char into digit and check
    
        sub cl, '0'
        jb _r_ui16_carry_end                ;ZF = 0, CF = 1, OF = X -> 0
        cmp cl, 9
        ja _r_ui16_carry_end                ;ZF = 0, CF = 0 -> 1, OF = X -> 0
    
    
        ;Update the partial result (taking care of overflow)
    
        ;AX = AX * 10
        mul bx
    
        ;DX:AX = DX:AX + CX
        add ax, cx
        adc dx, 0
           
        test dx, dx
       jz _r_ui16_convert            ;No overflow
       
        ;set OF and CF
        mov ax, 8000h
        dec ax                           
        stc

       jmp _r_ui16_end                      ;ZF = 0, CF = 1, OF = 1
        
       _r_ui16_carry_end:
    
        or bl, 1                ;Clear OF and ZF
        stc                     ;Set carry
     
        ;ZF = 0, CF = 1, OF = 0

       _r_ui16_end:
        ;Don't mess with flags hereafter!
    
        pop dx
        pop cx
        pop bx
        pop ds
    
        mov sp, bp
    
        pop bp
        ret
    
        CR_CHAR EQU 0dh

NASM porting
---------

To port the code to NASM remove the `PTR` keyword from memory accesses (e.g. `mov cl, BYTE PTR [si]` becomes `mov cl, BYTE [si]`)

## MS-DOS, TASM/MASM function to print a 16-bit number in binary, quaternary, octal, hex
Print a number in binary, quaternary, octal, hexadecimal and a general power of two
--

All the bases that are a power of two, like the binary (2<sup>1</sup>), quaternary (2<sup>2</sup>), octal (2<sup>3</sup>), hexadecimal (2<sup>4</sup>) bases, have an integral number of bits per digit<sup>1</sup>.  
Thus to retrieve each digit<sup>2</sup> of a numeral we simply break the number intro group of *n* bits starting from the LSb (the right).  
For example for the quaternary base, we break a 16-bit number in groups of two bits. There are 8 of such groups.  
Not all power of two bases have an integral number of groups that fits 16 bits; for example, the octal base has 5 groups of 3 bits that account for 3·5 = 15 bits out of 16, leaving a partial group of 1 bit<sup>3</sup>.  

The algorithm is simple, we isolate each group with a shift followed by an *AND* operation.  
This procedure works for every size of the groups or, in other words, for any base power of two. 

In order to show the digits in the right order the function start by isolating the most significant group (the leftmost), thereby it is important to know: a) how many bits *D* a group is and b) the bit position *S* where the leftmost group starts.  
These values are precomputed and stored in carefully crafted constants.

Parameters
--

The parameters must be pushed on the stack.  
Each one is 16-bit wide.  
They are shown in order of push.

| Parameter | Description |
| ------ | ------ |
| *N*   | The number to convert   |
| Base  | The base to use expressed using the constants `BASE2`, `BASE4`, `BASE8` and `BASE16` |
| Print leading zeros | If *zero* no non-significant zeros are print, otherwise they are. The number 0 is printed as "0" though |


Usage
--

    push 241
    push BASE16
    push 0
    call print_pow2              ;Prints f1
    
    push 241
    push BASE16
    push 1
    call print_pow2              ;Prints 00f1
    
    push 241
    push BASE2
    push 0
    call print_pow2              ;Prints 11110001

**Note to TASM users**: If you put the constants defined with `EQU` after the code that uses them, enable *multi-pass* with the `/m` flag of *TASM* or you'll get *Forward reference needs override*.

Code
--

    ;Parameters (in order of push):
    ;
    ;number
    ;base (Use constants below)
    ;print leading zeros
    print_pow2:
     push bp
     mov bp, sp
    
     push ax
     push bx
     push cx
     push dx
     push si
     push di
    
     ;Get parameters into the registers
    
     ;SI = Number (left) to convert
     ;CH = Amount of bits to shift for each digit (D)
     ;CL = Amount od bits to shift the number (S)
     ;BX = Bit mask for a digit
    
     mov si, WORD PTR [bp+08h]
     mov cx, WORD PTR [bp+06h]            ;CL = D, CH = S
    
     ;Computes BX = (1 << D)-1
     
     mov bx, 1
     shl bx, cl
     dec bx
    
     xchg cl, ch              ;CL = S, CH = D
    
    _pp2_convert:
     mov di, si
     shr di, cl
     and di, bx         ;DI = Current digit
    
     or WORD PTR [bp+04h], di             ;If digit is non zero, [bp+04h] will become non zero
                          ;If [bp+04h] was non zero, result is non zero
     jnz _pp2_print                       ;Simply put, if the result is non zero, we must print the digit
    
     
     ;Here we have a non significant zero
     ;We should skip it BUT only if it is not the last digit (0 should be printed as "0" not
     ;an empty string)
    
     test cl, cl
     jnz _pp_continue
    
    
    _pp2_print:
     ;Convert digit to digital and print it
    
     
     mov dl, BYTE PTR [DIGITS + di]
     mov ah, 02h
     int 21h
    
    
    _pp_continue:
     ;Remove digit from the number
    
     sub cl, ch
    jnc _pp2_convert
    
     pop di
     pop si
     pop dx
     pop cx
     pop bx
     pop ax
    
     pop bp
     ret 06h
    
Data
---

    This data must be put in the data segment, the one reached by `DS`.

    DIGITS    db    "0123456789abcdef"
    
    ;Format for each WORD is S D where S and D are bytes (S the higher one)
    ;D = Bits per digit  --> log2(BASE)
    ;S = Initial shift count --> D*[ceil(16/D)-1]
    
    BASE2    EQU    0f01h
    BASE4    EQU    0e02h
    BASE8    EQU    0f03h
    BASE16    EQU    0c04h

NASM porting
---

To port the code to NASM remove the PTR keyword from memory accesses (e.g. `mov si, WORD PTR [bp+08h]` becomes `mov si, WORD PTR [bp+08h]`)

Extending the function
--

The function can be easily extended to any base up to 2<sup>255</sup>, though each base above 2<sup>16</sup> will print the same numeral as the number is only 16 bits.

To add a base:

1. Define a new constant `BASEx` where *x* is 2<sup>n</sup>.  
   The lower byte, named *D*, is *D* = *n*.  
   The upper byte, named *S*, is the position, in bits, of the higher group. It can be calculated as *S* = *n* · (⌈16/*n*⌉ - 1).
2. Add the necessary digits to the string `DIGITS`.

**Example: adding base 32**

We have *D* = 5 and *S* = 15, so we define `BASE32 EQU 0f05h`.  
We then add sixteen more digits: `DIGITS db "0123456789abcdefghijklmnopqrstuv"`.

As it should be clear, the digits can be changed by editing the `DIGITS` string.

---

<sup>1</sup> If *B* is a base, then it has *B* digits per definition. The number of bits per digit is thus *log*<sub>2</sub>(*B*). For power of two bases this simplifies to *log*<sub>2</sub>(2<sup>*n*</sup>) = *n* which is an integer by definition.  

<sup>2</sup> In this context it is assumed implicitly that the base under consideration is a power of two base 2<sup>*n*</sup>.

<sup>3</sup> For a base *B = 2<sup>*n*</sup>* to have an integral number of bit groups it must be that *n* | 16 (*n* divides 16). Since the only factor in 16 is 2, it must be that *n* is itself a power of two. So *B* has the form 2<sup>2<sup>*k*</sup></sup> or equivalently *log*<sub>2</sub>(*log*<sub>2</sub>(*B*)) must be an integer.



## MS-DOS, TASM/MASM, function to print a 16-bit number in decimal
Print a 16-bit unsigned number in decimal
---

The interrupt service [Int 21/AH=02h](http://www.ctyme.com/intr/rb-2554.htm) is used to print the digits.  
The standard conversion from *number* to *numeral* is performed with the `div` instruction, the dividend is initially the highest power of ten fitting 16 bits (10<sup>4</sup>) and it is reduced to lower powers at each iteration.

Parameters
--

The parameters are shown in order of push.  
Each one is 16 bits.

| Parameter| Description|
| ------ | ------ |
| number| The 16-bit unsigned number to print in decimal|
| show leading zeros | If 0 no non-significant zeros are printed, else they are. The number 0 is always printed as "0" |

Usage
--

    push 241
    push 0
    call print_dec          ;prints 241
    
    push 56
    push 1
    call print_dec          ;prints 00056
    
    push 0
    push 0
    call print_dec          ;prints 0


Code
--

    ;Parameters (in order of push):
    ;
    ;number
    ;Show leading zeros
    print_dec:
     push bp
     mov bp, sp
 
     push ax
     push bx
     push cx
     push dx

     ;Set up registers:
     ;AX = Number left to print
     ;BX = Power of ten to extract the current digit
     ;DX = Scratch/Needed for DIV
     ;CX = Scratch

     mov ax, WORD PTR [bp+06h]
     mov bx, 10000d
     xor dx, dx

    _pd_convert:     
     div bx                           ;DX = Number without highmost digit, AX = Highmost digit
     mov cx, dx                       ;Number left to print

     ;If digit is non zero or param for leading zeros is non zero
     ;print the digit
     or WORD PTR [bp+04h], ax
     jnz _pd_print

     ;If both are zeros, make sure to show at least one digit so that 0 prints as "0"
     cmp bx, 1
     jne _pd_continue

    _pd_print:

     ;Print digit in AL

     mov dl, al
     add dl, '0'
     mov ah, 02h
     int 21h

    _pd_continue:
     ;BX = BX/10
     ;DX = 0

     mov ax, bx
     xor dx, dx
     mov bx, 10d
     div bx
     mov bx, ax

     ;Put what's left of the number in AX again and repeat...
     mov ax, cx
    
     ;...Until the divisor is zero
     test bx, bx
    jnz _pd_convert

     pop dx
     pop cx
     pop bx
     pop ax

     pop bp
     ret 04h

NASM porting
--
To port the code to NASM remove the `PTR` keyword from memory accesses (e.g. `mov ax, WORD PTR [bp+06h]` becomes `mov ax, WORD [bp+06h]`)


