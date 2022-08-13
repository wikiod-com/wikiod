---
title: "Linux elf64 examples not using glibc"
slug: "linux-elf64-examples-not-using-glibc"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## User Interface
I would venture to say that 80% of the processing that goes on in modern computing systems does not require user interaction, such as kernel code for Linux, OSX and Windows. For those that do, there are two fundamentals which are interactivity via keyboard (_pointing devices_) and console. This example and others in my series are oriented around text based console (VT100 emulation) and keyboard.

In and of itself, this example is very simple, but it is an essential building block toward more complex algorithms.<h1>Subrtx.asm</h1>

            STDIN    equ        0
           STDOUT    equ        1

         SYS_READ    equ        0
        SYS_WRITE    equ        1

        global  gets, strlen, print, atoq

                section .text
As this is intended exclusively for keyboard, the probability of errors is next to none. I would imagine most often, program will be able to contemplate buffer size to
circumvent buffer overrun, but that is not guaranteed due to indirection.

    ; =============================================================================
    ; Accept canonical input from operator for a maximum of EDX bytes and replace
    ; terminating CR with NULL.

    ;        ENTER: RSI = Pointer to input buffer
    ;               EDX = Maximum number of characters

    ;        LEAVE: EAX = Number of characters entered
    ;               R11 = Modified by syscall, all others preserved.

    ;        FLAGS:  ZF = Null entry, NZ otherwise.
    ; _____________________________________________________________________________

         gets:  push    rcx
                push    rdi

                xor     eax, eax            ; RAX = SYS_READ
                mov     edi, eax            ; RDI = STDIN
                syscall

    ; TODO:    Should probably do some error trapping here, especially for
    ;            buffer overrun, but I'll see if it becomes an issue over time.

                dec     eax                 ; Bump back to CR
                mov     byte [rsi+rax], 0   ; Replace it with NULL

                pop     rdi
                pop     rcx
                ret
To begin with, this was intended to circumvent the need to either code or manually calculate a strings length for write(2). Then I decided to incorporate a delimiter, now it can be used to scan for any character (0 - FF). This opens the possibility for word wrapping text for example, so the label ***strlen*** is a bit of a misnomer as one would generally think the result is going to be the number of visible character.

    ; =============================================================================
    ; Determine length, including terminating character EOS. Result may include
    ; VT100 escape sequences.

    ;        ENTER: RDI = Pointer to ASCII string.
    ;               RCX   Bits 31 - 08 = Max chars to scan (1 - 1.67e7)
    ;                           07 - 00 = Terminating character (0 - FF)

    ;        LEAVE: RAX = Pointer to next string (optional).

    ;        FLAGS:  ZF = Terminating character found, NZ otherwise (overrun).
    ; _____________________________________________________________________________

       strlen:  push    rcx                 ; Preserve registers used by proc so
                push    rdi                 ; it's non-destructive except for RAX.

                mov      al, cl             ; Byte to scan for in AL.
                shr     ecx, 8              ; Shift max count into bits 23 - 00

    ; NOTE: Probably should check direction flag here, but I always set and
    ;       reset DF in the process that is using it.

                repnz   scasb               ; Scan for AL or until ECX = 0
                mov     rax, rdi            ; Return pointer to EOS + 1

                pop     rdi                 ; Original pointer for proglogue
                jz      $ + 5               ; ZF indicates EOS was found
                mov     rax, rdi            ; RAX = RDI, NULL string
                pop     rcx

                ret
The intent to this procedure is to simplify loop design in the calling procedure.

    ; =============================================================================
    ; Display an ASCIIZ string on console that may have embedded VT100 sequences.

    ;        ENTER: RDI = Points to string

    ;        LEAVE: RAX = Number of characters displayed, including EOS
    ;                   = Error code if SF
    ;               RDI = Points to byte after EOS.
    ;               R11 = Modified by syscall all others preserved

    ;        FLAGS:  ZF = Terminating NULL was not found. NZ otherwise
    ;                SF = RAX is negated syscall error code.
    ;______________________________________________________________________________

        print:  push    rsi
                push    rdx
                push    rcx

                mov     ecx, -1 << 8        ; Scan for NULL
                call    strlen
                push    rax                 ; Preserve point to next string
                sub     rax, rdi            ; EAX = End pntr - Start pntr
                jz      .done

         ; size_t = write (int STDOUT, char *, size_t length)

                mov     edx, eax            ; RDX = length
                mov     rsi, rdi            ; RSI = Pointer
                mov     eax, SYS_WRITE
                mov     edi, eax            ; RDI = STDOUT
                syscall
                or      rax, rax            ; Sets SF if syscall error
                
    ; NOTE:    This procedure is intended for console, but in the event STDOUT is
    ;        redirected by some means, EAX may return error code from syscall.

        .done:  pop     rdi                 ; Retrieve pointer to next string.
                pop     rcx
                pop     rdx
                pop     rsi

                ret
Finally an example of how these functions can be used.
<h1>Generic.asm</h1>

    global  _start

        extern  print, gets, atoq

        SYS_EXIT  equ   60
             ESC  equ   27

           BSize  equ   96

                section .rodata
       Prompt:  db  ESC, '[2J'      ; VT100 clear screen
                db  ESC, '[4;11H'   ;   "   Position cursor to line 4 column 11
                db  'ASCII -> INT64 (binary, octal, hexidecimal, decimal), '
                db  'Packed & Unpacked BCD and floating point conversions'
                db  10, 10, 0, 9, 9, 9, '=> ', 0
                db  10, 9, 'Bye'
                db  ESC, '[0m'      ; VT100 Reset console
                db  10, 10, 0

                section .text
       _start:  pop    rdi
                mov    rsi, rsp
                and    rsp, byte 0xf0       ; Align stack on 16 byte boundary.

                call   main
                mov    rdi, rax             ; Copy return code into ARG0

                mov    eax, SYS_EXIT
                syscall

    ; int main ( int argc, char *args[] )

         main:  enter   BSize, 0            ; Input buffer on stack
                mov     edi, Prompt
                call    print
                lea     rsi, [rbp-BSize]    ; Establish pointer to input buffer
                mov     edx, BSize          ; Max size for read(2)

        .Next:  push    rdi                 ; Save pointer to "=> "
                call    print
                call    gets
                jz      .done

                call    atoq                ; Convert string pointed to by RSI 

                pop     rdi                 ; Restore pointer to prompt
                jmp     .Next

        .done:  call    print               ; RDI already points to "Bye"
                xor     eax, eax
                leave
                ret
<h1>Makefile</h1>

    OBJECTS = Subrtx.o Generic.o

    Generic : $(OBJECTS)
        ld -oGeneric $(OBJECTS)
        readelf -WS Generic
        
    Generic.o : Generic.asm
         nasm -g -felf64 Generic.asm
         
    Subrtx.o : Subrtx.asm
        nasm -g -felf64 Subrtx.asm
    
    clean:
        rm -f $(OBJECTS) Generic

