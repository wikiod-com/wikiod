---
title: "System Call Mechanisms"
slug: "system-call-mechanisms"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## BIOS calls
## How to interact with the BIOS ##

The Basic Input/Output System, or BIOS, is what controls the computer before any operating system runs. To access services provided by the BIOS, assembly code uses *interrupts*. An interrupt takes the form of

    int <interrupt> ; interrupt must be a literal number, not in a register or memory

The interrupt number must be between 0 and 255 (0x00 - 0xFF), inclusive.

Most BIOS calls use the `AH` register as a "function select" parameter, and use the `AL` register as a data parameter. The function selected by `AH` depends on the interrupt called. Some BIOS calls require a single 16-bit parameter in `AX`, or do not accept parameters at all, and are simply called by the interrupt. Some have even more parameters, that are passed in other registers.

The registers used for BIOS calls are fixed and cannot be interchanged with other registers.

## Using BIOS calls with function select ##

The general syntax for a BIOS interrupt using a function select parameter is:

    mov ah, <function>
    mov al, <data>
    int <interrupt>

## Examples ##

### How to write a character to the display: ###

    mov ah, 0x0E             ; Select 'Write character' function
    mov al, <char>           ; Character to write
    int 0x10                 ; Video services interrupt

### How to read a character from the keyboard (blocking): ###

    mov ah, 0x00             ; Select 'Blocking read character' function
    int 0x16                 ; Keyboard services interrupt
    mov <ascii_char>, al     ; AL contains the character read
    mov <scan_code>, ah      ; AH contains the BIOS scan code

### How to read one or more sectors from an external drive (using CHS addressing): ###

    mov ah, 0x02             ; Select 'Drive read' function
    mov bx, <destination>    ; Destination to write to, in ES:BX
    mov al, <num_sectors>    ; Number of sectors to read at a time
    mov dl, <drive_num>      ; The external drive's ID
    mov cl, <start_sector>   ; The sector to start reading from
    mov dh, <head>           ; The head to read from
    mov ch, <cylinder>       ; The cylinder to read from
    int 0x13                 ; Drive services interrupt
    jc <error_handler>       ; Jump to error handler on CF set

### How to read the system RTC (Real Time Clock): ###

    mov ah, 0x00             ; Select 'Read RTC' function
    int 0x1A                 ; RTC services interrupt
    shl ecx, 16              ; Clock ticks are split in the CX:DX pair, so shift ECX left by 16...
    or cx, dx                ; and add in the low half of the pair
    mov <new_day>, al        ; AL is non-zero if the last call to this function was before midnight
                             ; Now ECX holds the clock ticks (approx. 18.2/sec) since midnight
                             ; and <new_day> is non-zero if we passed midnight since the last read

### How to read the system time from the RTC:

    mov ah, 0x02             ; Select 'Read system time' function
    int 0x1A                 ; RTC services interrupt
                             ; Now CH contains hour, CL minutes, DH seconds, and DL the DST flag,
                             ; all encoded in BCD (DL is zero if in standard time)
                             ; Now we can decode them into a string (we'll ignore DST for now)

    mov al, ch               ; Get hour
    shr al, 4                ; Discard one's place for now
    add al, 48               ; Add ASCII code of digit 0
    mov [CLOCK_STRING+0], al ; Set ten's place of hour
    mov al, ch               ; Get hour again
    and al, 0x0F             ; Discard ten's place this time
    add al, 48               ; Add ASCII code of digit 0 again
    mov [CLOCK_STRING+1], al ; Set one's place of hour

    mov al, cl               ; Get minute
    shr al, 4                ; Discard one's place for now
    add al, 48               ; Add ASCII code of digit 0
    mov [CLOCK_STRING+3], al ; Set ten's place of minute
    mov al, cl               ; Get minute again
    and al, 0x0F             ; Discard ten's place this time
    add al, 48               ; Add ASCII code of digit 0 again
    mov [CLOCK_STRING+4], al ; Set one's place of minute

    mov al, dh               ; Get second
    shr al, 4                ; Discard one's place for now
    add al, 48               ; Add ASCII code of digit 0
    mov [CLOCK_STRING+6], al ; Set ten's place of second
    mov al, dh               ; Get second again
    and al, 0x0F             ; Discard ten's place this time
    add al, 48               ; Add ASCII code of digit 0 again
    mov [CLOCK_STRING+7], al ; Set one's place of second
    ...
    db CLOCK_STRING "00:00:00", 0   ; Place in some separate (non-code) area

### How to read the system date from the RTC: ###

    mov ah, 0x04             ; Select 'Read system date' function
    int 0x1A                 ; RTC services interrupt
                             ; Now CH contains century, CL year, DH month, and DL day, all in BCD
                             ; Decoding to a string is similar to the RTC Time example above

### How to get size of contiguous low memory:

    int 0x12                 ; Conventional memory interrupt (no function select parameter)
    and eax, 0xFFFF          ; AX contains kilobytes of conventional memory; clear high bits of EAX
    shl eax, 10              ; Multiply by 1 kilobyte (1024 bytes = 2^10 bytes)
                             ; EAX contains the number of bytes available from address 0000:0000

### How to reboot the computer:

    int 0x19                 ; That's it! One call. Just make sure nothing has overwritten the
                             ; interrupt vector table, since this call does NOT restore them to the
                             ; default values of normal power-up. This means this call will not
                             ; work too well in an environment with an operating system loaded.

## Error handling ##

Some BIOS calls may not be implemented on every machine, and are not guaranteed to work. Often an unimplemented interrupt will return either `0x86` or `0x80` in register `AH`. **Just about every interrupt will set the carry flag (CF) on an error condition.** This makes it easy to jump to an error handler with the `jc` conditional jump. (See [Conditional Jumps](https://www.wikiod.com/x86/control-flow#Conditional jumps))

## References ##

A rather exhaustive list of BIOS calls and other interrupts is [Ralf Brown's Interrupt List](https://www.cs.cmu.edu/~ralf/files.html). An HTML version can be found [here](http://www.delorie.com/djgpp/doc/rbinter/).

Interrupts often assumed to be available are found in a list on [Wikipedia](https://en.wikipedia.org/wiki/BIOS_interrupt_call).

A more in-depth overview of commonly available interrupts can be found at [osdev.org](http://wiki.osdev.org/BIOS)





