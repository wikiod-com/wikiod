---
title: "Multiprocessor management"
slug: "multiprocessor-management"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Parameters
| LAPIC register| Address (Relative to *APIC BASE*)|
| ------ | ------ |
| Local APIC ID Register | +20h |
| Spurious Interrupt Vector Register | +0f0h |
| Interrupt Command Register (ICR); bits 0-31 | +300h |
| Interrupt Command Register (ICR); bits 32-63 | +310h |


In order to access the LAPIC registers a segment must be able to reach the address range starting at *APIC Base* (in *IA32_APIC_BASE*).  
This address is relocatable and can theoretically be set to point somewhere in the lower memory, thus making the range addressable in real mode.  

The read/write cycles to the LAPIC range are **not** however propagated to the Bus Interface Unit, thereby masking any access to the addresses "behind" it.  

It is assumed that the reader is familiar with the [Unreal mode](https://www.wikiod.com/x86/real-vs-protected-modes#Unreal mode), since it will be used in some example.

It is also necessary to be proficient with:
* Handling the difference between *logical* and *physical* addresses<sup>1</sup>
* [Real mode](https://www.wikiod.com/x86/real-vs-protected-modes#Real Mode) segmentation.
* Memory aliasing, id est the ability to use different *logical* addresses for the same *physical* address
* Absolute, relative, far, near calls and jumps.
* [NASM assembler](http://www.nasm.us/), particularly that the `ORG` directive is global. Splitting the code into multiple files **greatly** simplify the coding as it will be possible to give different section different *ORGs*.

Finally, we assume the CPU has a *Local Advanced Programmable Interrupt Controller* (*LAPIC*).  
If ambiguous from the context, APIC always means LAPIC (e not IOAPIC, or xAPIC in general).

---

References:

* Chapter 8 and 10 of [Intel manuals](https://www-ssl.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html).

|Bitfields  |
| --- |
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;![Spurious Interrupt Vector Register](http://i.stack.imgur.com/aJskw.png)|
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;![Interrupt Command Register](http://i.stack.imgur.com/tswa4.png)|
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;![Local APIC ID Register](http://i.stack.imgur.com/5WxC4.png)| 
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;![IA32_APIC_BASE](http://i.stack.imgur.com/BpDgH.png)| 


| MSR name | Address |
| --- | --- |
| IA32_APIC_BASE| 1bh |

---

<sup>1</sup> If paging will be used, *virtual* addresses also come into play.

## Wake up all the processors
This example will wake up every *Application Processor* (AP) and make them, along with the *Bootstrap Processor* (BSP), display their LAPIC ID.  

    ; Assemble boot sector and insert it into a 1.44MiB floppy image
    ;
    ; nasm -f bin boot.asm -o boot.bin
    ; dd if=/dev/zero of=disk.img bs=512 count=2880
    ; dd if=boot.bin of=disk.img bs=512 conv=notrunc

    BITS 16
    ; Bootloader starts at segment:offset 07c0h:0000h
    section bootloader, vstart=0000h
    jmp 7c0h:__START__
    
    __START__:
     mov ax, cs
     mov ds, ax
     mov es, ax
     mov ss, ax
     xor sp, sp
     cld
    
     ;Clear screen
     mov ax, 03h
     int 10h
    
     ;Set limit of 4GiB and base 0 for FS and GS
     call 7c0h:unrealmode
    
     ;Enable the APIC
     call enable_lapic
    
     ;Move the payload to the expected address
     mov si, payload_start_abs
     mov cx, payload_end-payload + 1
     mov di, 400h                 ;7c0h:400h = 8000h
     rep movsb
    
    
     ;Wakeup the other APs
    
     ;INIT
     call lapic_send_init
     mov cx, WAIT_10_ms
     call us_wait
    
     ;SIPI
     call lapic_send_sipi
     mov cx, WAIT_200_us
     call us_wait
    
     ;SIPI
     call lapic_send_sipi
    
     ;Jump to the payload
     jmp 0000h:8000h
    
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
    
     ;CX = Wait (in ms) Max 65536 us (=0 on input)
     us_wait:
      mov dx, 80h               ;POST Diagnose port, 1us per IO
      xor si, si
      rep outsb
    
      ret
    
      WAIT_10_ms     EQU 10000
      WAIT_200_us    EQU 200
    
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
    
    
     enable_lapic:
    
      ;Enable the APIC globally
      ;On P6 CPU once this flag is set to 0, it cannot be set back to 16
      ;Without an HARD RESET
      mov ecx, IA32_APIC_BASE_MSR
      rdmsr
      or ah, 08h        ;bit11: APIC GLOBAL Enable/Disable
      wrmsr
    
      ;Mask off lower 12 bits to get the APIC base address
      and ah, 0f0h
      mov DWORD [APIC_BASE], eax
    
      ;Newer processors enables the APIC through the Spurious Interrupt Vector register
      mov ecx, DWORD [fs: eax + APIC_REG_SIV]
      or ch, 01h                                ;bit8: APIC SOFTWARE enable/disable
      mov DWORD [fs: eax+APIC_REG_SIV], ecx
    
      ret
    
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
    
     lapic_send_sipi:
      mov eax, DWORD [APIC_BASE]
    
      ;Destination field is set to 0 has we will use a shorthand
      xor ebx, ebx
      mov DWORD [fs: eax+APIC_REG_ICR_HIGH], ebx
    
      ;Vector: 08h (Will make the CPU execute instruction ad address 08000h)
      ;Delivery mode: Startup
      ;Destination mode: ignored (0)
      ;Level: ignored (1)
      ;Trigger mode: ignored (0)
      ;Shorthand: All excluding self (3)
      mov ebx, 0c4608h
      mov DWORD [fs: eax+APIC_REG_ICR_LOW], ebx  ;Writing the low DWORD sent the IPI
    
      ret
    
      ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
    
     lapic_send_init:
      mov eax, DWORD [APIC_BASE]
    
      ;Destination field is set to 0 has we will use a shorthand
      xor ebx, ebx
      mov DWORD [fs: eax+APIC_REG_ICR_HIGH], ebx
    
      ;Vector: 00h
      ;Delivery mode: Startup
      ;Destination mode: ignored (0)
      ;Level: ignored (1)
      ;Trigger mode: ignored (0)
      ;Shorthand: All excluding self (3)
      mov ebx, 0c4500h
      mov DWORD [fs: eax+APIC_REG_ICR_LOW], ebx  ;Writing the low DWORD sent the IPI
    
      ret
    
     IA32_APIC_BASE_MSR    EQU    1bh
    
     APIC_REG_SIV        EQU    0f0h
    
     APIC_REG_ICR_LOW    EQU 300h
     APIC_REG_ICR_HIGH    EQU 310h
    
     APIC_REG_ID        EQU 20h
    
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
    
     APIC_BASE            dd     00h
    
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
    
    unrealmode:
     lgdt [cs:GDT]
    
     cli
    
     mov eax, cr0
     or ax, 01h
     mov cr0, eax
    
     mov bx, 08h
     mov fs, bx
     mov gs, bx
    
     and ax, 0fffeh
     mov cr0, eax
    
     sti
    
     ;IMPORTAT: This call is FAR!
     ;So it can be called from everywhere
     retf
    
     GDT:
        dw 0fh
        dd GDT + 7c00h
        dw 00h
    
        dd 0000ffffh
        dd 00cf9200h
    
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
     ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
    
    payload_start_abs:
    ; payload starts at segment:offset 0800h:0000h
    section payload, vstart=0000h, align=1
     payload:
    
      ;IMPORTANT NOTE: Here we are in a "new" CPU every state we set before is no
      ;more present here (except for the BSP, but we handler every processor with
      ;the same code).
     jmp 800h: __RESTART__
    
     __RESTART__:
      mov ax, cs
      mov ds, ax
      xor sp, sp
      cld
    
      ;IMPORTANT: We can't use the stack yet. Every CPU is pointing to the same stack!
    
      ;Get an unique id
      mov ax, WORD [counter]
      .try:
        mov bx, ax
        inc bx
        lock cmpxchg WORD [counter], bx
       jnz .try
    
      mov cx, ax            ;Save this unique id
    
      ;Stack segment = CS + unique id * 1000
      shl ax, 12
      mov bx, cs
      add ax, bx
      mov ss, ax
    
      ;Text buffer
      push 0b800h
      pop es
    
      ;Set unreal mode again
      call 7c0h:unrealmode
    
      ;Use GS for old variables
      mov ax, 7c0h
      mov gs, ax
    
      ;Calculate text row
      mov ax, cx
      mov bx, 160d           ;80 * 2
      mul bx
      mov di, ax
    
      ;Get LAPIC id
      mov ebx, DWORD [gs:APIC_BASE]
      mov edx, DWORD [fs:ebx + APIC_REG_ID]
      shr edx, 24d
      call itoa8
    
      cli
      hlt
    
      ;DL = Number
      ;DI = ptr to text buffer
      itoa8:
        mov bx, dx
        shr bx, 0fh
        mov al, BYTE [bx +  digits]
        mov ah, 09h
        stosw
    
        mov bx, dx
        and bx, 0fh
        mov al, BYTE [bx +  digits]
        mov ah, 09h
        stosw
    
        ret
    
      digits db "0123456789abcdef"
      counter dw 0
    
     payload_end:
    
    
    
    ; Boot signature is at physical offset 01feh of
    ; the boot sector
    section bootsig, start=01feh
     dw 0aa55h

---

There are two major steps to perform:

**1. Waking the APs**  
This is achieved by inssuing a *INIT-SIPI-SIPI* (ISS) sequence to the all the APs. 
 
The BSP that will send the ISS sequence using as destination the shorthand *All excluding self*, thereby targeting all the APs. 
 
A SIPI (Startup Inter Processor Interrupt) is ignored by all the CPUs that are waked by the time they receive it, thus the second SIPI is ignored if the first one suffices to wake up the target processors. It is advised by Intel for compatibility reason.

A SIPI contains a *vector*, this is similar in meaning, **but absolutely different in practice**, to an interrupt vector (a.k.a. interrupt number).  
The vector is an 8 bit number, of value *V* (represented as *vv* in base 16), that makes the CPU starts executing instructions at the *physical* address *0vv000h*.  
We will call *0vv000h* the *Wake-up address* (WA).  
The WA is forced at a 4KiB (or page) boundary.

We will use 08h as *V*, the WA is then *08000h*, 400h bytes after the bootloader.

This gives control to the APs.

**2. Initializing and differentiating the APs**  
It is necessary to have an executable code at the WA. The bootloader is at *7c00h*, so we need to relocate some code at page boundary.  

The first thing to remember when writing the payload is that any access to a shared resource must be protected or differentiated.  
**A common shared resource is the stack**, if we initialize the stack naively, every APs will end up using the same stack!  

The first step is then using different stack addresses, thus *differentiating* the stack.  
We accomplish that by assigning an unique number, zero based, for each CPU. This number, we will call it *index*, is used for differentiating the stack and the line were the CPU will write its APIC ID.  

The stack address for each CPU is *800h:(index* * *1000h)* giving each AP 64KiB of stack.  
The line number for each CPU is *index*, the pointer into the text buffer is thus 80 * 2 *  *index*.  

To generate the index a `lock cmpxchg` is used to atomically increment and return a WORD. 

**Final notes**  
* A write to port 80h is used to generate a delay of 1 µs.  
* `unrealmode` is a far routine, so it can be called after the wake up too.  
* The BSP also jump to the WA.

**Screenshot**

From Bochs with 8 processors

[![Screenshot with eight processors][1]][1]


  [1]: http://i.stack.imgur.com/4ryWT.png



