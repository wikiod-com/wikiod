---
title: "Real vs Protected modes"
slug: "real-vs-protected-modes"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Real Mode
When Intel designed the original x86, the 8086 (and 8088 derivative), they included Segmentation to allow the 16-bit processor to access more than 16 bits worth of address. They did this by making the 16-bit addresses be relative to a given 16-bit Segment Register, of which they defined four: Code Segment (`CS`), Data Segment (`DS`), Extra Segment (`ES`) and Stack Segment (`SS`).

Most instructions implied which Segment Register to use: instructions were fecthed from the Code Segment, `PUSH` and `POP` implied the Stack Segment, and simple data references implied the Data Segment - although this could be overridden to access memory in any of the other Segments.

The implementation was simple: for every memory access, the CPU would take the implied (or explicit) Segment Register, shift it four places to the left, then add in the indicated address:

            +-------------------+---------+
    Segment | 16-bit value      | 0 0 0 0 |
            +-------------------+---------+
                       PLUS
            +---------+-------------------+
    Address | 0 0 0 0 | 16-bit value      |
            +---------+-------------------+
                      EQUALS
            +-----------------------------+
    Result  |  20-bit memory address      |
            +-----------------------------+

This allowed for various techniques:
* Allowing Code, Data and Stack to all be mutually accessable (`CS`, `DS` and `SS` all had the same value);
* Keeping Code, Data and Stack completely separate from each other (`CS`, `DS` and `SS` all 4K (or more) separate from each other - remember it gets multiplied by 16, so that's 64K).

It also allowed bizarre overlaps and all sorts of weird things!

When the 80286 was invented, it supported this legacy mode (now called "Real Mode"), but added a new mode called "Protected Mode" (q.v.).

The important things to notice is that in Real Mode:
* Any memory address was accessible, simply by putting the correct value inside a Segment Register and accessing the 16-bit address;
* The extent of "protection" was to allow the programmer to separate different areas of memory for different purposes, and make it _harder_ to accidentally write to the wrong data - while still making it possible to do so.

In other words... not very protected at all!


## Protected Mode
# Introduction
When the 80286 was invented, it supported the legacy 8086 Segmentation (now called "Real Mode"), and added a new mode called "Protected Mode". This mode has been in every x86 processor since, albeit enhanced with various improvements such as 32- and 64-bit addressing.

# Design
In Protected Mode, the simple "Add address to Shifted Segment Register value" was done away with completely. They kept the Segment Registers, but instead of using them to calculate an address, they used them to index into a table (actually, one of two...) which defined the Segment to be accessed. This definition not only described where in memory the Segment was (using Base and Limit), but also what _type_ of Segment it was (Code, Data, Stack or even System) and what kinds of programs could access it (OS Kernel, normal program, Device Driver, etc.).

# Segment Register
Each 16-bit Segment Register took on the following form:

    +------------+-----+------+
    | Desc Index | G/L | Priv |
    +------------+-----+------+
     Desc Index = 13-bit index into a Descriptor Table (described below)
     G/L        = 1-bit flag for which Descriptor Table to Index: Global or Local
     Priv       = 2-bit field defining the Privilege level for access

## Global / Local
The Global/Local bit defined whether the access was into a Global Table of descriptors (called unsurprisingly the Global Descriptor Table, or GDT), or a Local Descriptor Table (LDT). The idea for the LDT was that every program could have its own Descriptor Table - the OS woud define a Global set of Segments, and each program would have its own set of Local Code, Data and Stack Segments. The OS would manage the memory between the different Descriptor Tables.

# Descriptor Table
Each Descriptor Table (Global or Local) was a 64K array of 8,192 Descriptors: each an 8-byte record that defined multiple aspects of the Segment that it was describing. The Segment Registers' Descriptor Index fields allowed for 8,192 descriptors: no coincidence!

# Descriptor
A Descriptor held the following information - note that the format of the Descriptor changed as new processors were released, but the same sort of information was kept in each:

* **Base**  
  This defined the start address of the memory segment.
* **Limit**  
  This defined the size of the memory segment - sort of. They had to make a decision: would a size of `0x0000` mean a size of `0`, so not accessible? Or maximum size?  
Instead they chose a third option: the Limit field was the last addressible location within the Segment. That meant that a one-bye Segment could be defined; or a maximum-sized one for the address size.
* **Type**  
  There were multiple types of Segments: the traditional Code, Data and Stack (see below), but other System Segments were defined as well:
  - Local Descriptor Table Segments defined how many Local Descriptors could be accessed;
  - Task State Segments could be used for hardware-managed context switching;
  - Controlled "Call Gates" that could allow programs to call into the Operating System - but only through carefully managed entry points.
* **Attributes**  
  Certain attributes of the Segment were also maintained, where relevant:
  - Read-Only vs Read-Write;
  - Whether the Segment was currently Present or not - allowing for on-demand memory management;
  - What level of code (OS vs Driver vs program) could access this Segment.

# True protection at last!
If the OS kept the Descriptor Tables in Segments that couldn't be accessed by mere programs, then it could tightly manage which Segments were defined, and what memory was assigned and accessible to each. A program could manufacture whatever Segment Register value it liked - but if it had the _audaciousness_ to actually _load_ it into a _Segment Register_!... the CPU hardware would recognise that the proposed Descriptor value broke any one of a large number of rules, and instead of completing the request, it would raise an Exception to the Operating System to allow it to handle the errant program.

> This Exception was usually #13, the General Protection Exception - made world famous by Microsoft Windows... (Anyone think an Intel engineer was superstitious?)

# Errors
The sorts of errors that could happen included:
* If the proposed Descriptor Index was larger than the size of the table;
* If the proposed Descriptor was a System Descriptor rather than Code, Data or Stack;
* If the proposed Descriptor was more privileged than the requesting program;
* If the proposed Descriptor was marked as Not Readable (such as a Code Segment), but it was attempted to be Read rather than Executed;
* If the proposed Descriptor was marked Not Present.

  > Note that the last may not be a fatal problem for the program: the OS could note the flag, reinstate the Segment, mark it as now Present then allow the faulting instruction to proceed successfully.

Or, perhaps the Descriptor was successfully loaded into a Segment Register, but then a future access with it broke one of a number of rules:
* The Segment Register was loaded with the `0x0000` Descriptor Index for the GDT. This was reserved by the hardware as `NULL`;
* If the loaded Descriptor was marked Read-Only, but a Write was attempted to it.
* If any part of the access (1, 2, 4 or more bytes) was outside the Limit of the Segment.



## Switching into Protected Mode
Switching into Protected Mode is easy: you just need to set a single bit in a Control Register. But _staying_ in Protected Mode, without the CPU throwing up its hands and resetting itself due to not knowing what to do next, takes a lot of preparation.

In short, the steps required are as follows:
* An area of memory for the Global Descriptor Table needs to be set up to define a minimum of three Descriptors:
  1. The zeroeth, `NULL` Descriptor;
  2. Another Descriptor for a Code Segment;
  3. Another Descriptor for a Data Segment.
     > This can be used for both Data and Stack.
* The Global Descriptor Table Register (`GDTR`) needs to be initialised to point to this defined area of memory;

       GDT_Ptr    dw      SIZE GDT
                  dd      OFFSET GDT

                  ...

                  lgdt    [GDT_Ptr]
* The `PM` bit in `CR0` needs to be set:

           mov   eax, cr0      ; Get CR0 into register
           or    eax, 0x01     ; Set the Protected Mode bit
           mov   cr0, eax      ; We're now in Protected Mode!
* The Segment Registers need to be loaded from the GDT to remove the current Real Mode values:

           jmp   0x0008:NowInPM  ; This is a FAR Jump. 0x0008 is the Code Descriptor

      NowInPM:
           mov   ax, 0x0010      ; This is the Data Descriptor
           mov   ds, ax
           mov   es, ax
           mov   ss, ax
           mov   sp, 0x0000      ; Top of stack!

Note that this is the _**bare**_ minimum, just to get the CPU into Protected Mode. To actually get the whole system ready may require many more steps. For example:

* The upper memory areas may have to be enabled - turning off the `A20` gate;
* The Interrupts should definitely be disabled - but perhaps the various Fault Handlers could be set up before entering Protected Mode, to allow for errors early on in the processing.

> The original author of this section wrote an entire [tutorial][1] on entering Protected Mode and working with it.


  [1]: http://wiki.osdev.org/JohnBurger:Demo

## Unreal mode
The *unreal mode* exploits two facts on how both Intel and AMD processors load and save the information to describe a segment.

1. The processor caches the descriptor information fetched during a *move* in a selector register in protected mode.  
These information are stored in an architectural invisible part of the selector register themselves. 

2. In real mode the selector registers are called segment registers but, other than that, they designate the same set of registers and as such they also have an invisible part. These parts are filled with fixed values, but for the base which is derived from the value just loaded.  

In such view, real mode is just a special case of protected mode: where the information of a segment, suchlike the base and limit, are fetched without a GDT/LDT but still read from the segment register hidden part.

---

By switching in protected mode and crafting a GDT is possible to create a segment with the desired attributes, for example a base of 0 and a limit of 4GiB.  
Through a successive loading of a selector register such attributes are cached, it is then possible to switch back in real mode and have a segment register through which access the whole 32 bit address space. 

    BITS 16
    
    jmp 7c0h:__START__
    
    __START__:
     push cs
     pop ds
     push ds
     pop ss
     xor sp, sp
    
     
     lgdt [GDT]            ;Set the GDTR register
     
    
     cli                ;We don't have an IDT set, we can't handle interrupts
    
    
     ;Entering protected mode
    
     mov eax, cr0
     or ax, 01h            ;Set bit PE (bit 0) of CR0
     mov cr0, eax            ;Apply
    
     ;We are now in Protected mode
    
     mov bx, 08h           ;Selector to use, RPL = 0, Table = 0 (GDT), Index = 1
    
     mov fs, bx            ;Load FS with descriptor 1 info
     mov gs, bx            ;Load GS with descriptor 1 info
    
     ;Exit protected mode
    
     and ax, 0fffeh            ;Clear bit PE (bit0) of CR0
     mov cr0, eax                   ;Apply
    
     sti                
    
     ;Back to real mode
    
     ;Do nothing
     cli
     hlt 
    
    
    
     GDT:
        ;First entry, number 0
        ;Null descriptor
        ;Used to store a m16&32 object that tells the GDT start and size
    
        dw 0fh                 ;Size in byte -1 of the GDT (2 descriptors = 16 bytes)
        dd GDT + 7c00h         ;Linear address of GDT start (24 bits)
        dw 00h                 ;Pad 
    
        dd 0000ffffh           ;Base[15:00] = 0, Limit[15:00] = 0ffffh
        dd 00cf9200h           ;Base[31:24] = 0, G = 1, B = 1, Limit[19:16] = 0fh, 
                   ;P = 1, DPL = 0, E = 0, W = 1, A = 0, Base[23:16] = 00h
    
    
     TIMES 510-($-$$) db 00h
     dw 0aa55h 

---

**Considerations**
* As soon as a segment register is reloaded, even with the same value, the processor reload the hidden attributes according to the current mode. This is why the code above use `fs` and `gs` to hold the "extended" segments: such registers are less likely to be used/saved/restored by the various 16 bit services.
* The `lgdt` instruction doesn't load a far pointer to the GDT, instead it loads a 24 bit (can be overridden to 32 bit) *linear address*. This is not a *near address*, it is the *physical address* (since paging must be disabled). That's the reason of `GDT+7c00h`.
* The program above is a bootloader (for MBR, it has no BPB) that set `cs`/`ds`/`ss` tp *7c00h* and start the location counter from 0. So a byte at offset *X* in the file is at offset *X* in the segment *7c00h* and at the linear address *7c00h + X*.
*  Interrupts must be disabled as an IDT is not set for the short round trip in protected mode.
* The code use an hack to save 6 bytes of code. The structure loaded by `lgdt` is saved in the... GDT itself, in the null descriptor (the first descriptor).

---

For a description of the GDT descriptors see Chapter 3.4.3 of [Intel Manual Volume 3A](https://www-ssl.intel.com/content/www/us/en/architecture-and-technology/64-ia-32-architectures-software-developer-vol-3a-part-1-manual.html).

