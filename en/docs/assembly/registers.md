---
title: "Registers"
slug: "registers"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

**What are Registers?**

The processor can operate upon numeric values (numbers), but these have to be stored somewhere first. The data are stored mostly in memory, or inside the instruction opcode (which is stored usually in memory too), or in special on-chip memory placed directly in processor, which is called **register**.

To work with value in register, you don't need to address it by address, but special mnemonic "names" are used, like for example `ax` on x86, or `A` on Z80, or `r0` on ARM.

Some processors are constructed in a way, where almost all registers are equal and can be used for all purposes (often RISC group of processors), others have distinct specialization, when only some registers may be used for arithmetic (*"accumulator"* on early CPUs) and other registers for memory addressing only, etc.

This construction using memory directly on the processor chip has huge performance implication, adding two numbers from registers storing it back to register is usually done in shortest possible time by that processor (Example on ARM processor: `ADD r2,r0,r1` sets `r2` to `(r0 + r1)` value, in single processor cycle).

On the contrary, when one of the operands is referencing a memory location, the processor may stall for some time, waiting for the value to arrive from the memory chip (on x86 this can range from zero wait for values in L0 cache to hundreds of CPU cycles when the value is not in any cache and has to be read directly from memory DRAM chip).

So when programmer is creating some data processing code, she usually wants to have all data during processing in registers to get best performance. If that's not possible, and memory reads/writes are required, then those should be minimised, and form a pattern which cooperates with caches/memory architecture of the particular platform.

The native size of register in bits is often used to group processors, like *Z80* being *"8 bit processor"*, and *80386* being *"32 bit processor"* - although that grouping is rarely a clear cut. For example *Z80* operates also with pairs of registers, forming native 16 bit value, and 32 bit *80686* CPU has MMX instructions to work with 64 bit registers natively.

## x64 Registers
The x64 architecture is the evolution of the older x86 architecture, it kept compatibility with its predecessor (x86 registers are still available) but it also introduced new features:

 - Registers have now a capacity of 64 bits;
 - There are 8 more general-purpose registers;
 - Segment registers are forced to 0 in 64 bits mode;
 - The lower 32, 16 and 8 bits of each register are now available.

**General-purpose**

| Register | Name |  Subregisters(bits)
| ------ | ------ |---|
| RAX | Accumulator | EAX(32), AX(16), AH(8), AL(8) |
| RBX | Base | EBX(32), BX(16), BH(8), BL(8) |
| RCX | Counter | ECX(32), CX(16), CH(8), CL(8) |
| RDX | Data | EDX(32), DX(16), DH(8), DL(8) |
| RSI | Source | ESI(32), SI(16), SL(8) |
| RDI | Destination | EDI(32), DI(16), DL(8) |
| RBP | Base pointer| EBP(32), BP(16), BPL(8) |
| RSP | Stack pointer | ESP(32), SP(16), SPL(8) |
| R8-R15 | New registers | R8D-R15D(32), R8W-R15W(16), R8B-R15B(8)|

**Note**

The suffixes used to address the lower bits of the new registers stand for:
 - B byte, 8 bits;
 - W word, 16 bits;
 - D double word, 32 bits.

## Zilog Z80 registers
Registers: 8 bit: `A`, `B`, `C`, `D`, `E`, `H`, `L`, `F`, `I`, `R`, 16 bit: `SP`, `PC`, `IX`, `IY`, and shadows of some 8b registers: `A'`, `B'`, `C'`, `D'`, `E'`, `H'`, `L'` and `F'`.

Most of the 8 bit registers can be used also in pairs as 16 bit registers: `AF`, `BC`, `DE` and `HL`.

`SP` is *stack pointer*, marking the bottom of stack memory (used by `PUSH`/`POP`/`CALL`/`RET` instructions).  
`PC` is *program counter*, pointing to the currently executed instruction.  
`I` is *Interrupt register*, supplying high byte of vector table address for `IM 2` interrupt mode.  
`R` is *refresh register*, it increments each time the CPU fetches an opcode (or opcode prefix).  
Some unofficial instructions exist on some Z80 processors to manipulate 8bit parts of `IX` as `IXH:IXL` and `IY` as `IYH:IYL`.

Shadow variants can't be directly accessed by any instruction, the `EX AF,AF'` instruction will swap between `AF` and `AF'`, and `EXX` instruction will swap `BC,DE,HL` with `BC',DE',HL'`.

---
Loading value into a register:

        ; from other register
        LD   I,A        ; copies value in A into I (8 bit)
        LD   BC,HL      ; copies value in HL into BC (16 bit)
        ; directly with value encoded in instruction machine code
        LD   B,d8       ; 8b value d8 into B
        LD   DE,d16     ; 16b value d16 into DE
        ; from a memory (ROM/RAM)
        LD   A,(HL)     ; value from memory addressed by HL into A
        LD   A,(a16)    ; value from memory with address a16 into A
        LD   HL,(a16)   ; 16b value from memory with address a16 into HL
        POP  IX         ; 16b value popped from stack into IX
        LD   A,(IY+a8)  ; IX and IY allows addressing with 8b offset
        ; from I/O port (for writing value at I/O port use "OUT")
        IN   A,(C)      ; reads I/O port C, value goes to A

Correct combinations of possible source and destination operands are limited (for example `LD H,(a16)` does not exist).

---
Storing value into a memory:

        LD   (HL),D     ; value D stored into memory addressed by HL
        LD   (a16),A    ; value A into memory with address a16
        LD   (a16),HL   ; value HL into 16b of memory with address a16
        LD   (IX+a8),d8 ; value d8 into memory at address IX+a8
        LD   (IY+a8),B  ; value B into memory at address IY+a8
        ; specials ;)
        PUSH DE         ; 16b value DE pushed to stack
        CALL a16        ; while primarily used for execution branching
          ; it also stores next instruction address into stack


## x86 Registers
In the 32-bit world, the general-purpose registers fall into three
general classes: the 16-bit general-purpose registers, the 32-bit extended
general-purpose registers, and the 8-bit register halves. These three classes
do not represent three entirely distinct sets of registers at all. The 16-bit and
8-bit registers are actually names of regions *inside* the 32-bit registers. Register
growth in the x86 CPU family has come about by *extending* registers existing in
older CPUs

There are eight 16-bit general-purpose registers: AX, BX, CX, DX, BP, SI, DI,
and SP; and you can place any value in them that may be expressed in 16 bits or fewer.

When Intel expanded the x86 architecture to 32 bits in 1986, it doubled the size
of all eight registers and gave them new names by prefixing an E in front of
each register name, resulting in EAX, EBX, ECX, EDX, EBP, ESI, EDI, and ESP.

With x86_64 came another doubling of register size, as well as the addition of some new registers. These registers are 64 bits wide and are named (slash used to show alternate register name): RAX/r0, RBX/r3, RCX/r1, RDX/r2, RBP/r5, RSI/r6, RDI/r7, RSP/r4, R8, R9, R10, R11, R12, R13, R14, R15.

While the general purpose registers can be technically used for anything, each register also has an alternate/main purpose:

- AX (accumulator) is used in arithmetic operations.
- CX (counter) is used in the shift and rotate instructions, and used for loops.
- DX (data) is used in arithmetic and I/O operations.
- BX (base) used as a pointer to data (specifically as an offset to the DS segment register when in segmented mode).
- SP (stack) points to the top of the stack.
- BP (stack base) points to the base of the stack.
- SI (source) points to a source in memory for stream operations (e.g. `lodsb`).
- DI (destination) points to a destination in memory for stream operations (e.g. `stosb`).

<hr>

Segment registers, used in segmented mode, point to different segments in memory. Each 16-bit segment register gives a view to 64k (16 bits) of data. After a segment register has been set to point to a block of memory, registers (such as `BX`, `SI`, and `DI`) can be used as offsets to the segment register so specific locations in the 64k space can be accessed.

The six segment registers and their uses are:

| Register | Full name | Description |
| ------ | ----- | ------ |
| SS   | Stack Segment | Points to the stack |
| CS | Code Segment | Used by the CPU to fetch the code |
| DS | Data Segment | Default register for MOV operations |
| ES | Extra Segment | Extra data segment |
| FS | Extra Segment | Extra data segment |
| GS | Extra Segment | Extra data segment |

