---
title: "Register Fundamentals"
slug: "register-fundamentals"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## 16-bit Registers
When Intel defined the original 8086, it was a 16-bit processor with a 20-bit address bus (see below). They defined 8 general-purpose 16-bit registers - but gave them specific roles for certain instructions:

* `AX` The Accumulator register.  
Many opcodes either assumed this register, or were faster if it was specified.
* `DX` The Data register.  
This was sometimes combined as the high 16 bits of a 32-bit value with `AX` - for example, as the result of a multiply.
* `CX` The Count register.  
This was used in a number of loop-oriented instructions as the implicit counter for those loops - for example `LOOPNE` (loop if not equal) and `REP` (repeated move/compare)
* `BX` The Base register.  
This could be used to index the base of a structure in memory - none of the above registers could be used to directly index into memory.
* `SI` The Source Index register.  
This was the implicit source index into memory for certain move and compare operations.
* `DI` The Destination Index register.  
This was the implicit destination index into memory for certain move and compare operations.
* `SP` The Stack Pointer register.  
This is the least general-purpose register in the set! It pointed to the current position in the stack, which was used explicitly for `PUSH` and `POP` operations,  implicitly for `CALL` and `RET` with subroutines, and VERY implicitly during interrupts. As such, using it for anything else was hazardous to your program!
* `BP` The Base Pointer register.  
When subroutines call other subroutines, the stack holds multiple "stack frames". `BP` could be used to hold the current stack frame, and then when a new subroutine was called it coould be saved on the stack, the new stack frame created and used, and on return from the inner subroutine the old stack frame value could be restored.

## Notes:
1. The first three registers cannot be used for indexing into memory.
2. `BX`, `SI` and `DI` by default index into the current Data Segment (see below).

        MOV    AX, [BX+5]     ; Point into Data Segment
        MOV    AX, ES:[DI+5]  ; Override into Extra Segment
3. `DI`, when used in memory-to-memory operations such as `MOVS` and `CMPS`, solely uses the Extra Segment (see below). This cannot be overridden.
4. `SP` and `BP` use the Stack Segment (see below) by default.

## 32-bit registers
When Intel produced the 80386, they upgraded from a 16-bit processor to a 32-bit one. 32-bit processing means two things: both the data being manipulated was 32-bit, and the memory addresses being accessed were 32-bit. To do this, but still remain compatible with their earlier processors, they introduced whole new modes for the processor. It was either in 16-bit mode or 32-bit mode - but you could override this mode on an instruction-by-instruction basis for either data, addressing, or both!

First of all, they had to define 32-bit registers. They did this by simply extending the existing eight from 16 bits to 32 bits and giving them "extended" names with an `E` prefix: `EAX`, `EBX`, `ECX`, `EDX`, `ESI`, `EDI`, `EBP`, and `ESP`. The lower 16 bits of these registers were the same as before, but the upper halves of the registers were available for 32-bit operations such as `ADD` and `CMP`. The upper halves were not separately accessible as they'd done with the 8-bit registers.

The processor had to have separate 16-bit and 32-bit modes because Intel used the same opcodes for many of the operations: `CMP AX,DX` in 16-bit mode and `CMP EAX,EDX` in 32-bit mode had exactly the same opcodes! This meant that the same code could NOT be run in either mode:

> The opcode for "Move immediate into `AX`" is `0xB8`, followed by two bytes of the immediate value: `0xB8 0x12 0x34`

> The opcode for "Move immediate into `EAX`" is `0xB8`, followed by _**four**_ bytes of the immediate value: `0xB8 0x12 0x34 0x56 0x78`

So the assember has to know what mode the processor is in when executing the code, so that it knows to emit the correct number of bytes.

## 8-bit Registers
The first four [16-bit registers](https://www.wikiod.com/x86/register-fundamentals#16-bit Registers) could have their upper- and lower-half bytes accessed directly as their own registers:

* `AH` and `AL` are the High and Low halves of the `AX` register.
* `BH` and `BL` are the High and Low halves of the `BX` register.
* `CH` and `CL` are the High and Low halves of the `CX` register.
* `DH` and `DL` are the High and Low halves of the `DX` register.

Note that this means that altering `AH` or `AL` will immediately alter `AX` as well! Also note that any operation on an 8-bit register couldn't affect its "partner" - incrementing `AL` such that it overflowed from `0xFF` to `0x00` wouldn't alter `AH`.

https://www.wikiod.com/x86/register-fundamentals#64-bit registers also have 8-bit versions representing their lower bytes:

* `SIL` for `RSI`
* `DIL` for `RDI`
* `BPL` for `RBP`
* `SPL` for `RSP`

The same applies to registers `R8` through `R15`: their respective lower byte parts are named `R8B` – `R15B`.

## Segment Registers
# Segmentation
When Intel was designing the original 8086, there were already a number of 8-bit processors that had 16-bit capabilities - but they wanted to produce a true 16-bit processor. They also wanted to produce something better and more capable than what was already out there, so they wanted to be able to access more than the maximum of 65,536 bytes of memory implied by 16-bit addressing registers.

# Original Segment Registers
So they implemented the idea of "Segments" - a 64 kilobyte block of memory indexed by the 16-bit address registers - that could be re-based to address different areas of the total memory. To hold these segment bases, they included Segment Registers:

* `CS` The Code Segment register.  
This holds the segment of the code that is currently being executed, indexed by the implicit `IP` (Instruction Pointer) register.
* `DS` The Data Segment register.  
This holds the default segment for data being manipulated by the program.
* `ES` The Extra Segment register.  
This holds a second data segment, for simultaneous data operations across the total memory.
* `SS` The Stack Segment register.  
This holds the segment of memory that holds the current stack.

# Segment Size?
The segment registers could be any size, but making them 16 bits wide made it easy to interoperate with the other registers. The next question was: should the segments overlap, and if so, how much? The answer to that question would dictate the total memory size that could be accessed.

If there was no overlap at all, then the address space would be 32 bits - 4 gigabytes - a totally unheard-of size at the time! A more "natural" overlap of 8 bits would produce a 24-bit address space, or 16 megabytes. In the end Intel decided to save four more address pins on the processor by making the address space 1 megabyte with a 12-bit overlap - they considered this sufficiently large for the time!

# More Segment Registers!
When Intel was designing the 80386, they recognised that the existing suite of 4 Segment Registers wasn't enough for the complexity of programs that they wanted it to be able to support. So they added two more:

* `FS` The Far Segment register
* `GS` The Global Segment register

These new Segment registers didn't have any processor-enforced uses: they were merely available for whatever the programmer wanted.
> Some say that the names were chosen to simply continue the `C`, `D`, `E` theme of the existing set...



## 64-bit registers
AMD is a processor manufacturer that had licensed the design of the 80386 from Intel to produce compatible - but competing - versions. They made internal changes to the design to improve throughput or other enhancements to the design, while still being able to execute the same programs.

To one-up Intel, they came up with 64-bit extensions to the Intel 32-bit design and produced the first 64-bit chip that could still run 32-bit x86 code. Intel ended up following AMD's design in their versions of the 64-bit architecture.

The 64-bit design made a number of changes to the register set, while still being backward compatible:

* The existing general-purpose registers were extended to 64 bits, and named with an `R` prefix: `RAX`, `RBX`, `RCX`, `RDX`, `RSI`, `RDI`, `RBP`, and `RSP`.
  > Again, the bottom halves of these registers were the same `E`-prefix registers as before, and the top halves couldn't be independently accessed.
* 8 more 64-bit registers were added, and not named but merely numbered: `R8`, `R9`, `R10`, `R11`, `R12`, `R13`, `R14`, and `R15`.
  * The 32-bit low half of these registers are `R8D` through `R15D` (D for DWORD as usual).
  * The lowest 16 bits of these registers could be accessed by suffixing a `W` to the register name: `R8W` through `R15W`.
* The lowest 8 bits of _all 16_ registers could now be accessed:
  * The traditional `AL`, `BL`, `CL`, and `DL`;
  * The low bytes of the (traditionally) pointer registers: `SIL`, `DIL`, `BPL`, and `SPL`;
  * And the low bytes of the 8 new registers: `R8B` through `R15B`.
  * However, `AH`, `BH`, `CH`, and `DH` are inaccessible in instructions that use a REX prefix (for 64bit operand size, or to access R8-R15, or to access `SIL`, `DIL`, `BPL`, or `SPL`).  With a REX prefix, the machine-code bit-pattern that used to mean `AH` instead means `SPL`, and so on.  See Table 3-1 of Intel's instruction reference manual (volume 2).

Writing to a 32-bit register always zeros the upper 32 bits of the full-width register, unlike writing to an 8 or 16-bit register (which merges with the old value, which is an extra dependency for out-of-order execution).

## Flags register
When the x86 Arithmetic Logic Unit (ALU) performs operations like `NOT` and `ADD`, it flags the results of these operations ("became zero", "overflowed", "became negative") in a special 16-bit `FLAGS` register. 32-bit processors upgraded this to 32 bits and called it `EFLAGS`, while 64-bit processors upgraded this to 64 bits and called it `RFLAGS`.

# Condition Codes
But no matter the name, the register is not directly accessible (except for a couple of instructions - see below). Instead, individual flags are referenced in certain instructions, such as conditional Jump or conditional Set, known as `Jcc` and `SETcc` where `cc` means "condition code" and references the following table:

| Condition Code   | Name                | Definition |
| --------------   | ----                | ---------- |
| `E`, `Z`         | Equal, Zero         | `ZF` == 1   |
| `NE`, `NZ`       | Not Equal, Not Zero | `ZF` == 0   |
| `O`              | Overflow            | `OF` == 1   |
| `NO`             | No Overflow         | `OF` == 0   |
| `S`              | Signed              | `SF` == 1   |
| `NS`             | Not Signed          | `SF` == 0   |
| `P`              | Parity              | `PF` == 1   |
| `NP`             | No Parity           | `PF` == 0   |
| --------------   | ----                | ---------- |
| `C`, `B`, `NAE`  | Carry, Below, Not Above or Equal | `CF` == 1 |
| `NC`, `NB`, `AE` | No Carry, Not Below, Above or Equal | `CF` == 0 |
| `A`, `NBE`       | Above, Not Below or Equal | `CF`==0 and `ZF`==0 |
| `NA`, `BE`       | Not Above, Below or Equal | `CF`==1 or `ZF`==1 |
| ---------------  | ----                | ---------- |
| `GE`, `NL`       | Greater or Equal, Not Less | `SF`==`OF`  |
| `NGE`, `L`       | Not Greater or Equal, Less | `SF`!=`OF` |
| `G`, `NLE`       | Greater, Not Less or Equal | `ZF`==0 and `SF`==`OF` |
| `NG`, `LE`       | Not Greater, Less or Equal | `ZF`==1 or `SF`!=`OF` |

In 16 bits, subtracting `1` from `0` is either `65,535` or `-1` depending on whether unsigned or signed arithmetic is used - but the destination holds `0xFFFF` either way. It's only by interpreting the condition codes that the meaning is clear. It's even more telling if `1` is subtracted from `0x8000`: in unsigned arithmetic, that merely changes `32,768` into `32,767`; while in signed arithmetic it changes `-32,768` into `32,767` - a much more noteworthy overflow!

The condition codes are grouped into three blocks in the table: sign-irrelevant, unsigned, and signed. The naming inside the latter two blocks uses "Above" and "Below" for unsigned, and "Greater" or "Less" for signed. So `JB` would be "Jump if Below" (unsigned), while `JL` would be "Jump if Less" (signed).

# Accessing `FLAGS` directly
The above condition codes are useful for interpreting predefined concepts, but the actual flag bits are also available directly with the following two instructions:

* `LAHF` Load `AH` register with Flags
* `SAHF` Store `AH` register into Flags

Only certain flags are copied across with these instructions. The whole `FLAGS` / `EFLAGS` / `RFLAGS` register can be saved or restored on the stack:

* `PUSHF` / `POPF`  Push/pop 16-bit `FLAGS` onto/from the stack
* `PUSHFD` / `POPFD` Push/pop 32-bit `EFLAGS` onto/from the stack
* `PUSHFQ` / `POPFQ` Push/pop 64-bit `RFLAGS` onto/from the stack

Note that interrupts save and restore the current `[R/E]FLAGS` register automatically.

# Other Flags
As well as the ALU flags described above, the `FLAGS` register defines other system-state flags:

* `IF` The Interrupt Flag.  
This is set with the `STI` instruction to globally enable interrupts, and cleared with the `CLI` instruction to globally disable interrupts.
* `DF` The Direction Flag.  
Memory-to-memory operations such as `CMPS` and `MOVS` (to compare and move between memory locations) automatically increment or decrement the index registers as part of the instruction. The `DF` flag dictates which one happens: if cleared with the `CLD` instruction, they're incremented; if set with the `STD` instruction, they're decremented.
* `TF` The Trap Flag.
This is a debug flag. Setting it will put the processor into "single-step" mode: after each instruction is executed it will call the "Single Step Interrupt Handler", which is expected to be handled by a debugger. There are no instructions to set or clear this flag: you need to manipulate the bit while it is in memory.

## 80286 Flags
To support the new multitasking facilities in the 80286, Intel added extra flags to the `FLAGS` register:

* `IOPL` The I/O Privilege Level.  
To protect multitasking code, some tasks needed privileges to access I/O ports, while others had to be stopped from accessing them. Intel introduced a four-level Privilege scale, with `00`<sub>2</sub> being most privileged and `11`<sub>2</sub> being least. If `IOPL` was less than the current Privilege Level, any attempt to access I/O ports, or enable or disable interrups, would cause a General Protection Fault instead.
* `NT` Nested Task flag.  
This flag was set if one Task `CALL`ed another Task, which caused a context switch. The set flag told the processor to do a context switch back when the `RET` was executed.

## 80386 Flags
The '386 needed extra flags to support extra features designed into the processor.

* `RF` The Resume Flag.  
The `386 added Debug registers, which could invoke the debugger on various hardware accesses like reading, writing or executing a certain memry location. However, when the debug handler returned to execute the instruction _the access would immediately re-invoke the debug handler!_ Or at least it would if it wasn't for the Resume Flag, which is automatically set on entry into the debug handler, and automatically cleared after every instruction. If the Resume Flag is set, the Debug handler is not invoked.
* `VM` The Virtual 8086 Flag.  
To support older 16-bit code as well as newer 32-bit code, the 80386 could run 16-bit Tasks in a "Virtual 8086" mode, with the aid of a Virtual 8086 executive. The `VM` flag indicated that this Task was a Virtual 8086 Task.

## 80486 Flags
As the Intel architecture improved, it got faster through such technology as caches and super-scalar execution. That had to optimise access to the system by making assumptions. To control those assumptions, more flags were needed:

* `AC` Alignment Check flag
The x86 architecture could always access multi-byte memory values on any byte boundary, unlike some architectures which required them to be size-aligned (4-byte values needed to be on 4-byte boundaries). However, it was less efficient to do so, since multiple memory accesses were needed to access unaligned data. If the `AC` flag was set, then an unaligned access would raise an exception rather than execute the code. That way, code could be improved during development with `AC` set, but turned off for production code.

## Pentium Flags
The Pentium added more support for virtualising, plus support for the `CPUID` instruction:

* `VIF` The Virtual Interrupt Flag.  
This is a virtual copy of this Task's `IF` - whether or not this Task wants to disable interrupts, without actually affecting Global Interrupts.
* `VIP` The Virtual Interrupt Pending Flag.  
This indicates that an interrupt was virtually blocked by `VIF`, so when the Task does an `STI` a virtual interrupt can be raised for it.
* `ID` The `CPUID`-allowed Flag.  
Whether or not to allow this Task to execute the `CPUID` instruction. A Virtual monitor could disallow it, and "lie" to the requesting Task if it executes the instruction.


