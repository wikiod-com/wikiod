---
title: "Control Flow"
slug: "control-flow"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Testing conditions
In order to use a conditional jump a condition must be tested.  **Testing a condition** here refers only to the act of checking the flags, the actual jumping is described under [Conditional jumps](https://www.wikiod.com/x86/control-flow#Conditional jumps).  
  
x86 tests conditions by relying on the EFLAGS register, which holds a set of flags that each instruction can potentially set.  

Arithmetic instructions, like `sub` or `add`, and logical instructions, like `xor` or `and`, obviously "set the flags". This means that the flags *CF*, *OF*, *SF*, *ZF*, *AF*, *PF* are modified by those instructions. Any instruction is allowed to modify the flags though, for example `cmpxchg` modifies the *ZF*.

**Always check the instruction reference** to know which flags are modified by a specific instruction.

x86 has a set of *conditional jumps*, referred to earlier, that jump if and only if some flags are set or some are clear or both.

---

## Flags ##

Arithmetic and logical operations are very useful in setting the flags. For example after a `sub eax, ebx`, for now holding **unsigned** values, we have:

| Flag | When set| When clear |
| ------ | ------ | ----- |
| *ZF* | When result is zero.<br> *EAX - EBX = 0 ⇒ EAX = EBX*| When result is **not** zero.<br>*EAX - EBX ≠ 0 ⇒ EAX ≠ EBX* |
| *CF* | When result did need carry for the MSb.<br> *EAX - EBX < 0 ⇒ EAX < EBX*| When result did **not** need carry for the MSb.<br>*EAX - EBX ≮  0 ⇒ EAX ≮  EBX* |
| *SF* | When result MSb is set.| When result MSb is **not** set. |
| *OF* | When a signed overflow occurred. | When a signed overflow did **not** occur.
| *PF* | When the number of bits set in least significant byte of result is even.| When the number of bits set in least significant byte of result is odd. |
| *AF* | When the lower BCD digit generated a carry.<br>It is bit 4 carry. | When the lower BCD digit did **not** generate a carry.<br>It is bit 4 carry. |

---

## Non-destructive tests ##

The `sub` and `and` instructions modify their destination operand and would require two extra copies (save and restore) to keep the destination unmodified. 
  
To perform a non-destructive test there are the instructions `cmp` and `test`. 
They are identical to their destructive counterpart **except the result of the operation is discarded, and only the flags are saved**.

| Destructive | Non destructive |
| ---- | ---- |
| `sub` | `cmp` |
| `and` | `test` |


----

    test eax, eax             ;and eax, eax
                              ;ZF = 1 iff EAX is zero
    
    test eax, 03h             ;and eax, 03h
                              ;ZF = 1 if both bit[1:0] are clear
                              ;ZF = 0 if at least one of bit[1:0] is set
    
    cmp eax, 241d             ;sub eax, 241d
                              ;ZF = 1 iff EAX is 241
                              ;CF = 1 iff EAX < 241

## Signed and unsigned tests ##

The CPU gives no special meaning to register values<sup>1</sup>, sign is a programmer construct. **There is no difference when testing signed and unsigned values.** The processor computes enough flags to test the usual arithmetic relationships (equal, less than, greater than, etc.) both if the operands were to be considered signed and unsigned.

---
<sup>1</sup> Though it has some instructions that make sense only with specific formats, like two's complement. This is to make the code more efficient as implementing the algorithm in software would require a lot of code.

## Conditional jumps
Based on the state of the flags the CPU can either execute or ignore a jump. An instruction that performs a jump based on the flags falls under the generic name of *Jcc* - *Jump on Condition Code*<sup>1</sup>.

## Synonyms and terminology ##

In order to improve the readability of the assembly code, Intel defined several synonyms for the same condition code. For example, `jae`, `jnb` and `jnc` are all the same condition code *CF = 0*.

While the instruction name may give a very strong hint on when to use it or not, the only meaningful approach is to recognize the flags that need to be tested and **then** choose the instructions appropriately.   
Intel however gave the instructions names that make perfect sense when used after a `cmp` instruction. For the purposes of this discussion, `cmp` will be assumed to have set the flags before a conditional jump.


## Equality  ##

The operand are equal iff *ZF* has been set, they differ otherwise. To test for equality we need *ZF = 1*.

    je a_label           ;Jump if operands are equal
    jz a_label           ;Jump if zero (Synonym)
    
    jne a_label          ;Jump if operands are NOT equal
    jnz a_label          ;Jump if not zero (Synonym)

| Instruction| Flags|
| ------ | ------ |
| `je`, `jz`| ZF = 1|
| `jne`, `jnz`| ZF = 0|

## Greater than ##

For **unsigned operands**, the destination is greater than the source if carry was not needed, that is, if *CF = 0*. When *CF = 0* it is possible that the operands were equal, testing *ZF* will disambiguate. 

    jae a_label      ;Jump if above or equal (>=)
    jnc a_label      ;Jump if not carry (Synonym)
    jnb a_label      ;Jump if not below (Synonym)
    
    ja a_label       ;Jump if above  (>)
    jnbe a_label     ;Jump if not below and not equal (Synonym)


| Instruction | Flags|
| ------ | ------ |
| `jae`, `jnc`, `jnb`| CF = 0|
| `ja`, `jnbe`| CF = 0, ZF = 0|

For **signed operands** we need to check that *SF = 0*, unless there has been a signed overflow, in which case the resulting *SF* is reversed. Since *OF = 0* if no signed overflow occurred and 1 otherwise, we need to check that *SF = OF*.  

*ZF* can be used to implement a strict/non strict test.

    jge a_label      ;Jump if greater or equal (>=)
    jnl a_label      ;Jump if not less (Synonym)
    
    jg a_label       ;Jump if greater (>)
    jnle a_label     ;Jump if not less and not equal (Synonym)


| Instruction | Flags|
| ------ | ------ |
| `jge`, `jnl`| SF = OF |
| `jg`, `jnle`| SF = OF, ZF = 0|

## Less than ##

These use the inverted conditions of above.

    jbe a_label      ;Jump if below or equal (<=)
    jna a_label      ;Jump if not above (Synonym)
    
    jb a_label       ;Jump if below (<)
    jc a_label       ;Jump if carry (Synonym)
    jnae a_label     ;Jump if not above and not equal (Synonym)

    ;SIGNED 

    jle a_label      ;Jump if less or equal (<=)
    jng a_label      ;Jump if not greater (Synonym)
    
    jl a_label       ;Jump if less (<)
    jnge a_label     ;Jump if not greater and not equal (Synonym)

| Instruction | Flags|
| ------ | ------ |
| `jbe`, `jna`| CF = 1 or ZF = 1 |
| `jb`, `jc`, `jnae`| CF = 1 |
| `jle`, `jng`| SF != OF or ZF = 1 |
| `jl`, `jnge`| SF != OF |

## Specific flags ##
Each flag can be tested individually with `j<flag_name>` where *flag_name* does not contain the trailing *F* (for example *CF* → *C*, *PF* → *P*).

The remaining codes not covered before are:

| Instruction| Flag|
| ------ | ------ |
| `js` | SF = 1|
| `jns` | SF = 0|
| | |
| `jo` | OF = 1|
| `jno` | OF = 0|
| | |
| `jp`, `jpe` (e = even) | PF = 1|
| `jnp`, `jpo` (o = odd) | PF = 0|


## One more conditional jump (extra one) ##
One special x86 conditional jump doesn't test flag. Instead it does test value of `cx` or `ecx` register (based on current CPU address mode being 16 or 32 bit), and the jump is executed when the register contains zero.

This instruction was designed for validation of *counter* register (`cx/ecx`) ahead of `rep`-like instructions, or ahead of `loop` loops.

    jcxz  a_label   ; jump if cx (16b mode) or ecx (32b mode) is zero
    jecxz a_label   ; synonym of jcxz (recommended in source code for 32b target)

| Instruction| Register (not flag) |
| ------ | ------ |
| `jcxz`, `jecxz` | cx = 0 (16b mode) |
| `jcxz`, `jecxz` | ecx = 0 (32b mode) |


---
<sup>1</sup> Or something like that.


## Test arithmetic relations
## Unsigned integers ##

**Greater than**

    cmp eax, ebx
    ja a_label  

**Greater than or equal**

    cmp eax, ebx
    jae a_label  

**Less than**

    cmp eax, ebx
    jb a_label  

**Less than or equal**

    cmp eax, ebx
    jbe a_label 

**Equal**

    cmp eax, ebx
    je a_label 

**Not equal**

    cmp eax, ebx
    jne a_label     

## Signed integers ##

**Greater than**

    cmp eax, ebx
    jg a_label  

**Greater than or equal**

    cmp eax, ebx
    jge a_label  

**Less than**

    cmp eax, ebx
    jl a_label  

**Less than or equal**

    cmp eax, ebx
    jle a_label 

**Equal**

    cmp eax, ebx
    je a_label 

**Not equal**

    cmp eax, ebx
    jne a_label 

---

## `a_label` ##
In examples above the `a_label` is target destination for CPU when the tested condition is "true". When tested condition is "false", the CPU will continue on the next instruction following the conditional jump.

## Synonyms ## 
There are instruction synonyms that can be used to improve the readability of the code.  
For example `ja` and `jnbe` (Jump non below nor equal) are the same instruction.

## Signed unsigned companion codes ##

| Operation| Unsigned | Signed 
| ------ | ------ | ----- | 
| &gt; | `ja` | `jg`   |
| &gt;= | `jae` | `jge`   |  
| &lt; | `jb` | `jl`   |  
| &lt;= | `jbe` | `jle`   |  
| = | `je` | `je`   |  
| &ne;, !=, <> | `jne` | `jne`   |  

## Unconditional jumps
    jmp a_label                      ;Jump to a_label
    jmp bx                           ;Jump to address in BX
    jmp WORD [aPointer]              ;Jump to address in aPointer
    jmp 7c0h:0000h                   ;Jump to segment 7c0h and offset 0000h
    jmp FAR WORD [aFarPointer]       ;Jump to segment:offset in aFarPointer

---

## Relative near jumps ##

`jmp a_label` is:
* **near**  
It only specify the offset part of the *logical address* of destination. The segment is assumed to be `CS`.
* **relative**  
The instruction semantic is jump *rel* bytes forward<sup>1</sup> from next instruction address or `IP = IP + rel`.

The instruction is encoded as either `EB <rel8>` or `EB <rel16/32>`, the assembler picking up the most appropriate form, usually preferring a shorter one.  
Per assembler overriding is possible, for example with NASM `jmp SHORT a_label`, `jmp WORD a_label` and `jmp DWORD a_label` generate the three possible forms.

## Absolute indirect near jumps ##

`jmp bx` and `jmp WORD [aPointer]` are:

* **near**  
They only specify the offset part of the logical address of destination. The segment is assumed to be `CS`.
* **absolute indirect**  
The semantic of the instructions is jump to the address in *reg* or *mem* or `IP = reg`, `IP = mem`.

The instruction is encoded as `FF /4`, for memory indirect the size of the operand is determined as for every other memory access.

## Absolute far jumps ##

`jmp 7c0h:0000h` is:

* **far**  
It specifies both parts of the *logical* address: the segment and the offset.  

* **absolute**
The semantic of the instruction is jump to the address *segment:offset* or `CS = segment, IP = offset`.

The instruction is encoded as `EA <imm32/48>` depending on the code size.  
It is possible to choose between the two forms in some assembler, for example with NASM `jmp 7c0h: WORD 0000h` and `jmp 7c0h: DWORD 0000h` generate the first and second form.

## Absolute indirect far jumps ##
`jmp FAR WORD [aFarPointer]` is:

* **far**
It specifies both parts of the *logical* address: the segment and the offset. 

* **Absolute indirect**
The semantic of the instruction is jump to the *segment:offset* stored in *mem*<sup>2</sup> or `CS = mem[23:16/32], IP = [15/31:0]`.  

The instruction is encoded as `FF /5`, the size of the operand can be controller with the size specifiers.  
In NASM, a little bit non intuitive, they are `jmp FAR WORD [aFarPointer]` for a *16:16* operand and `jmp FAR DWORD [aFarPointer]` for a *16:32* operand.

---

## Missing jumps ##

* **near absolute**  
Can be emulated with a near indirect jump. 

        mov bx, target            ;BX = absolute address of target
        jmp bx

* **far relative**  
Make no sense or too narrow of use anyway.

---

<sup>1</sup> Two complement is used to specify a signed offset and thus jump backward.  
<sup>2</sup> Which can be a *seg16:off16* or a *seg16:off32*, of sizes *16:16* and *16:32*.


