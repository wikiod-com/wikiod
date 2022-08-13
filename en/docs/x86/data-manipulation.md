---
title: "Data Manipulation"
slug: "data-manipulation"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
- ***.386***: Tells **MASM** to compile for a minimum x86 chip version of 386.  
- ***.model***: Sets memory model to use, see [.MODEL][3].  
- ***.code***: Code segment, used for processes such as the main process.  
- ***proc***: Declares process.  
- ***ret***: used for exiting functions successfully, see [Working With Return Values][2].
- ***endp***: Ends process declaration.  
- ***public***: Makes process available to all segments of the program.  
- ***end***: Ends program, or if used with a process, such as in "**end main**", makes the process the main method.
- ***call***: Calls process and pushes its opcode onto the stack, see [Control Flow][4].
- ***ecx***: Counter register, see [registers][1].
- ***ecx***: Counter register.
- ***mul***: Multiplies value by eax

[1]: https://www.wikiod.com/x86/register-fundamentals
[2]: https://www.wikiod.com/x86/control-flow
[3]: https://msdn.microsoft.com/en-us/library/ss9fh0d6.aspx
[4]: https://en.wikibooks.org/wiki/X86_Assembly/Control_Flow#Function_Calls


**mov** is used to transfer data between the [registers][1].

[1]: https://www.wikiod.com/x86/register-fundamentals

## Using MOV to manipulate values
***Description:***

`mov` *copies* values of bits from source argument to destination argument.

Common source/destination are [registers][3], usually the fastest way to manipulate values with[in] CPU.

Another important group of source_of/destination_for values is computer memory.

Finally some immediate values may be part of the `mov` instruction encoding itself, saving time of separate memory access by reading the value together with instruction.

On x86 CPU in 32 and 64 bit mode there are rich possibilities to combine these, especially various memory addressing modes. Generally memory-to-memory copying is out limit (except specialized instructions like `MOVSB`), and such manipulation requires intermediate storage of values into register[s] first.

***Step 1:*** Set up your project to use **MASM**, see [Executing x86 assembly in Visual Studio 2015][1]  
***Step 2:*** Type in this:

    .386
    .model small
    .code

    public main
    main proc
        mov ecx, 16       ; Move immediate value 16 into ecx
        mov eax, ecx      ; Copy value of ecx into eax
        ret               ; return back to caller
            ; function return value is in eax (16)
    main endp
    end main

***Step 3:*** Compile and debug.

The program should return value `16`.


  [1]: https://www.wikiod.com/assembly/getting-started-with-assembly-language#Executing x86 assembly in Visual Studio 2015
  [3]: https://www.wikiod.com/x86/register-fundamentals

