---
title: "Getting started with Intel x86 Assembly Language & Microarchitecture"
slug: "getting-started-with-intel-x86-assembly-language--microarchitecture"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## x86 Linux Hello World Example
This is a basic Hello World program in NASM assembly for 32-bit x86 Linux, using system calls directly (without any libc function calls).  It's a lot to take in, but over time it will become understandable. Lines starting with a semicolon(`;`) are comments.

If you don't already know low-level Unix systems programming, you might want to just write functions in asm and call them from C or C++ programs.  Then you can just worry about learning how to handle registers and memory, without also learning the POSIX system-call API and the ABI for using it.

---

This makes two system calls: [`write(2)`][1] and [`_exit(2)`][2] (not the [`exit(3)`][3] libc wrapper that flushes stdio buffers and so on).  (Technically, `_exit()` calls sys_exit_group, not sys_exit, but that only [matters in a multi-threaded process][4].)  See also [`syscalls(2)`][5] for documentation about system calls in general, and the difference between making them directly vs. using the libc wrapper functions.

In summary, system calls are made by placing the args in the appropriate registers, and the system call number in `eax`, then running an `int 0x80` instruction.  See also [What are the return values of system calls in Assembly?][6] for more explanation of how the asm syscall interface is documented with mostly C syntax.

The syscall call numbers for the 32-bit ABI are in `/usr/include/i386-linux-gnu/asm/unistd_32.h` (same contents in `/usr/include/x86_64-linux-gnu/asm/unistd_32.h`).

`#include <sys/syscall.h>` will ultimately include the right file, so you could run `echo '#include <sys/syscall.h>' | gcc -E - -dM | less` to see the macro defs (see [this answer for more about finding constants for asm in C headers][7])

---

    section .text             ; Executable code goes in the .text section
    global _start             ; The linker looks for this symbol to set the process entry point, so execution start here
    ;;;a name followed by a colon defines a symbol.  The global _start directive modifies it so it's a global symbol, not just one that we can CALL or JMP to from inside the asm.
    ;;; note that _start isn't really a "function".  You can't return from it, and the kernel passes argc, argv, and env differently than main() would expect.
     _start:
        ;;; write(1, msg, len);
        ; Start by moving the arguments into registers, where the kernel will look for them
        mov     edx,len       ; 3rd arg goes in edx: buffer length
        mov     ecx,msg       ; 2nd arg goes in ecx: pointer to the buffer
        ;Set output to stdout (goes to your terminal, or wherever you redirect or pipe)
        mov     ebx,1         ; 1st arg goes in ebx: Unix file descriptor. 1 = stdout, which is normally connected to the terminal.

        mov     eax,4         ; system call number (from SYS_write / __NR_write from unistd_32.h).
        int     0x80          ; generate an interrupt, activating the kernel's system-call handling code.  64-bit code uses a different instruction, different registers, and different call numbers.
        ;; eax = return value, all other registers unchanged.

        ;;;Second, exit the process.  There's nothing to return to, so we can't use a ret instruction (like we could if this was main() or any function with a caller)
        ;;; If we don't exit, execution continues into whatever bytes are next in the memory page,
        ;;; typically leading to a segmentation fault because the padding 00 00 decodes to  add [eax],al.

        ;;; _exit(0);
        xor     ebx,ebx       ; first arg = exit status = 0.  (will be truncated to 8 bits).  Zeroing registers is a special case on x86, and mov ebx,0 would be less efficient.
                          ;; leaving out the zeroing of ebx would mean we exit(1), i.e. with an error status, since ebx still holds 1 from earlier.
        mov     eax,1         ; put __NR_exit into eax
        int     0x80          ;Execute the Linux function

    section     .rodata       ; Section for read-only constants

                 ;; msg is a label, and in this context doesn't need to be msg:.  It could be on a separate line.
                 ;; db = Data Bytes: assemble some literal bytes into the output file.
    msg     db  'Hello, world!',0xa     ; ASCII string constant plus a newline (0x10)

                 ;;  No terminating zero byte is needed, because we're using write(), which takes a buffer + length instead of an implicit-length string.
                 ;; To make this a C string that we could pass to puts or strlen, we'd need a terminating 0 byte. (e.g. "...", 0x10, 0)

    len     equ $ - msg       ; Define an assemble-time constant (not stored by itself in the output file, but will appear as an immediate operand in insns that use it)
                              ; Calculate len = string length.  subtract the address of the start
                              ; of the string from the current position ($)
      ;; equivalently, we could have put a str_end: label after the string and done   len equ str_end - str

On Linux, you can save this file as `Hello.asm` and build a 32-bit executable from it with these commands:

    nasm -felf32 Hello.asm                  # assemble as 32-bit code.  Add -Worphan-labels -g -Fdwarf  for debug symbols and warnings
    gcc -nostdlib -m32 Hello.o -o Hello     # link without CRT startup code or libc, making a static binary

See [this answer][8] for more details on building assembly into 32 or 64-bit static or dynamically linked Linux executables, for NASM/YASM syntax or GNU AT&T syntax with GNU `as` directives.  (Key point: make sure to use `-m32` or equivalent when building 32-bit code on a 64-bit host, or you will have confusing problems at run-time.)

You can trace it's execution with `strace` to see the system calls it makes:

    $ strace ./Hello 
    execve("./Hello", ["./Hello"], [/* 72 vars */]) = 0
    [ Process PID=4019 runs in 32 bit mode. ]
    write(1, "Hello, world!\n", 14Hello, world!
    )         = 14
    _exit(0)                                = ?
    +++ exited with 0 +++

The trace on stderr and the regular output on stdout are both going to the terminal here, so they interfere in the line with the `write` system call.  Redirect or trace to a file if you care.  Notice how this lets us easily see the syscall return values without having to add code to print them, and is actually even easier than using a regular debugger (like gdb) for this.

The x86-64 version of this program would be extremely similar, passing the same args to the same system calls, just in different registers.  And using the `syscall` instruction instead of `int 0x80`.


  [1]: http://man7.org/linux/man-pages/man2/write.2.html
  [2]: http://man7.org/linux/man-pages/man2/_exit.2.html
  [3]: http://man7.org/linux/man-pages/man3/exit.3.html
  [4]: http://stackoverflow.com/questions/38434609/why-do-i-get-a-zombie-when-i-link-assembly-code-without-stdlib
  [5]: http://man7.org/linux/man-pages/man2/syscalls.2.html
  [6]: http://stackoverflow.com/q/38751614/224132
  [7]: http://stackoverflow.com/q/38602525/224132
  [8]: http://stackoverflow.com/questions/36861903/assembling-32-bit-binaries-on-a-64-bit-system-gnu-toolchain/36901649#36901649

## x86 Assembly Language
The family of x86 assembly languages represents decades of advances on the original Intel 8086 architecture. In addition to there being several different dialects based on the assembler used, additional processor instructions, registers and other features have been added over the years while still remaining backwards compatible to the 16-bit assembly used in the 1980s.

The first step to working with x86 assembly is to determine what the goal is. If you are seeking to write code within an operating system, for example, you will want to additionally determine whether you will choose to use a stand-alone assembler or built-in inline assembly features of a higher level language such as C. If you wish to code down on the "bare metal" without an operating system, you simply need to install the assembler of your choice and understand how to create binary code that can be turned into flash memory, bootable image or otherwise be loaded into memory at the appropriate location to begin execution.

A very popular assembler that is well supported on a number of platforms is NASM (Netwide Assembler), which can be obtained from http://nasm.us/. On the NASM site you can proceed to download the latest release build for your platform.

**Windows**

Both 32-bit and 64-bit versions of NASM are available for Windows. NASM comes with a convenient installer that can be used on your Windows host to install the assembler automatically.

**Linux**

It may well be that NASM is already installed on your version of Linux. To check, execute:

    nasm -v

If the command is not found, you will need to perform an install. Unless you are doing something that requires bleeding edge NASM features, the best path is to use your built-in package management tool for your Linux distribution to install NASM. For example, under Debian-derived systems such as Ubuntu and others, execute the following from a command prompt:

    sudo apt-get install nasm

For RPM based systems, you might try:

    sudo yum install nasm

**Mac OS X**

Recent versions of OS X (including Yosemite and El Capitan) come with an older version of NASM pre-installed. For example, El Capitan has version 0.98.40 installed. While this will likely work for almost all normal purposes, it is actually quite old. At this writing, NASM version 2.11 is released and 2.12 has a number of release candidates available.

You can obtain the NASM source code from the above link, but unless you have a specific need to install from source, it is far simpler to download the binary package from the OS X release directory and unzip it.

Once unzipped, it is strongly recommended that you *not* overwrite the system-installed version of NASM. Instead, you might install it into /usr/local:

     $ sudo su
     <user's password entered to become root>
     # cd /usr/local/bin
     # cp <path/to/unzipped/nasm/files/nasm> ./
     # exit

At this point, NASM is in `/usr/local/bin`, but it is not in your path. You should now add the following line to the end of your profile:

     $ echo 'export PATH=/usr/local/bin:$PATH' >> ~/.bash_profile

This will prepend `/usr/local/bin` to your path. Executing `nasm -v` at the command prompt should now display the proper, newer, version.


