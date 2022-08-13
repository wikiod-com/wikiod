---
title: "Getting started with gcc"
slug: "getting-started-with-gcc"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## "Hello world!" with common command line options
For programs with a single source file, using gcc is simple.

    /* File name is hello_world.c */
    #include <stdio.h>

    int main(void)
    {
        int i;
        printf("Hello world!\n");
    }

To compile the file hello_world.c from the command line:

    gcc hello_world.c

gcc will then compile program and output the executable to the file a.out.  If you want to name the executable, use the -o option.

    gcc hello_world.c -o hello_world

The executable will then be named hello_world instead of a.out.  By default, there are not that many warnings that are emitted by gcc.  gcc has many warning options and it is a good idea to look through the gcc documentation to learn what is available.  Using '-Wall' is a good starting point and covers many common problems.

    gcc -Wall hello_world.c -o hello_world

Output:
    
    hello_world.c: In function ‘main’:
    hello_world.c:6:9: warning: unused variable ‘i’ [-Wunused-variable]
         int i;
             ^
Here we see we now get a warning that the variable 'i' was declared but not used at all in the function.

If you plan to use a debugger for testing your program, you'll need to tell gcc to include debugging information.  Use the '-g' option for debugging support.

    gcc -Wall -g hello_world.c -o hello_world

hello_world now has debugging information present supported by GDB.  If you use a different debugger, you may need to use different debugging options so the output is formatted correctly.  See the official gcc documentation for more debugging options.

By default gcc compiles code so that it is easy to debug.  gcc can optimize the output so that the final executable produces the same result but has faster performance and may result in a smaller sized executable.  The '-O' option enables optimization.  There are several recognized qualifiers to add after the O to specify the level of optimization.  Each optimization level adds or removes a set list of command line options.  '-O2', '-Os', '-O0' and '-Og' are the most common optimization levels.

    gcc -Wall -O2 hello_world.c -o hello_world

'-O2' is the most common optimization level for production-ready code.  It provides an excellent balance between performance increase and final executable size.

    gcc -Wall -Os hello_world.c -o hello_world

'-Os' is similar to '-O2', except certain optimizations that may increase execution speed by increasing the executable size are disabled.  If the final executable size matters to you, try '-Os' and see if there is a noticeable size difference in the final executable.

    gcc -Wall -g -Og hello_world.c -o -hello_world

Note that in the above examples with '-Os' and '-O2', the '-g' option was removed.  That is because when when you start telling the compiler to optimize the code, certain lines of code may in essence no longer exist in the final executable making debugging difficult.  However, there are also cases where certain errors occur only when optimizations are on.  If you want to debug your application and have the compiler optimize the code, try the '-Og' option.  This tells gcc to perform all optimizations that should not hamper the debugging experience.

    gcc -Wall -g -O0 hello_world.c -o hello_world

'-O0' performs even less optimizations than '-Og'.  This is the optimization level gcc uses by default.  Use this option if you want to make sure that optimizations are disabled.

## Determine gcc version
When referring to gcc's documentation, you should know which version of gcc you are running.  The GCC project has a manual for each version of gcc which includes features that are implemented in that version.  Use the '-v' option to determine the version of gcc you are running.

    gcc -v

Example Output:

    Using built-in specs.
    COLLECT_GCC=/usr/bin/gcc
    COLLECT_LTO_WRAPPER=/usr/libexec/gcc/x86_64-redhat-linux/5.3.1/lto-wrapper
    Target: x86_64-redhat-linux
    Configured with: ../configure --enable-bootstrap --enable-languages=c,c++,objc,obj-c++,fortran,ada,go,lto --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --with-bugurl=http://bugzilla.redhat.com/bugzilla --enable-shared --enable-threads=posix --enable-checking=release --enable-multilib --with-system-zlib --enable-__cxa_atexit --disable-libunwind-exceptions --enable-gnu-unique-object --enable-linker-build-id --with-linker-hash-style=gnu --enable-plugin --enable-initfini-array --disable-libgcj --with-default-libstdcxx-abi=gcc4-compatible --with-isl --enable-libmpx --enable-gnu-indirect-function --with-tune=generic --with-arch_32=i686 --build=x86_64-redhat-linux
    Thread model: posix
    gcc version 5.3.1 20160406 (Red Hat 5.3.1-6) (GCC)

In this example we see that we are running gcc version 5.3.1.  You would then know to refer to the GCC 5.3 manual.  It is also helpful to include your gcc version when asking questions in case you have a version specific problem.

