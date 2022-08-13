---
title: "Getting started with POSIX"
slug: "getting-started-with-posix"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What is POSIX?
POSIX stands for "*Portable Operating System Interface*" and defines a set of standards to provide compatibility between different computing platforms.  The current version of the standard is IEEE 1003.1 2016 and can be accessed from the OpenGroup [POSIX specification](http://pubs.opengroup.org/onlinepubs/9699919799/toc.htm).  Previous versions include [POSIX 2004](http://pubs.opengroup.org/onlinepubs/009695399/) and [POSIX 1997](http://pubs.opengroup.org/onlinepubs/7990989775/).  The POSIX 2016 edition is essentially POSIX 2008 plus errata (there was a POSIX 2013 release too).

POSIX defines various tools interfaces, commands and APIs for UNIX-like operating systems and others.

The following are considered to be within the scope of POSIX standardization:

 * System interface (functions, macros and external variables)
 * Command interpreter, or Shell (the *sh* utility)
 * Utilities (such as *more*, *cat*, *ls*)

Outside of POSIX scope:

 * DBMS Interfaces
 * Graphical Interfaces
 * Binary code portability


## Hello World
A simple `Hello, World` program without error checking:

<!-- language: c -->

    #include <unistd.h> /* For write() and STDOUT_FILENO */
    #include <stdlib.h> /* For EXIT_SUCCESS and EXIT_FAILURE */
    
    int main(void) {
            char hello[] = "Hello, World\n";
            
            /* Attempt to write `hello` to standard output file */
            write(STDOUT_FILENO, hello, sizeof(hello) - 1);

            return EXIT_SUCCESS; 
    }

And with error checking:

<!-- language: c -->

    #include <unistd.h> /* For write() and STDOUT_FILENO */
    #include <stdlib.h> /* For EXIT_SUCCESS and EXIT_FAILURE */
    
    int main(void) {
            char hello[] = "Hello, World\n";
            ssize_t ret = 0;
            
            /* Attempt to write `hello` to standard output file */
            ret = write(STDOUT_FILENO, hello, sizeof(hello) - 1);
    
            if (ret == -1) {
                    /* write() failed. */
                    return EXIT_FAILURE;
            } else if (ret != sizeof(hello) - 1) {
                    /* Not all bytes of `hello` were written. */
                    return EXIT_FAILURE;
            }
    
            return EXIT_SUCCESS; 
    }

### Compiling and running

If the code shown above (either version) is stored in file `hello.c`, then you can compile the code into a program `hello` using either [`c99`](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/c99.html) or 
[`make`](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/make.html).  For example, in a strictly POSIX compliant mode, you might in theory compile and run the program using:

    $ make hello
    c99 -o hello hello.c
    $ ./hello
    Hello, World
    $

Most actual `make` implementations will use a different C compiler (perhaps `cc`, perhaps `gcc`, `clang`, `xlc` or some other name), and many will use more options to the compiler.  Clearly, you could simply type the command that `make` executes directly on the command line.


