---
title : C Language Tutorial
slug : c-language-tutorial
weight : 8152
draft : false
images : []
type : docs
---

C is a general-purpose, imperative computer programming language, supporting structured programming, lexical variable scope and recursion, while a static type system prevents many unintended operations. By design, C provides constructs that map efficiently to typical machine instructions, and therefore it has found lasting use in applications that had formerly been coded in assembly language, including operating systems, as well as various application software for computers ranging from supercomputers to embedded systems.

Despite its low-level capabilities, the language was designed to encourage cross-platform programming. A standards-compliant and portably written C program can be compiled for a very wide variety of computer platforms and operating systems with few changes to its source code. The language has become available on a very wide range of platforms, from embedded microcontrollers to supercomputers.


C was originally developed by Dennis Ritchie between 1969 and 1973 at Bell Labs and used to re-implement the [Unix][1] operating systems. It has since become one of the most widely used programming languages of all time, with C compilers from various vendors available for the majority of existing computer architectures and operating systems.

Common Compilers
----------------

The process to compile a C program differs between compilers and operating systems. Most operating systems ship without a compiler, so you will have to install one. Some common compilers choices are: 

- [GCC, the GNU Compiler Collection](https://gcc.gnu.org/)
- [clang: a C language family front-end for LLVM](http://clang.llvm.org/)
- [MSVC, Microsoft Visual C/C++ build tools](http://landinghub.visualstudio.com/visual-cpp-build-tools)

The following documents should give you a good overview on how to get started using a few of the most common compilers:

- [Getting started with Microsoft Visual C](https://msdn.microsoft.com/en-us/library/bb384838.aspx)
- [Getting started with GCC](https://www3.ntu.edu.sg/home/ehchua/programming/cpp/gcc_make.html)

### Compiler C version Support ###
Note that compilers have varying levels of support for standard C with many still not completely supporting C99. For example, as of the 2015 release, MSVC supports much of C99 yet still has some important exceptions for support of the language itself (e.g the preprocessing seems non-conformant) and for the C library (e.g. `<tgmath.h>`), nor do they necessarily document their "implementation dependent choices". [Wikipedia has a table][2] showing support offered by some popular compilers.

Some compilers (notably GCC) have offered, or continue to offer, *compiler extensions* that implement additional features that the compiler producers deem necessary, helpful or believe may become part of a future C version, but that are not currently part of any C standard. As these extensions are compiler specific they can be considered to not be cross-compatible and compiler developers may remove or alter them in later compiler versions. The use of such extensions can generally be controlled by compiler flags.

Additionally, many developers have compilers that support only specific versions of C imposed by the environment or platform they are targeting. 

If selecting a compiler, it is recommended to choose a compiler that has the best support for the latest version of C allowed for the target environment.

Code style (off-topic here):
----------
Because white space is insignificant in C (that is, it does not affect the operation of the code), programmers often use white space to make the code easier to read and comprehend, this is called the *code style*. It is a set of rules and guidelines used when writing the source code. It covers concerns such as how lines should be indented, whether spaces or tabs should be used, how braces should be placed, how spaces should be used around operators and brackets, how variables should be named and so forth.

Code style is not covered by the standard and is primarily opinion based (different people find different styles easier to read), as such, it is generally considered off-topic on SO. The overriding advice on style in one's own code is that consistency is paramount - pick, or make, a style and stick to it. Suffice it to explain that there are various named styles in common usage that are often chosen by programmers rather than creating their own style. 

Some common indent styles are: K & R style, Allman style, GNU style and so on. Some of these styles have different variants. Allman, for example, is used as either regular Allman or the popular variant, Allman-8. Information on some of the popular styles may be found on [Wikipedia][3]. Such style names are taken from the standards the authors or organizations often publish for use by the many people contributing to their code, so that everyone can easily read the code when they know the style, such as the [GNU formatting guide][4] that makes up part of the [GNU coding standards][5] document.

Some common naming conventions are: UpperCamelCase, lowerCamelCase, lower_case_with_underscore, ALL_CAPS, etc. These styles are combined in various ways for use with different objects and types (e.g., macros often use ALL_CAPS style)

K & R style is generally recommended for use within SO documentation, whereas the more esoteric styles, such as Pico, are discouraged.

Libraries and APIs not covered by the C Standard (and therefore being off-topic here):
-----------------------------------------------------------------------
* [POSIX][6] API (covering for example [PThreads][7], [Sockets][8], [Signals][9])


  [1]: https://en.wikipedia.org/wiki/Unix
  [2]: https://en.wikipedia.org/wiki/C99#Implementations
  [3]: https://en.wikipedia.org/wiki/Indent_style
  [4]: https://www.gnu.org/prep/standards/html_node/Writing-C.html
  [5]: https://www.gnu.org/prep/standards/html_node/index.html
  [6]: https://www.wikiod.com/posix
  [7]: https://www.wikiod.com/posix/threads
  [8]: https://www.wikiod.com/posix/sockets
  [9]: https://www.wikiod.com/posix/signals

