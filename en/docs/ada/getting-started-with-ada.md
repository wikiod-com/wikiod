---
title: "Getting started with ada"
slug: "getting-started-with-ada"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Version
The standard Ada programming language is defined in the [*Ada Reference Manual*][1]. Interim version changes and release notes are discussed in the corresponding rationale documents. Implementations typically document their compliance with the standard in the form of a user guide and/or reference manual, for [example][2].

- Ada 2012

    - [*Ada 2012 Language Reference Manual*][3]
    - [*Rationale for Ada 2012*][4]

- Ada 2005

    - [*Ada 2005 Language Reference Manual*][5]
    - [*Rationale for Ada 2005*][6]

- Ada 95

    - [*Ada 95 Language Reference Manua*l][7]
    - [*Rationale for Ada 95*][8]

- Ada 83

    - [*Ada 83 Language Reference Manua*l][9] 
    - [*Ada 83 Rationale for the Design of the
Ada® Programming Language*][10]


  [1]: http://www.ada-auth.org/arm.html
  [2]: https://www.wikiod.com/ada/getting-started-with-ada#Installation or Setup
  [3]: http://www.ada-auth.org/standards/12rm/html/RM-TOC.html
  [4]: http://www.ada-auth.org/standards/12rat/html/Rat12-TOC.html
  [5]: http://www.adaic.org/resources/add_content/standards/05rm/html/RM-TOC.html
  [6]: http://www.adaic.org/resources/add_content/standards/05rat/html/Rat-TOC.html
  [7]: http://www.adaic.org/resources/add_content/standards/95lrm/ARM_HTML/RM-TOC.html
  [8]: http://www.adaic.org/resources/add_content/standards/95rat/rat95html/rat95-contents.html
  [9]: http://archive.adaic.com/standards/83lrm/html/
  [10]: http://archive.adaic.com/standards/83rat/html/

## Installation or Setup
Ada is a programming language for which there exists multiple compilers.

* One of these compilers, and perhaps the most used, is GNAT. It is part of the GCC toolchain. It can be installed from several sources:
   
  - The yearly GPL release done by AdaCore, available for free on [libre site][3]. This version has undergone all internal testing that AdaCore does for its pro releases, is available on a large number of platforms. The compiler and its runtime are released under the GPL license, and, unless you are using no runtime, any executables you distribute will also be covered by this license. For academics and projects in their initial stages, this is not a problem.

  - The FSF [gcc][1] receives the same patches regularly. The version of GNAT might not be always up-to-date, but catches up regularly.

  - A number of contributors are packaging that FSF version for various Linux distributions (Debian-based systems, among others) and [binaries][2] for Mac OS X. Using the package manager from your distribution might be the simplest way to install GNAT. Such versions come with the standard GCC license, and allow you to write closed source code.

  - AdaCore also provides [GNAT Pro][4], which comes with the standard GCC license which allows you to write closed source code. More importantly perhaps, it comes with support, should you have questions on the use of the language, tools, how to best implement something, and of course bug reports and enhancement requests.

Another [number of compilers][5] are listed in the [Ada WikiBook][.wikibook], together with installation instructions. [Getadanow.com][.getadanow] features editions of FSF GNAT, ready-made for various operating systems on several types of hardware, or virtual machines. The site also collects resources for learning and sharing Ada. 

  [1]: https://gcc.gnu.org
  [2]: https://sourceforge.net/projects/gnuada/
  [3]: https://libre.adacore.com
  [4]: https://www.adacore.com

  [5]: https://en.wikibooks.org/wiki/Ada_Programming/Installing
  [.wikibook]: https://en.wikibooks.org/wiki/Ada_Programming
  [.getadanow]: http://getadanow.com

## Hello World
    with Ada.Text_IO;
    
    procedure Hello_World is
    begin
       Ada.Text_IO.Put_Line ("Hello World");
    end Hello_World;

Alternatively, after importing the package [Ada.Text_IO][1], you can say `use Ada.Text_IO;` in order to be able to use [Put_Line][2] without explicitly declaring what package it should come from, as such:

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Hello_World is
    begin
        Put_Line ("Hello World");
    end Hello_World;

If you are using the `gnat` compiler, this simple program can be compiled with

    gnatmake hello_world

This will generate a number of files, including a `hello_world` (or `hello_world.exe` on Windows) that you can execute to see the famous message. The name of the executable is computed automatically from the name of the main Ada subprogram. In Ada a main subprogram can have any name. It only has to be a parameter-less procedure, that you give as an argument to `gnatmake`.

Other compilers have similar requirements, although of course the build command is different.


  [1]: https://www.wikiod.com/ada/package-adatext_io
  [2]: https://www.wikiod.com/ada/package-adatext_io#Put_Line

## Libraries
As for any programming language, Ada comes with extensive libraries to accomplish various tasks. Here are some pointers to some of them, although searching on github will lead some more.

- The Ada runtime itself, distributed will all compilers, includes an extensive set of packages and annexes, ranging from data structures and containers, to input/output, string manipulation, time manipulation, files, numeric computations, multi-tasking, command line switches, random numbers,...

- The GNAT compiler comes with its own extended runtime, with new packages in the `GNAT` hierarchy, that provide support for regular expressions, sorting, searching, unicode, CRC, time input/output, ...

- [gnatcoll][1] is a library that is available from AdaCore's [libre site][2], and includes an extensive logging framework, extending applications with python, mmap, an extensive framework to interface with file systems, parsing email messages and mailboxes, an extensive framework to interact with databases in a type-safe manner, interface to various libraries like icon, readline, terminal colors, support for reference counted types for automatic memory management, JSON files,...

- [XML/Ada][3] is a library to parse and validate XML documents

- [GtkAda][5] is a full binding to the gtk+ library, that let's you write portable user interfaces on Unix, Windows and OSX.

- [AWS][2] is a framework to create web servers in Ada, with full support for various protocols like HTTP, Websockets,... and its own template system.

  [1]: http://docs.adacore.com/gnatcoll-docs/
  [2]: https://libre.adacore.com/
  [3]: http://docs.adacore.com/xmlada-docs/
  [5]: http://docs.adacore.com/gtkada-docs/gtkada_rm/gtkada_rm/

