---
title: "Getting started with cobol"
slug: "getting-started-with-cobol"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, world
    HELLO * HISTORIC EXAMPLE OF HELLO WORLD IN COBOL
           IDENTIFICATION DIVISION.
           PROGRAM-ID. HELLO.
           PROCEDURE DIVISION.
               DISPLAY "HELLO, WORLD".
               STOP RUN.

The days of punch card layout and uppercase only inputs are far behind. Yet most COBOL implementations still handle the same code layout. Even current implementations follow the same (often even in uppercase,) compiled and in production.

A well-formatted modern implementation might look like:

    *> Hello, world
    identification division.
    program-id. hello.

    procedure division.
    display "Hello, world"
    goback.
    end program hello.

With some implementations of COBOL, this can be shortened to:

    display "Hello, world".

This format usually requires compile time switches to put a COBOL compiler into a relaxed syntax mode, as some of the normally mandatory `DIVISION` statements are missing.

COBOL assumes FIXED format sources by default, even in the current specification.

Pre-2002 COBOL

| Column | Area                   |
| ------ | ----                   |
| 1-6    | Sequence Number Area   |
| 7      | Indicator Area |
| 8-12   | Area A |
| 12-72  | Area B |
| 73-80  | Program Name Area |

IBM mainframe text editors are still configured for this form in some cases.

Post 2002 and into COBOL 2014, Area A and B were merged and extended to column 255, and the Program Name Area was dropped.

| Column | Area |
| ------ | ---- |
| 1-6    | Sequence Number Area   |
| 7      | Indicator Area |
| 8-     | Program text Area |

Column 8 thru an implementation defined column *Margin R*, is usually still limited to column 72, but allowed by spec to run up to column 255.

COBOL 2002 introduced `FORMAT FREE` source text.  There is no *Sequence Number Area*, no *Indicator Area*, and source lines can be any length (up to an implementation defined *Margin R* limit, usually less than 2048 characters per line, commonly 255).

But the compiler starts out in FORMAT FIXED mode by default. There is usually a compilation switch or *Compiler Directive Facility* statement before free format source is recognized.

    bbbbbb >>SOURCE FORMAT IS FREE

Where `bbbbbb` represents 6 blanks, or any other characters. (These are ignored as part of the initial default fixed format mode Sequence Number Area.)

## Install gnu-cobol on Mac OS X
gnu-cobol is available via the homebrew system.

Open a terminal window from `/Applications/Utilities/Terminal` or use the keypress `Command+Space` and type `"Terminal"`.

If you do not have the homebrew system installed, add it by typing, or copying and pasting into your terminal:

    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

Once the command has finished, type:

    brew install gnu-cobol

That is it, you can now compile Cobol programs on your Mac.



