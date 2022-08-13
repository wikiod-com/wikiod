---
title: "COPY directive"
slug: "copy-directive"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

The COBOL version of the C `#include` preprocessor directive.  *Or, more historically accurate, COBOL came first, developed some 10 years earlier.*

Due to some of the design decisions in COBOL (no arguments for `PERFORM` as the primary reason), many data structure access sequences need to break the [DRY principle][1].  Names of structure components need to be repeated in the ENVIRONMENT DIVISION, the DATA DIVISION and possibly many times in the PROCEDURE DIVISION.  This is usually handled by adding copybooks.  Record declarations and access code are tucked away in separate files and the COPY statement is the only repeated source.  A change to the copybook keeps all uses of name spelling and data layout in synch, instead of requiring multiple edits to multiple files when a change occurs.

[![enter image description here][2]][2]


  [1]: https://en.wikipedia.org/wiki/Don't_repeat_yourself
  [2]: http://i.stack.imgur.com/CNHkq.png

## COPY record-layout.
program-one.

    FD important-file.
    01 file-record.
       COPY record-layout.

    DATA DIVISION.
    01 memory-record.
       COPY record-layout.

    PROCEDURE DIVISION.
       ...
       COPY record-move.
       ...  
       COPY record-move.

program-two.

       DATA DIVISION.

       01 print-record.
          COPY record-layout.
       ...

       PROCEDURE DIVISION.
       ...
       print-line.
           COPY record-move.

