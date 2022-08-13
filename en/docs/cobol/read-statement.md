---
title: "READ statement"
slug: "read-statement"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The `READ` statement is a staple of COBOL transaction processing programming.  Reads data from external storage into working store.  With or without locks or sharing, sequentially, by random access, or by key.  Declarative clauses for `AT END` may also be specified, but some programmers prefer explicit `FILE STATUS` testing.

As each file resource may contain any type of record in any given slot, COBOL is a "read a file", "write a record" language, `READ` takes a filename (FD) and it is up to the programmer to place the record in an appropriate structure if heterogeneous data is saved in the file.

[![enter image description here][1]][1] 


  [1]: http://i.stack.imgur.com/kZQUR.png

## Simple READ from FD
    READ data-file

