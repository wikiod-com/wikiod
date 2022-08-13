---
title: "GOBACK statement"
slug: "goback-statement"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

The COBOL `GOBACK` statement is a return.  Unlike `EXIT PROGRAM`, or `STOP RUN`, `GOBACK` always returns one level.  If the current module is "main", `GOBACK` will return to the operating system.  If the current module is a subprogram, `GOBACK` will return to the statement after a call.

[![GOBACK statement syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/TjRBh.png

## GOBACK
    identification division.
    program-id. subprog.
    procedure division.
    display "in subprog"
    goback.
    
    ...

    call "subprog"
    goback.

The first `GOBACK` above will return from subprog.  Assuming the second is inside the main procedure, `GOBACK` will return to the operating system.    

