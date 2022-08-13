---
title: "EXIT statement"
slug: "exit-statement"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

The COBOL `EXIT` statement is a terminating flow control verb.

`EXIT` comes is a few flavours:

- bare `EXIT` is a common end point for a series of procedures.
- `EXIT PARAGRAPH`, `EXIT SECTION` provides a means of exiting a structured procedure without executing any of the subsequent statements.
- `EXIT FUNCTION`, `EXIT METHOD`, `EXIT PROGRAM` marks the logical end of a module of code.
- `EXIT PERFORM` breaks out of a inline perform loop.
- `EXIT PERFORM CYCLE` causes an inline perform loop to begin the next iteration.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/PBpvY.png

## EXIT statement
    PERFORM VARYING counter FROM 1 BY 1 UNTIL counter > 10
        IF debug-override THEN EXIT PERFORM
        IF counter = 5 THEN EXIT PERFORM CYCLE
        PERFORM some-miracle
    END-PERFORM

