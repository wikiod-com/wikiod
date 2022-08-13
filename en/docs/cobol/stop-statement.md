---
title: "STOP statement"
slug: "stop-statement"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

The `STOP` statement terminates the current run unit.

A now deemed obsolete extension to `STOP RUN` is `STOP literal`, which will pause a program until a response from the console is given, at which point execution will resume.  This could be handy for things like, "go get the big box of paper and load up the special printer".

`STOP` is a hard program end, `GOBACK` is a slightly nicer way of returning to the operating system or caller module, especially in subroutines that may have no business terminating a run.

[![STOP statement syntax diagram][1]][1]


  [1]: https://i.stack.imgur.com/2TO2Y.png

## STOP RUN
    STOP RUN

