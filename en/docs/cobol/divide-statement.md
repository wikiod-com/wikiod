---
title: "DIVIDE statement"
slug: "divide-statement"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The COBOL `DIVIDE` statement divides one numeric item into others setting data items to the quotient and, optionally, the remainder.

[![enter image description here][1]][1]


`ROUNDED` phrase:

Default is `TRUNCATION` if no rounded phrase specified.  Default `ROUNDED` mode is `NEAREST-TOWARD-ZERO` (rounding down) unless other specified.  So called *Banker's rounding* is `NEAREST-EVEN`.

[![ROUNDED phrase][2]][2]

  [1]: http://i.stack.imgur.com/86Pc8.png
  [2]: http://i.stack.imgur.com/VzagT.png



## DIVIDE statement formats
    DIVIDE a INTO b c d

Data item `b`, `c`, and `d` are changed as `b/a`, `c/a` and `d/a`.

    DIVIDE a INTO b GIVING c

Data item `c` is changed as `b/a`.

    DIVIDE a BY b GIVING c

Data item `c` is changed as `a/b`.

    DIVIDE a INTO b GIVING q REMAINDER r

Data items `q` and `r` are set with results of `b/a`

    DIVIDE a BY b GIVING q REMAINDER r

Data items `q` and `r` are set with results of `b/a`

All `DIVIDE` result fields may have `ROUNDED MODE IS` clauses.

All `DIVIDE` statements may have `ON SIZE ERROR` and `NOT ON SIZE ERROR` declarative statements included to catch invalid results given the type and size of the result fields.

