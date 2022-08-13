---
title: "EVALUATE statement"
slug: "evaluate-statement"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

The `EVALUATE` statement is a multiple branch, multiple join, conditional test and selection structure.

[![EVALUATE statement syntax diagram][1]][1] 


  [1]: http://i.stack.imgur.com/odJev.png

## A three condition EVALUATE
    EVALUATE a ALSO b ALSO TRUE
        WHEN 1 ALSO 1 THRU 9 ALSO c EQUAL 1 PERFORM all-life
        WHEN 2 ALSO 1 THRU 9 ALSO c EQUAL 2 PERFORM life
        WHEN 3 THRU 9 ALSO 1 ALSO c EQUAL 9 PERFORM disability
        WHEN OTHER PERFORM invalid
    END-EVALUATE

