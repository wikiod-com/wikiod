---
title: "IF statement"
slug: "if-statement"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The conditional expression and selection statement.  Use of explicit scope terminators is recommended.  COBOL conditional expressions allow shortforms, where the current identifier (and conditional) is assumed through multiple condition tests, unless explicitly given.

    IF A = 1 OR 2 ...

is equivalent to

    IF A = 1 OR A = 2 ...

[![IF statement syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/Nkbbz.png

## IF with shortform conditionals
    IF A = 1 OR 2 THEN
        perform miracles
    END-IF

    IF A = 1 OR 2 AND B = 1 THEN
        perform rites-of-passage
    ELSE
        perform song-and-dance
    END-IF

`IF` statements can be terminated with full stop or explicit scope terminator `END-IF`.  *Use of periods for scope termination is no longer recommended.*  Full stops mean just that in the case of nested IF, all nesting is terminated at the first full stop `.`, and any subsequent code will be outside the IF block.

