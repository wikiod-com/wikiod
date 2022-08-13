---
title: "CONTINUE statement"
slug: "continue-statement"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

The CONTINUE statement causes the flow of control to continue at the next statement.  Not quite a no-op, as it can influence control flow when inside compound statement sequences, in particular IF/THEN/ELSE.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/xZ1Kz.png

A handy? example is during early development and building with and without debugging aids.

    CALL "CBL_OC_DUMP" USING structure ON EXCEPTION CONTINUE END-CALL

That code, while expensive, will allow for formatted memory dumps when the module `CBL_OC_DUMP` is linked into the executable, but will harmlessly fail when it is not.  *That trick is only applicable during early stages of development. The expense of a dynamic lookup failure is not something to leave in active code, and those lines should be removed from the source as soon as any initial concerns are satisfied in alpha testing.  On first day coding, it can be a handy aid. By second day coding ON EXCEPTION CONTINUE occurrences should be wiped clean.

## Placeholder
This is contrived; but some COBOL programmers may prefer the positive clarity, versus using `NOT` in conditional expressions (especially with the logic error prone `var NOT = value OR other-value`).

     if action-flag = "C" or "R" or "U" or "D"
         continue
     else
         display "invalid action-code" upon syserr
         perform report-exception
         exit section
     end-if

