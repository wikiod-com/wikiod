---
title: "ADD statement"
slug: "add-statement"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

[![enter image description here][1]][1]


Where rounded-phase is

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/Y1j64.png
  [2]: http://i.stack.imgur.com/Ts1Q9.png

## ADD statement
    ADD 1 TO cobol

This modifies the variable `cobol`.  Overflow silently ignored.

    ADD 1 TO cobol GIVING GnuCOBOL

This doesn't modify `cobol`, the result of the ADD being stored in `GnuCOBOL`.  Again, overflow of the storage allocation silently ignored (the field will stay at its old value on size errors and there will be no exception raised).

    ADD
        a b c d f g h i j k l m n o p q r s t u v w x y z
        GIVING total-of
        ON SIZE ERROR
            PERFORM log-problem
        NOT ON SIZE ERROR
            PERFORM graph-result
    END-ADD

Multiple inputs are allowed, with storage size testing explicit.  COBOL has an intrinsic `FUNCTION E`, so it not a wise choice for a single letter identifier.

`SIZE ERROR` in COBOL is dependent on type and/or `PICTURE`.  A `PIC 9` field will only safely store values from 0 to 9, an intermediate result of 10 would trigger the `ON SIZE ERROR` phrase in that case.

