---
title: "SUBTRACT statement"
slug: "subtract-statement"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

The `SUBTRACT` statement is used to subtract one, or the sum of two or more, numeric data items from one or more items, and set the values of one or more identifiers to the result.

[![SUBTRACT statement syntax diagram][1]][1]

*rounded-phrase*

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/ZCMpk.png
  [2]: https://i.stack.imgur.com/Yyj8C.png

## SUBTRACT example
    SUBTRACT item-a item-b item-c FROM account-z ROUNDED MODE IS NEAREST-EVEN
        ON SIZE ERROR
            DISPLAY "CALL THE BOSS, Account `Z` is OUT OF MONEY" END-DISPLAY
            PERFORM promisary-processing
        NOT ON SIZE ERROR
            PERFORM normal-processing
    END-SUBTRACT

