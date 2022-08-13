---
title: "MULTIPLY statement"
slug: "multiply-statement"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

The `MULTIPLY` statement multiplies numeric data setting the result to one or more identifiers of numeric type.

[![MULTIPLY statement syntax diagram][1]][1]


Where `rounded-phrase` is

[![rounded-phrase][2]][2]


  [1]: http://i.stack.imgur.com/CG2es.png
  [2]: http://i.stack.imgur.com/ldXYi.png

## Some MULTIPLY formats
    MULTIPLY 5 BY a

    MULTIPLY a BY b
        ON SIZE ERROR
            PERFORM error-handling
        NOT ON SIZE ERROR
            PERFORM who-does-that
    END-MULTIPLY

    MULTIPLY a BY b GIVING x ROUNDED MODE IS PROHIBITED
                           y ROUNDED MODE IS NEAREST-EVEN
                           z ROUNDED

