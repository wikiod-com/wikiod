---
title: "COMPUTE statement"
slug: "compute-statement"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

The COMPUTE statement allows for algebraic calculation expressions.

[![compute statement][1]][1]


Rounded phrase is

[![rounded phrase][2]][2]


  [1]: http://i.stack.imgur.com/inJFi.png
  [2]: http://i.stack.imgur.com/Gnep0.png

## Advice: Use spaces around all components
    COMPUTE answer = 3*var-1

That is a reference to the variable `var-1`, and not `var - 1`.

    COMPUTE answer = 3 * var - 1

Recommended, *opinion*.


