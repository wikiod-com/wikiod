---
title: "Array formulas"
slug: "array-formulas"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Sum of Product of Ranges
In this example, the total cost of buying food items is found by taking the number of each item and multiplying it by its  cost and then adding all those values together.

[![Sum Product][1]][1]

Instead of creating a separate column for `Number` times `Price` and then summing the values in that new column, we can calculate the total price directly using an array formula:

    =SUM(B2:B6*C2:C6)

Since this is an array formula, it must be entered using <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Enter</kbd> in order for Excel to treat it as such (otherwise will return `#VALUE!`). Notice that f you see curly brackets `{...}` around the formula in the formula bar, then you know it is being evaluated as an array formula.

This is how this formula is evaluated step-by-step:

    = SUM(B2:B6*C2:C6)
    = SUM({6, 8, 2, 20, 3} * {0.55, 0.25, 0.89, 0.12, 1.23})
    = SUM({6 * 0.55, 8 * 0.25, 2 * 0.89, 20 * 0.12, 3 * 1.23})
    = SUM({3.30, 2.00, 1.78, 2.40, 3.69})
    = 3.30 + 2.00 + 1.78 + 2.40 + 3.69
    = 13.17

Another way to do this is to use the `SUMPRODUCT` function:

    =SUMPRODUCT(B2:B6,C2:C6)

Note: In this case, using <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Enter</kbd> is not necessary.

  [1]: http://i.stack.imgur.com/2zIlb.png

