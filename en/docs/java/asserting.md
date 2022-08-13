---
title: "Asserting"
slug: "asserting"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
 - assert _expression1_;
 - assert _expression1_ : _expression2_;

## Parameters
| Parameter | Details |
| --------- | ------- |
| expression1 | The assertion statement throws an `AssertionError` if this expression evaluates to `false`. |
| expression2 | Optional. When used, `AssertionError`s thrown by the assert statement have this message. |

By default, assertions are disabled at runtime.

To enable assertions, you must run java with `-ea` flag.

    java -ea com.example.AssertionExample

Assertions are statements that will throw an error if their expression evaluates to `false`. Assertions should only be used to _test_ code; they should never be used in production.

## Checking arithmetic with assert
    a = 1 - Math.abs(1 - a % 2);
    
    // This will throw an error if my arithmetic above is wrong.
    assert a >= 0 && a <= 1 : "Calculated value of " + a + " is outside of expected bounds";
    
    return a;

