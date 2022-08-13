---
title: "While Loops"
slug: "while-loops"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
-
    while (boolean_expression) {
      block_expression
    }

-
    do { block_expression } while (boolean_expression)

## Parameters
| Parameter | Details |
| --------- | ------- |
| `boolean_expression` | Any expression that will evaluate to `true` or `false`.|
| `block_expression` | Any expression or set of expressions that will be evaluated if the `boolean_expression` evaluates to `true`. 

The primary difference between `while` and `do-while` loops is whether they execute the `block_expression` before they check to see if they should loop.

Because `while` and `do-while` loops rely on an expression to evaluate to `false` to terminate, they often require mutable state to be declared outside the loop and then modified inside the loop.

## Do-While Loops
    var line = 0
    var maximum_lines = 5
    
    do {
      line = line + 1
      println("Line number: " + line)
    } while (line < maximum_lines)

The `do`/`while` loop is infrequently used in functional programming, but  can be used to work around the lack of support for the `break`/`continue` construct, as seen in other languages:

    if(initial_condition) do if(filter) {
      ...
    } while(continuation_condition)

## While Loops
    var line = 0
    var maximum_lines = 5

    while (line < maximum_lines) {
      line = line + 1
      println("Line number: " + line)
    }

