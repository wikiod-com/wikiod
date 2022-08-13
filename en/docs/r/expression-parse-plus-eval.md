---
title: "Expression parse + eval"
slug: "expression-parse-+-eval"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The function `parse` convert text and files into expressions.

The function `eval` evaluate expressions.

## Execute code in string format
In this exemple, we want to execute code which is stored in a string format.
  
    # the string
    str <- "1+1"

    # A string is not an expression.
    is.expression(str)
    [1] FALSE

    eval(str)
    [1] "1+1"

    # parse convert string into expressions
    parsed.str <- parse(text="1+1")

    is.expression(parsed.str)
    [1] TRUE

    eval(parsed.str)
    [1] 2


