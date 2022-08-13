---
title: "Bash Arithmetic"
slug: "bash-arithmetic"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Syntax
- $(( EXPRESSION )) - Evaluates expression and returns its result.

- expr EXPRESSION - Prints result of EXPRESSION to stdout.

## Parameters
| Parameter | Details |
| ------ | ------ |
| EXPRESSION   | Expression to evaluate   |

A space (" ") is required between each term (or sign) of the expression. "1+2" won't work, but "1 + 2" will work.

## Simple arithmetic with (( ))
    #!/bin/bash
    echo $(( 1 + 2 ))
Output: 3

    # Using variables
    #!/bin/bash
    var1=4
    var2=5
    ((output=$var1 * $var2))
    printf "%d\n" "$output"
Output: 20

## Arithmetic command


## Simple arithmetic with expr
    #!/bin/bash
    expr 1 + 2
Output: 3

