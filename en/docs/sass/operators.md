---
title: "Operators"
slug: "operators"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Arithmetic Operators
Sass supports the following standard arithmetic operators:

| Operator| Description|
| ------ | ------ |
| \+    | Addition|
|–   |  Subtraction|
|\*  |   Multiplication|
|/   |  Division|
|%    | Remainder|

**Examples**

    p {
        font-size: 16px + 4px; // 20px
    }

<!-- -->
    h3 {
        width: 2px * 5 + 12px; // 22px
    }

<!-- -->

    h2 {
        width: 8px + (12px / 2) * 3; // 26px
    }

Normal order of operations applies as usual.

## Assignment Operator
Sass uses the colon (`:`) operator to assign values to variables.

**Example**

    $foreColor: red;
    
    p {
        color: $foreColor;
    }

## Comparison Operators
Sass supports all the usual comparison operators: `<`,`>`,`==`,`!=`,`<=`,`>=`.

**Examples**

`(10px == 10) // Returns true`
<!-- -->
`("3" == 3) // Returns false`
<!-- -->

    $padding: 10px;
    $padding <= 8px; // Returns false

