---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
 - @function function-name(parameter) { /* Function body */ }

## Basic Functions
A function is similar in look to a mixin but it doesn't add any styles, it only returns a value. Functions should be used to prevent repeated logic in your styles.

Sass has some built-in functions that are called using the standard CSS function syntax.

    h1 {
        background: hsl(0, 25%, 50%);
    }

Functions are declared using the below syntax,

    @function multiply(x, y) {
        @return x * y;
    }

    // example use below
    h1 {
        margin-top: multiply(10px, 2);
    }

In the code above, `@function` declares a function, and `@return` signifies the return value.

