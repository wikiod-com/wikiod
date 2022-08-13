---
title: "Pseudocode"
slug: "pseudocode"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Pseudocode is by definition informal. This topic is meant to describe ways to translate language-specific code into something everyone with a programming background can understand.

Pseudocode is an important way to describe an algorithm and is more neutral than giving a langugage-specific implementation. Wikipedia often uses some form of pseudocode when describing an algorithm

Some things, like if-else type conditions are quite easy to write down informally. But other things, js-style callbacks for instance, may be hard to turn into pseudocode for some people.

This is why these examples may prove useful

## Variable affectations
You could describe variable affectation in different ways.

Typed
-----

    int a = 1
    int a := 1
    let int a = 1
    int a <- 1

No type
-------

    a = 1
    a := 1
    let a = 1
    a <- 1



## Functions
As long as the function name, return statement and parameters are clear, you're fine.

    def incr n
        return n + 1

or

    let incr(n) = n + 1

or

    function incr (n)
        return n + 1

are all quite clear, so you may use them. Try not to be ambiguous with a variable affectation

