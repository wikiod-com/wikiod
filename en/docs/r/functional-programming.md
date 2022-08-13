---
title: "Functional programming"
slug: "functional-programming"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Built-in Higher Order Functions
R has a set of built in higher order functions: `Map`, `Reduce`, `Filter`, `Find`, `Position`, `Negate`.

`Map` applies a given function to a list of values:

    words <- list("this", "is", "an", "example")
    Map(toupper, words)

`Reduce` successively applies a binary function to a list of values in a recursive fashion.

    Reduce(`*`, 1:10)

`Filter` given a predicate function and a list of values returns a filtered list containing only values for whom predicate function is TRUE.

    Filter(is.character, list(1,"a",2,"b",3,"c"))

`Find` given a predicate function and a list of values returns the first value for which the predicate function is TRUE.

    Find(is.character, list(1,"a",2,"b",3,"c"))

`Position` given a predicate function and a list of values returns the position of the first value in the list for which the predicate function is TRUE.

    Position(is.character, list(1,"a",2,"b",3,"c"))

`Negate` inverts a predicate function making it return FALSE for values where it returned TRUE and vice versa.

    is.noncharacter <- Negate(is.character)
    is.noncharacter("a")
    is.noncharacter(mean)



