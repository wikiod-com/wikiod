---
title: "Constants"
slug: "constants"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Go supports constants of character, string, boolean, and numeric values.

## Declaring a constant
Constants are declared like variables, but using the `const` keyword:

    const Greeting string = "Hello World"
    const Years int = 10
    const Truth bool = true

Like for variables, names starting with an upper case letter are exported (_public_), names starting with lower case are not.

    // not exported
    const alpha string = "Alpha"
    // exported
    const Beta string = "Beta"

Constants can be used like any other variable, except for the fact that the value cannot be changed. Here's an example:

    package main
    
    import (
        "fmt"
        "math"
    )
    
    const s string = "constant"
    
    func main() {
        fmt.Println(s) // constant
    
        // A `const` statement can appear anywhere a `var` statement can.
        const n = 10
        fmt.Println(n)                           // 10
        fmt.Printf("n=%d is of type %T\n", n, n) // n=10 is of type int
    
        const m float64 = 4.3
        fmt.Println(m) // 4.3
    
        // An untyped constant takes the type needed by its context.
        // For example, here `math.Sin` expects a `float64`.
        const x = 10
        fmt.Println(math.Sin(x)) // -0.5440211108893699
    }

[Playground](https://play.golang.org/p/MI48yM88dE)

## Multiple constants declaration
You can declare multiple constants within the same `const` block:

    const (
        Alpha = "alpha"
        Beta  = "beta"
        Gamma = "gamma"
    )

And automatically increment constants with the `iota` keyword:

    const (
        Zero = iota // Zero == 0
        One         // One  == 1
        Two         // Two  == 2
    )

For more examples of using `iota` to declare constants, see https://www.wikiod.com/go/iota

You can also declare multiple constants using the multiple assignment. However, this syntax may be harder to read and it is generally avoided.

    const Foo, Bar = "foo", "bar"

## Typed vs. Untyped Constants
Constants in Go may be typed or untyped. For instance, given the following string literal:

    "bar"

one might say that the type of the literal is `string`, however, this is not semantically correct. Instead, literals are *Untyped string constants*. It is a string (more correctly, its *default type* is `string`), but it is not a Go **value** and therefore has no type until it is assigned or used in a context that is typed. This is a subtle distinction, but a useful one to understand.

Similarly, if we assign the literal to a constant:

    const foo = "bar"

It remains untyped since, by default, constants are untyped. It is possible to declare it as a *typed string constant* as well:

    const typedFoo string = "bar"

The difference comes into play when we attempt to assign these constants in a context that does have type. For instance, consider the following:

    var s string
    s = foo      // This works just fine
    s = typedFoo // As does this

    type MyString string
    var mys MyString
    mys = foo      // This works just fine
    mys = typedFoo // cannot use typedFoo (type string) as type MyString in assignment

