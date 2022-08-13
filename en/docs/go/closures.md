---
title: "Closures"
slug: "closures"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Closure Basics
A *Closure* is a function taken together with an environment. The function is typically an anonymous function defined inside another function. The environment is the lexical scope of the enclosing function (very basic idea of a lexical scope of a function would be the scope that exists between the function's braces.)

    func g() {
        i := 0
        f := func() { // anonymous function
            fmt.Println("f called")
        }
    }

Within the body of an anonymous function (say `f`) defined within another function (say `g`), variables present in scopes of both `f` and `g` are accessible. However, it is the scope of `g` that forms the environment part of the closure (function part is `f`) and as a result, changes made to the variables in `g`'s scope retain their values (i.e. the environment persists between calls to `f`).

Consider the below function:

    func NaturalNumbers() func() int {
        i := 0
        f:= func() int { // f is the function part of closure
            i++
            return i
        }
        return f
    }

In above definition, `NaturalNumbers` has an inner function `f` which `NaturalNumbers` returns. Inside `f`, variable `i` defined within the scope of `NaturalNumbers` is being accessed.

We get a new function from `NaturalNumbers` like so:

    n := NaturalNumbers()

Now `n` is a closure. It is a function (defined by `f`) which also has an associated environment (scope of `NaturalNumbers`).

In case of `n`, the environment part only contains one variable: `i`

Since `n` is a function, it can be called:

    fmt.Println(n()) // 1
    fmt.Println(n()) // 2
    fmt.Println(n()) // 3

As evident from above output, each time `n` is called, it increments `i`. `i` starts at 0, and each call to `n` executes `i++`.

The value of `i` is retained between calls. That is, the environment, being a part of closure, persists.

Calling `NaturalNumbers` again would create and return a new function. This would initialize a new `i` within `NaturalNumbers`. Which means that the newly returned function forms another closure having the same part for function (still `f`) but a brand new environment (a newly initialized `i`).

    o := NaturalNumbers()

    fmt.Println(n()) // 4
    fmt.Println(o()) // 1
    fmt.Println(o()) // 2
    fmt.Println(n()) // 5

Both `n` and `o` are closures containing same function part (which gives them the same behavior), but different environments. Thus, use of closures allow functions to have access to a persistent environment that can be used to retain information between calls.

Another example:

    func multiples(i int) func() int {
        var x int = 0
        return func() int {
            x++
            // paramenter to multiples (here it is i) also forms
            // a part of the environment, and is retained
            return x * i
        }
    }

    two := multiples(2)
    fmt.Println(two(), two(), two()) // 2 4 6

    fortyTwo := multiples(42)
    fmt.Println(fortyTwo(), fortyTwo(), fortyTwo()) // 42 84 126

