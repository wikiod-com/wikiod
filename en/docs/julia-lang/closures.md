---
title: "Closures"
slug: "closures"
draft: false
images: []
weight: 9841
type: docs
toc: true
---

## Syntax
- x -> [body]
- (x, y) -> [body]
- (xs...) -> [body]

<!-- if version [lte 0.4.0] -->
In older versions of Julia, closures and anonymous functions had a runtime performance penalty. This penalty has been eliminated in 0.5.
<!-- end version if -->

## Introduction to Closures
[Functions][1] are an important part of Julia programming. They can be defined directly within modules, in which case the functions are referred to as _top-level_. But functions can also be defined within other functions. Such functions are called "[closures](https://en.wikipedia.org/wiki/Closure_(computer_programming))".

Closures capture the variables in their outer function. A top-level function can only use global variables from their module, function parameters, or local variables:

    x = 0  # global
    function toplevel(y)
        println("x = ", x, " is a global variable")
        println("y = ", y, " is a parameter")
        z = 2
        println("z = ", z, " is a local variable")
    end

A closure, on the other hand, can use all those in addition to variables from outer functions that it captures:

    x = 0  # global
    function toplevel(y)
        println("x = ", x, " is a global variable")
        println("y = ", y, " is a parameter")
        z = 2
        println("z = ", z, " is a local variable")

        function closure(v)
            println("v = ", v, " is a parameter")
            w = 3
            println("w = ", w, " is a local variable")
            println("x = ", x, " is a global variable")
            println("y = ", y, " is a closed variable (a parameter of the outer function)")
            println("z = ", z, " is a closed variable (a local of the outer function)")
        end
    end

If we run `c = toplevel(10)`, we see the result is

    julia> c = toplevel(10)
    x = 0 is a global variable
    y = 10 is a parameter
    z = 2 is a local variable
    (::closure) (generic function with 1 method)

Note that the tail expression of this function is a function in itself; that is, a closure. We can call the closure `c` like it was any other function:

    julia> c(11)
    v = 11 is a parameter
    w = 3 is a local variable
    x = 0 is a global variable
    y = 10 is a closed variable (a parameter of the outer function)
    z = 2 is a closed variable (a local of the outer function)

Note that `c` still has access to the variables `y` and `z` from the `toplevel` call — even though `toplevel` has already returned! Each closure, even those returned by the same function, closes over different variables. We can call `toplevel` again

    julia> d = toplevel(20)
    x = 0 is a global variable
    y = 20 is a parameter
    z = 2 is a local variable
    (::closure) (generic function with 1 method)
    
    julia> d(22)
    v = 22 is a parameter
    w = 3 is a local variable
    x = 0 is a global variable
    y = 20 is a closed variable (a parameter of the outer function)
    z = 2 is a closed variable (a local of the outer function)
    
    julia> c(22)
    v = 22 is a parameter
    w = 3 is a local variable
    x = 0 is a global variable
    y = 10 is a closed variable (a parameter of the outer function)
    z = 2 is a closed variable (a local of the outer function)

Note that despite `d` and `c` having the same code, and being passed the same arguments, their output is different. They are distinct closures.


  [1]: https://www.wikiod.com/julia-lang/functions

## Implementing Currying
One application of closures is to partially apply a function; that is, provide some arguments now and create a function that takes the remaining arguments. [Currying](https://en.wikipedia.org/wiki/Currying) is a specific form of partial application.

Let's start with the simple function `curry(f, x)` that will provide the first argument to a function, and expect additional arguments later. The definition is fairly straightforward:

    curry(f, x) = (xs...) -> f(x, xs...)

Once again, we use [anonymous function syntax](https://www.wikiod.com/julia-lang/functions#Anonymous functions), this time in combination with variadic argument syntax.

We can implement some basic functions in [tacit](https://en.wikipedia.org/wiki/Tacit_programming) (or point-free) style using this `curry` function.

    julia> const double = curry(*, 2)
    (::#19) (generic function with 1 method)

    julia> double(10)
    20

    julia> const simon_says = curry(println, "Simon: ")
    (::#19) (generic function with 1 method)

    julia> simon_says("How are you?")
    Simon: How are you?

Functions maintain the generism expected:

    julia> simon_says("I have ", 3, " arguments.")
    Simon: I have 3 arguments.

    julia> double([1, 2, 3])
    3-element Array{Int64,1}:
     2
     4
     6



## Function Composition
We can define a function to perform [function composition](https://en.wikipedia.org/wiki/Function_composition) using [anonymous function syntax](https://www.wikiod.com/julia-lang/functions#Anonymous functions):

    f ∘ g = x -> f(g(x))

Note that this definition is equivalent to each of the following definitions:

    ∘(f, g) = x -> f(g(x))

or

    function ∘(f, g)
        x -> f(g(x))
    end

recalling that in Julia, `f ∘ g` is just syntax sugar for `∘(f, g)`.

We can see that this function composes correctly:

    julia> double(x) = 2x
    double (generic function with 1 method)

    julia> triple(x) = 3x
    triple (generic function with 1 method)

    julia> const sextuple = double ∘ triple
    (::#17) (generic function with 1 method)

    julia> sextuple(1.5)
    9.0

<!-- if version [gte 0.5.0] -->

In version v0.5, this definition is very performant. We can look into the LLVM code generated:

    julia> @code_llvm sextuple(1)
    
    define i64 @"julia_#17_71238"(i64) #0 {
    top:
      %1 = mul i64 %0, 6
      ret i64 %1
    }

It is clear that the two multiplications have been folded into a single multiplication, and that this function is as efficient as is possible.

<!-- end version if -->

How does this higher-order function work? It creates a so-called [closure](https://en.wikipedia.org/wiki/Closure_(computer_programming)), which consists of not just its code, but also keeps track of certain variables from its scope. All functions in Julia that are not created at top-level scope are closures.

<!-- if version [gte 0.5.0] -->

One can inspect the variables closed over through the fields of the closure. For instance, we see that:

    julia> (sin ∘ cos).f
    sin (generic function with 10 methods)

    julia> (sin ∘ cos).g
    cos (generic function with 10 methods)
<!-- end version if -->


