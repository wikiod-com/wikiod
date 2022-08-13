---
title: "Higher-Order Functions"
slug: "higher-order-functions"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
- foreach(f, xs)
- map(f, xs)
- filter(f, xs)
- reduce(f, v0, xs)
- foldl(f, v0, xs)
- foldr(f, v0, xs)

Functions can be accepted as parameters and can also be produced as return types. Indeed, functions can be created inside the body of other functions. These inner functions are known as [closures][1].


  [1]: https://www.wikiod.com/julia-lang/closures

## Functions as arguments
[Functions][1] are objects in Julia. Like any other objects, they can be passed as arguments to other functions. Functions that accept functions are known as [higher-order](https://en.wikipedia.org/wiki/Higher-order_function) functions.

For instance, we can implement an equivalent of the standard library's `foreach` function by taking a function `f` as the first parameter.

    function myforeach(f, xs)
        for x in xs
            f(x)
        end
    end
We can test that this function indeed works as we expect:

    julia> myforeach(println, ["a", "b", "c"])
    a
    b
    c

By taking a function as the _first_ parameter, instead of a later parameter, we can use Julia's do block syntax. The do block syntax is just a convenient way to pass an [anonymous function][2] as the first argument to a function.

    julia> myforeach([1, 2, 3]) do x
               println(x^x)
           end
    1
    4
    27

Our implementation of `myforeach` above is roughly equivalent to the built-in `foreach` function. Many other built-in higher order functions also exist.
    
Higher-order functions are quite powerful. Sometimes, when working with higher-order functions, the exact operations being performed become unimportant and programs can become quite abstract. [Combinators][3] are examples of systems of highly abstract higher-order functions.


  [1]: https://www.wikiod.com/julia-lang/functions
  [2]: https://www.wikiod.com/julia-lang/functions#Anonymous functions
  [3]: https://www.wikiod.com/julia-lang/combinators

## Map, filter, and reduce
Two of the most fundamental higher-order functions included in the standard library are `map` and `filter`. These functions are generic and can operate on any [iterable][1]. In particular, they are well-suited for computations on [arrays][2].

Suppose we have a dataset of schools. Each school teaches a particular subject, has a number of classes, and an average number of students per class. We can model a school with the following [immutable type][3]:

    immutable School
        subject::Symbol
        nclasses::Int
        nstudents::Int  # average no. of students per class
    end

Our dataset of schools will be a `Vector{School}`:

    dataset = [School(:math, 3, 30), School(:math, 5, 20), School(:science, 10, 5)]

Suppose we wish to find the number of students in total enrolled in a math program. To do this, we require several steps:

- we must narrow the dataset down to only schools that teach math (`filter`)
- we must compute the number of students at each school (`map`)
- and we must reduce that list of numbers of students to a single value, the sum (`reduce`)

A naïve (not most performant) solution would simply be to use those three higher-order functions directly.

    function nmath(data)
        maths = filter(x -> x.subject === :math, data)
        students = map(x -> x.nclasses * x.nstudents, maths)
        reduce(+, 0, students)
    end

and we verify there are 190 math students in our dataset:

    julia> nmath(dataset)
    190

Functions exist to combine these functions and thus improve performance. For instance, we could have used the `mapreduce` function to perform the mapping and reduction in one step, which would save time and memory.

The `reduce` is only meaningful for [associative operations](https://en.wikipedia.org/wiki/Associativity) like `+`, but occasionally it is useful to perform a reduction with a non-associative operation. The higher-order functions `foldl` and `foldr` are provided to force a particular reduction order.

  [1]: https://www.wikiod.com/julia-lang/iterables
  [2]: https://www.wikiod.com/julia-lang/arrays
  [3]: https://www.wikiod.com/julia-lang/types#Immutable Types

