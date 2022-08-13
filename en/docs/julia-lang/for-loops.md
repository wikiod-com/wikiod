---
title: "for Loops"
slug: "for-loops"
draft: false
images: []
weight: 9774
type: docs
toc: true
---

## Syntax
 - for i in iter; ...; end
 - while cond; ...; end
 - break
 - continue
 - @parallel (op) for i in iter; ...; end
 - @parallel for i in iter; ...; end
 - @goto label
 - @label label

Whenever it makes code shorter and easier to read, consider using higher-order functions, such as `map` or `filter`, instead of loops.

## Find smallest prime factor
In some situations, one might want to return from a function before finishing an entire loop. The `return` statement can be used for this.

    function primefactor(n)
        for i in 2:n
            if n % i == 0
                return i
            end
        end
        @assert false  # unreachable
    end

Usage:

    julia> primefactor(100)
    2
    
    julia> primefactor(97)
    97

Loops can also be terminated early with the `break` statement, which terminates just the enclosing loop instead of the entire function.

## Multidimensional iteration
In Julia, a for loop can contain a comma (`,`) to specify iterating over multiple dimensions. This acts similarly to nesting a loop within another, but can be more compact. For instance, the below function generates elements of the [Cartesian product](https://en.wikipedia.org/wiki/Cartesian_product) of two iterables:

    function cartesian(xs, ys)
        for x in xs, y in ys
            produce(x, y)
        end
    end

Usage:

    julia> collect(@task cartesian(1:2, 1:4))
    8-element Array{Tuple{Int64,Int64},1}:
     (1,1)
     (1,2)
     (1,3)
     (1,4)
     (2,1)
     (2,2)
     (2,3)
     (2,4)

However, indexing over arrays of any dimension should be done with `eachindex`, not with a multidimensional loop (if possible):

    s = zero(eltype(A))
    for ind in eachindex(A)
        s += A[ind]
    end

## Reduction and parallel loops
Julia provides macros to simplify distributing computation across multiple machines or workers. For instance, the following computes the sum of some number of squares, possibly in parallel.

    function sumofsquares(A)
        @parallel (+) for i in A
            i ^ 2
        end
    end

Usage:

    julia> sumofsquares(1:10)
    385

For more on this topic, see the [example](https://www.wikiod.com/julia-lang/parallel-processing#@parallel) on `@parallel` within the Parallel Processesing [topic](https://www.wikiod.com/julia-lang/parallel-processing).

## Fizz Buzz
A common use case for a `for` loop is to iterate over a predefined range or collection, and do the same task for all its elements. For instance, here we combine a `for` loop with a conditional [`if`-`elseif`-`else` statement](https://www.wikiod.com/julia-lang/conditionals#if statement with multiple branches):

    for i in 1:100
        if i % 15 == 0
            println("FizzBuzz")
        elseif i % 3 == 0
            println("Fizz")
        elseif i % 5 == 0
            println("Buzz")
        else
            println(i)
        end
    end

This is the classic [Fizz Buzz](https://en.wikipedia.org/wiki/Fizz_buzz) interview question. The (truncated) output is:

    1
    2
    Fizz
    4
    Buzz
    Fizz
    7
    8



