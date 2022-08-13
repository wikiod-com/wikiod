---
title: "Comprehensions"
slug: "comprehensions"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Array comprehension
## Basic Syntax

Julia's array comprehensions use the following syntax:

    [expression for element = iterable]

Note that as with [`for` loops][1], all of `=`, `in`, and `∈` are accepted for the comprehension.

This is roughly equivalent to creating an empty array and using a `for` loop to `push!` items to it.

    result = []
    for element in iterable
        push!(result, expression)
    end

however, the type of an array comprehension is as narrow as possible, which is better for performance.

For example, to get an array of the squares of the integers from `1` to `10`, the following code may be used.

    squares = [x^2 for x=1:10]

This is a clean, concise replacement for the longer `for`-loop version.

    squares = []
    for x in 1:10
        push!(squares, x^2)
    end
       

  [1]: https://www.wikiod.com/julia-lang/for-loops

## Conditional Array Comprehension
Before the Julia 0.5, there is no way to use conditions inside the array comprehensions. But, it is no longer true. In Julia 0.5 we can use the conditions inside conditions like the following:

    julia> [x^2 for x in 0:9 if x > 5] 
    4-element Array{Int64,1}:
     36
     49
     64
     81
    
Source of the above example can be found [here][1]. 

If we would like to use nested list comprehension:

    julia>[(x,y) for x=1:5 , y=3:6 if y>4 && x>3 ]
    4-element Array{Tuple{Int64,Int64},1}:
     (4,5)
     (5,5)
     (4,6)
     (5,6)

  


  [1]: http://stackoverflow.com/a/38947888/5223033

## Multidimensional array comprehensions
Nested `for` loops may be used to iterate over a number of unique iterables.

    result = []
    for a = iterable_a
        for b = iterable_b
            push!(result, expression)
        end
    end

Similarly, multiple iteration specifications may be supplied to an array comprehension.

    [expression for a = iterable_a, b = iterable_b]

For example, the following may be used to generate the Cartesian product of `1:3` and `1:2`.

    julia> [(x, y) for x = 1:3, y = 1:2]
    3×2 Array{Tuple{Int64,Int64},2}:
     (1,1)  (1,2)
     (2,1)  (2,2)
     (3,1)  (3,2)

Flattened multidimensional array comprehensions are similar, except that they lose the shape. For example,

    julia> [(x, y) for x = 1:3 for y = 1:2]
    6-element Array{Tuple{Int64,Int64},1}:
     (1, 1)
     (1, 2)
     (2, 1)
     (2, 2)
     (3, 1)
     (3, 2)

is a flattened variant of the above. The syntactic difference is that an additional `for` is used instead of a comma.


## Generator Comprehensions


