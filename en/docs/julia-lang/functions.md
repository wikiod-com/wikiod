---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Syntax
- f(n) = ...
- function f(n) ... end
- n::Type
- x -> ...
- f(n) do ... end

Aside from generic functions (which are most common), there are also built-in functions. Such functions include `is`, `isa`, `typeof`, `throw`, and similar functions. Built-in functions are typically implemented in C instead of Julia, so they cannot be specialized on argument types for dispatch.

## Introduction to Dispatch
We can use the `::` syntax to dispatch on the [type][1] of an argument.

    describe(n::Integer) = "integer $n"
    describe(n::AbstractFloat) = "floating point $n"

Usage:

    julia> describe(10)
    "integer 10"

    julia> describe(1.0)
    "floating point 1.0"

Unlike many languages, which typically provide either static multiple dispatch or dynamic single dispatch, Julia has full dynamic multiple dispatch. That is, functions can be specialized for more than one argument. This comes in handy when defining specialized methods for operations on certain types, and fallback methods for other types.

    describe(n::Integer, m::Integer) = "integers n=$n and m=$m"
    describe(n, m::Integer) = "only m=$m is an integer"
    describe(n::Integer, m) = "only n=$n is an integer"

Usage:

    julia> describe(10, 'x')
    "only n=10 is an integer"
    
    julia> describe('x', 10)
    "only m=10 is an integer"
    
    julia> describe(10, 10)
    "integers n=10 and m=10"

## Optional Arguments

Julia allows functions to take optional arguments. Behind the scenes, this is implemented as another special case of multiple dispatch. For instance, let's solve the popular [Fizz Buzz problem][2]. By default, we will do it for numbers in the range `1:10`, but we will allow a different value if necessary. We will also allow different phrases to be used for `Fizz` or `Buzz`.

    function fizzbuzz(xs=1:10, fizz="Fizz", buzz="Buzz")
        for i in xs
            if i % 15 == 0
                println(fizz, buzz)
            elseif i % 3 == 0
                println(fizz)
            elseif i % 5 == 0
                println(buzz)
            else
                println(i)
            end
        end
    end

If we inspect `fizzbuzz` in the REPL, it says that there are four methods. One method was created for each combination of arguments allowed.

    julia> fizzbuzz
    fizzbuzz (generic function with 4 methods)
    
    julia> methods(fizzbuzz)
    # 4 methods for generic function "fizzbuzz":
    fizzbuzz() at REPL[96]:2
    fizzbuzz(xs) at REPL[96]:2
    fizzbuzz(xs, fizz) at REPL[96]:2
    fizzbuzz(xs, fizz, buzz) at REPL[96]:2

We can verify that our default values are used when no parameters are provided:

    julia> fizzbuzz()
    1
    2
    Fizz
    4
    Buzz
    Fizz
    7
    8
    Fizz
    Buzz

but that the optional parameters are accepted and respected if we provide them:

    julia> fizzbuzz(5:8, "fuzz", "bizz")
    bizz
    fuzz
    7
    8

## Parametric Dispatch

It is frequently the case that a function should dispatch on parametric types, such as [`Vector{T}`][3] or `Dict{K,V}`, but the type parameters are not fixed. This case can be dealt with by using parametric dispatch:

    julia> foo{T<:Number}(xs::Vector{T}) = @show xs .+ 1
    foo (generic function with 1 method)
    
    julia> foo(xs::Vector) = @show xs
    foo (generic function with 2 methods)
    
    julia> foo([1, 2, 3])
    xs .+ 1 = [2,3,4]
    3-element Array{Int64,1}:
     2
     3
     4
    
    julia> foo([1.0, 2.0, 3.0])
    xs .+ 1 = [2.0,3.0,4.0]
    3-element Array{Float64,1}:
     2.0
     3.0
     4.0
    
    julia> foo(["x", "y", "z"])
    xs = String["x","y","z"]
    3-element Array{String,1}:
     "x"
     "y"
     "z"

One may be tempted to simply write `xs::Vector{Number}`. But this only works for objects whose type is explicitly `Vector{Number}`:

    julia> isa(Number[1, 2], Vector{Number})
    true
    
    julia> isa(Int[1, 2], Vector{Number})
    false

This is due to [parametric invariance][4]: the object `Int[1, 2]` is _not_ a `Vector{Number}`, because it can only contain `Int`s, whereas a `Vector{Number}` would be expected to be able to contain any kinds of numbers.

## Writing Generic Code

Dispatch is an incredibly powerful feature, but frequently it is better to write generic code that works for all types, instead of specializing code for each type. Writing generic code avoids code duplication.

For example, here is code to compute the sum of squares of a vector of integers:

    function sumsq(v::Vector{Int})
        s = 0
        for x in v
            s += x ^ 2
        end
        s
    end

But this code _only_ works for a vector of `Int`s. It will not work on a `UnitRange`:

    julia> sumsq(1:10)
    ERROR: MethodError: no method matching sumsq(::UnitRange{Int64})
    Closest candidates are:
      sumsq(::Array{Int64,1}) at REPL[8]:2

It will not work on a `Vector{Float64}`:

    julia> sumsq([1.0, 2.0])
    ERROR: MethodError: no method matching sumsq(::Array{Float64,1})
    Closest candidates are:
      sumsq(::Array{Int64,1}) at REPL[8]:2

A better way to write this `sumsq` function should be

    function sumsq(v::AbstractVector)
        s = zero(eltype(v))
        for x in v
            s += x ^ 2
        end
        s
    end

This will work on the two cases listed above. But there are some collections that we might want to sum the squares of that aren't vectors at all, in any sense. For instance,

    julia> sumsq(take(countfrom(1), 100))
    ERROR: MethodError: no method matching sumsq(::Base.Take{Base.Count{Int64}})
    Closest candidates are:
      sumsq(::Array{Int64,1}) at REPL[8]:2
      sumsq(::AbstractArray{T,1}) at REPL[11]:2

shows that we cannot sum the squares of a [lazy iterable][5].

An even more generic implementation is simply

    function sumsq(v)
        s = zero(eltype(v))
        for x in v
            s += x ^ 2
        end
        s
    end

Which works in all cases:

    julia> sumsq(take(countfrom(1), 100))
    338350

This is the most idiomatic Julia code, and can handle all sorts of situations. In some other languages, removing type annotations may affect performance, but that is not the case in Julia; only [type stability][6] is important for performance.


  [1]: https://www.wikiod.com/julia-lang/types
  [2]: https://www.wikiod.com/julia-lang/for-loops#Fizz Buzz
  [3]: https://www.wikiod.com/julia-lang/arrays#Vectors
  [4]: https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)#Arrays
  [5]: https://www.wikiod.com/julia-lang/iterables#Combining Lazy Iterables
  [6]: https://www.wikiod.com/julia-lang/type-stability

## Square a number
This is the easiest syntax to define a function:

    square(n) = n * n

To call a function, use round brackets (without spaces in between):

    julia> square(10)
    100

Functions are objects in Julia, and we can show them in the [REPL][1] as with any other objects:

    julia> square
    square (generic function with 1 method)

All Julia functions are generic (otherwise known as [polymorphic][2]) by default. Our `square` function works just as well with floating point values:

    julia> square(2.5)
    6.25

...or even [matrices][3]:

    julia> square([2 4
                   2 1])
    2×2 Array{Int64,2}:
     12  12
      6   9


  [1]: https://www.wikiod.com/julia-lang/repl
  [2]: https://en.wikipedia.org/wiki/Polymorphism_(computer_science)
  [3]: https://www.wikiod.com/julia-lang/arrays

## Recursive functions
## Simple recursion

Using recursion and the [ternary conditional operator][1], we can create an alternative implementation of the built-in `factorial` function:

    myfactorial(n) = n == 0 ? 1 : n * myfactorial(n - 1)

Usage:

    julia> myfactorial(10)
    3628800

## Working with trees

Recursive functions are often most useful on data structures, especially tree data structures. Since [expressions][2] in Julia are tree structures, recursion can be quite useful for [metaprogramming][3]. For instance, the below function gathers a set of all heads used in an expression.

    heads(ex::Expr) = reduce(∪, Set((ex.head,)), (heads(a) for a in ex.args))
    heads(::Any) = Set{Symbol}()

We can check that our function is working as intended:

    julia> heads(:(7 + 4x > 1 > A[0]))
    Set(Symbol[:comparison,:ref,:call])

This function is compact and uses a variety of more advanced techniques, such as the `reduce` [higher order function][4], the `Set` data type, and generator expressions.


  [1]: https://www.wikiod.com/julia-lang/conditionals#Ternary conditional operator
  [2]: https://www.wikiod.com/julia-lang/expressions
  [3]: https://www.wikiod.com/julia-lang/metaprogramming
  [4]: https://www.wikiod.com/julia-lang/higher-order-functions

## Imperative factorial
A long-form syntax is available for defining multi-line functions. This can be useful when we use imperative structures such as loops. The expression in tail position is returned. For instance, the below function uses a [`for` loop][1] to compute the [factorial][2] of some integer `n`:

    function myfactorial(n)
        fact = one(n)
        for m in 1:n
            fact *= m
        end
        fact
    end

Usage:

    julia> myfactorial(10)
    3628800

In longer functions, it is common to see the `return` statement used. The `return` statement is not necessary in tail position, but it is still sometimes used for clarity. For instance, another way of writing the above function would be

    function myfactorial(n)
        fact = one(n)
        for m in 1:n
            fact *= m
        end
        return fact
    end

which is identical in behaviour to the function above.


  [1]: https://www.wikiod.com/julia-lang/for-loops
  [2]: https://en.wikipedia.org/wiki/Factorial

## Anonymous functions
## Arrow syntax

Anonymous functions can be created using the `->` syntax. This is useful for passing functions to [higher-order functions][1], such as the [`map`][2] function. The below function computes the square of each number in an [array][3] `A`.

    squareall(A) = map(x -> x ^ 2, A)

An example of using this function:

    julia> squareall(1:10)
    10-element Array{Int64,1}:
       1
       4
       9
      16
      25
      36
      49
      64
      81
     100

## Multiline syntax

Multiline anonymous functions can be created using `function` syntax. For instance, the following example computes the [factorials][4] of the first `n` numbers, but using an anonymous function instead of the built in `factorial`.

    julia> map(function (n)
                   product = one(n)
                   for i in 1:n
                       product *= i
                   end
                   product
               end, 1:10)
    10-element Array{Int64,1}:
           1
           2
           6
          24
         120
         720
        5040
       40320
      362880
     3628800

## Do block syntax

Because it is so common to pass an anonymous function as the first argument to a function, there is a `do` block syntax. The syntax

    map(A) do x
        x ^ 2
    end

is equivalent to

    map(x -> x ^ 2, A)

but the former can be more clear in many situations, especially if a lot of computation is being done in the anonymous function. `do` block syntax is especially useful for [file input and output][5] for resource management reasons.


  [1]: https://www.wikiod.com/julia-lang/higher-order-functions
  [2]: https://www.wikiod.com/julia-lang/higher-order-functions#Map, filter, and reduce
  [3]: https://www.wikiod.com/julia-lang/arrays
  [4]: https://www.wikiod.com/julia-lang/functions#Imperative factorial
  [5]: https://www.wikiod.com/julia-lang/input#Reading Data from a File

