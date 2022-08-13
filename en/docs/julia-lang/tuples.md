---
title: "Tuples"
slug: "tuples"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Syntax
- a,
- a, b
- a, b = xs
- ()
- (a,)
- (a, b)
- (a, b...)
- Tuple{T, U, V}
- NTuple{N, T}
- Tuple{T, U, Vararg{V}}

Tuples have much better runtime performance than [arrays][1] for two reasons: their types are more precise, and their immutability allows them to be allocated on the stack instead of the heap. However, this more precise typing comes with both more compile-time overhead and more difficulty achieving [type stability][2].


  [1]: https://www.wikiod.com/julia-lang/arrays
  [2]: https://www.wikiod.com/julia-lang/type-stability

## Introduction to Tuples
`Tuple`s are immutable ordered collections of arbitrary distinct objects, either of the same type or of different [types][1]. Typically, tuples are constructed using the `(x, y)` syntax.

    julia> tup = (1, 1.0, "Hello, World!")
    (1,1.0,"Hello, World!")

The individual objects of a tuple can be retrieved using indexing syntax:

    julia> tup[1]
    1

    julia> tup[2]
    1.0

    julia> tup[3]
    "Hello, World!"

They implement the [iterable interface][2], and can therefore be iterated over using [`for` loops][3]:

    julia> for item in tup
               println(item)
           end
    1
    1.0
    Hello, World!

Tuples also support a variety of generic collections functions, such as `reverse` or `length`:

    julia> reverse(tup)
    ("Hello, World!",1.0,1)

    julia> length(tup)
    3

Furthermore, tuples support a variety of [higher-order][4] collections operations, including `any`, `all`, [`map`][5], or `broadcast`:

    julia> map(typeof, tup)
    (Int64,Float64,String)
    
    julia> all(x -> x < 2, (1, 2, 3))
    false
    
    julia> all(x -> x < 4, (1, 2, 3))
    true
    
    julia> any(x -> x < 2, (1, 2, 3))
    true

The empty tuple can be constructed using `()`:

    julia> ()
    ()

    julia> isempty(ans)
    true

However, to construct a tuple of one element, a trailing comma is required. This is because the parentheses (`(` and `)`) would otherwise be treated as grouping operations together instead of constructing a tuple.

    julia> (1)
    1

    julia> (1,)
    (1,)

For consistency, a trailing comma is also allowed for tuples with more than one element.

    julia> (1, 2, 3,)
    (1,2,3)


  [1]: https://www.wikiod.com/julia-lang/types
  [2]: https://www.wikiod.com/julia-lang/iterables
  [3]: https://www.wikiod.com/julia-lang/for-loops
  [4]: https://www.wikiod.com/julia-lang/higher-order-functions
  [5]: https://www.wikiod.com/julia-lang/higher-order-functions#Map, filter, and reduce

## Tuple types
The `typeof` a tuple is a subtype of `Tuple`:

    julia> typeof((1, 2, 3))
    Tuple{Int64,Int64,Int64}
    
    julia> typeof((1.0, :x, (1, 2)))
    Tuple{Float64,Symbol,Tuple{Int64,Int64}}

Unlike other data types, `Tuple` types are [covariant](https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)). Other data types in Julia are generally invariant. Thus,

    julia> Tuple{Int, Int} <: Tuple{Number, Number}
    true

    julia> Vector{Int} <: Vector{Number}
    false

This is the case because everywhere a `Tuple{Number, Number}` is accepted, so too would a `Tuple{Int, Int}`, since it also has two elements, both of which are numbers. That is not the case for a `Vector{Int}` versus a `Vector{Number}`, as a function accepting a `Vector{Number}` may wish to store a floating point (e.g. `1.0`) or a complex number (e.g. `1+3im`) in such a vector.

The covariance of tuple types means that `Tuple{Number}` (again unlike `Vector{Number}`) is actually an abstract type:

    julia> isleaftype(Tuple{Number})
    false

    julia> isleaftype(Vector{Number})
    true

Concrete subtypes of `Tuple{Number}` include `Tuple{Int}`, `Tuple{Float64}`, `Tuple{Rational{BigInt}}`, and so forth.

`Tuple` types may contain a terminating `Vararg` as their last parameter to indicate an indefinite number of objects. For instance, `Tuple{Vararg{Int}}` is the type of all tuples containing any number of `Int`s, possibly zero:

    julia> isa((), Tuple{Vararg{Int}})
    true
    
    julia> isa((1,), Tuple{Vararg{Int}})
    true
    
    julia> isa((1,2,3,4,5), Tuple{Vararg{Int}})
    true
    
    julia> isa((1.0,), Tuple{Vararg{Int}})
    false

whereas `Tuple{String, Vararg{Int}}` accepts tuples consisting of a [string][1], followed by any number (possibly zero) of `Int`s.

    julia> isa(("x", 1, 2), Tuple{String, Vararg{Int}})
    true
    
    julia> isa((1, 2), Tuple{String, Vararg{Int}})
    false

Combined with co-variance, this means that `Tuple{Vararg{Any}}` describes any tuple. Indeed, `Tuple{Vararg{Any}}` is just another way of saying `Tuple`:

    julia> Tuple{Vararg{Any}} == Tuple
    true

`Vararg` accepts a second numeric type parameter indicating how many times exactly its first type parameter should occur. (By default, if unspecified, this second type parameter is a typevar that can take any value, which is why any number of `Int`s are accepted in the `Vararg`s above.) `Tuple` types ending in a specified `Vararg` will automatically be expanded to the requested number of elements:

    julia> Tuple{String,Vararg{Int, 3}}
    Tuple{String,Int64,Int64,Int64}

Notation exists for homogenous tuples with a specified `Vararg`: `NTuple{N, T}`. In this notation, `N` denotes the number of elements in the tuple, and `T` denotes the type accepted. For instance,

    julia> NTuple{3, Int}
    Tuple{Int64,Int64,Int64}
    
    julia> NTuple{10, Int}
    NTuple{10,Int64}
    
    julia> ans.types
    svec(Int64,Int64,Int64,Int64,Int64,Int64,Int64,Int64,Int64,Int64)

Note that `NTuple`s beyond a certain size are shown simply as `NTuple{N, T}`, instead of the expanded `Tuple` form, but they are still the same type:

    julia> Tuple{Int,Int,Int,Int,Int,Int,Int,Int,Int,Int}
    NTuple{10,Int64}

  [1]: https://www.wikiod.com/julia-lang/strings

## Dispatching on tuple types
Because Julia function parameter lists are themselves tuples, [dispatching][2] on various kinds of tuples is often easier done through the method parameters themselves, often with liberal usage for the "splatting" `...` operator. For instance, consider the implementation of `reverse` for tuples, from `Base`:

    revargs() = ()
    revargs(x, r...) = (revargs(r...)..., x)
    
    reverse(t::Tuple) = revargs(t...)

Implementing methods on tuples this way preserves [type stability][3], which is crucial for performance. We can see that there is no overhead to this approach using the `@code_warntype` macro:

    julia> @code_warntype reverse((1, 2, 3))
    Variables:
      #self#::Base.#reverse
      t::Tuple{Int64,Int64,Int64}
    
    Body:
      begin 
          SSAValue(1) = (Core.getfield)(t::Tuple{Int64,Int64,Int64},2)::Int64
          SSAValue(2) = (Core.getfield)(t::Tuple{Int64,Int64,Int64},3)::Int64
          return (Core.tuple)(SSAValue(2),SSAValue(1),(Core.getfield)(t::Tuple{Int64,Int64,Int64},1)::Int64)::Tuple{Int64,Int64,Int64}
      end::Tuple{Int64,Int64,Int64}

Although somewhat hard to read, the code here is simply getting creating a new tuple with values 3rd, 2nd, and 1st elements of the original tuple, respectively. On many machines, this compiles down to extremely efficient LLVM code, which consists of loads and stores.

    julia> @code_llvm reverse((1, 2, 3))
    
    define void @julia_reverse_71456([3 x i64]* noalias sret, [3 x i64]*) #0 {
    top:
      %2 = getelementptr inbounds [3 x i64], [3 x i64]* %1, i64 0, i64 1
      %3 = getelementptr inbounds [3 x i64], [3 x i64]* %1, i64 0, i64 2
      %4 = load i64, i64* %3, align 1
      %5 = load i64, i64* %2, align 1
      %6 = getelementptr inbounds [3 x i64], [3 x i64]* %1, i64 0, i64 0
      %7 = load i64, i64* %6, align 1
      %.sroa.0.0..sroa_idx = getelementptr inbounds [3 x i64], [3 x i64]* %0, i64 0, i64 0
      store i64 %4, i64* %.sroa.0.0..sroa_idx, align 8
      %.sroa.2.0..sroa_idx1 = getelementptr inbounds [3 x i64], [3 x i64]* %0, i64 0, i64 1
      store i64 %5, i64* %.sroa.2.0..sroa_idx1, align 8
      %.sroa.3.0..sroa_idx2 = getelementptr inbounds [3 x i64], [3 x i64]* %0, i64 0, i64 2
      store i64 %7, i64* %.sroa.3.0..sroa_idx2, align 8
      ret void
    }

  [2]: https://www.wikiod.com/julia-lang/functions#Introduction to Dispatch
  [3]: https://www.wikiod.com/julia-lang/type-stability

## Multiple return values
Tuples are frequently used for multiple return values. Much of the standard library, including two of the functions of the [iterable interface][1] (`next` and `done`), returns tuples containing two related but distinct values.

The parentheses around tuples can be omitted in certain situations, making multiple return values easier to implement. For instance, we can create a function to return both positive and negative square roots of a real number:

    julia> pmsqrt(x::Real) = sqrt(x), -sqrt(x)
    pmsqrt (generic function with 1 method)
    
    julia> pmsqrt(4)
    (2.0,-2.0)

Destructuring assignment can be used to unpack the multiple return values. To store the square roots in variables `a` and `b`, it suffices to write:

    julia> a, b = pmsqrt(9.0)
    (3.0,-3.0)
    
    julia> a
    3.0
    
    julia> b
    -3.0

Another example of this is the `divrem` and `fldmod` functions, which do an [integer (truncating or floored, respectively) division][2] and remainder operation at the same time:

    julia> q, r = divrem(10, 3)
    (3,1)
    
    julia> q
    3
    
    julia> r
    1


  [1]: https://www.wikiod.com/julia-lang/iterables
  [2]: https://www.wikiod.com/julia-lang/arithmetic

