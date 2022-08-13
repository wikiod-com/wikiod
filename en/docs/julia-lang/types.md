---
title: "Types"
slug: "types"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
- immutable MyType; field; field; end
- type MyType; field; field; end

Types are key to Julia's performance. An important idea for performance is [type stability][1], which occurs when the type a function returns only depends on the types, not the values, of its arguments.


  [1]: https://www.wikiod.com/julia-lang/type-stability

## Dispatching on Types
On Julia, you can define more than one method for each function. Suppose we define three methods of the same function:

    foo(x) = 1
    foo(x::Number) = 2
    foo(x::Int) = 3

When deciding what method to use (called [dispatch][1]), Julia chooses the more specific method that matches the types of the arguments:

    julia> foo('one')
    1
    
    julia> foo(1.0)
    2
    
    julia> foo(1)
    3

This facilitates [polymorphism](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)). For instance, we can easily create a [linked list](https://en.wikipedia.org/wiki/Linked_list) by defining two immutable types, named `Nil` and `Cons`. These names are traditionally used to describe an empty list and a non-empty list, respectively.

    abstract LinkedList
    immutable Nil <: LinkedList end
    immutable Cons <: LinkedList
        first
        rest::LinkedList
    end

We will represent the empty list by `Nil()` and any other lists by `Cons(first, rest)`, where `first` is the first element of the linked list and `rest` is the linked list consisting of all remaining elements. For example, the list `[1, 2, 3]` will be represented as

    julia> Cons(1, Cons(2, Cons(3, Nil())))
    Cons(1,Cons(2,Cons(3,Nil())))

## Is the list empty?

Suppose we want to extend the standard library's `isempty` function, which works on a variety of different collections:

    julia> methods(isempty)
    # 29 methods for generic function "isempty":
    isempty(v::SimpleVector) at essentials.jl:180
    isempty(m::Base.MethodList) at reflection.jl:394
    ...

We can simply use the function dispatch syntax, and define two additional methods of `isempty`. Since this function is from the `Base` module, we have to qualify it as `Base.isempty` in order to extend it.

    Base.isempty(::Nil) = true
    Base.isempty(::Cons) = false

Here, we did not need the argument values at all to determine whether the list is empty. Merely the type alone suffices to compute that information. Julia allows us to omit the names of arguments, keeping only their type annotation, if we need not use their values.

We can [test][2] that our `isempty` methods work:

    julia> using Base.Test
    
    julia> @test isempty(Nil())
    Test Passed
      Expression: isempty(Nil())
    
    julia> @test !isempty(Cons(1, Cons(2, Cons(3, Nil()))))
    Test Passed
      Expression: !(isempty(Cons(1,Cons(2,Cons(3,Nil())))))

and indeed the number of methods for `isempty` have increased by `2`:

    julia> methods(isempty)
    # 31 methods for generic function "isempty":
    isempty(v::SimpleVector) at essentials.jl:180
    isempty(m::Base.MethodList) at reflection.jl:394

Clearly, determining whether a linked list is empty or not is a trivial example. But it leads up to something more interesting:

## How long is the list?

The `length` function from the standard library gives us the length of a collection or certain [iterables][3]. There are many ways to implement `length` for a linked list. In particular, using a `while` loop is likely fastest and most memory-efficient in Julia. But [premature optimization](https://en.wikipedia.org/wiki/Program_optimization#When_to_optimize) is to be avoided, so let's suppose for a second that our linked list need not be efficient. What's the simplest way to write a `length` function?

    Base.length(::Nil) = 0
    Base.length(xs::Cons) = 1 + length(xs.rest)

The first definition is straightforward: an empty list has length `0`. The second definition is also easy to read: to count the length of a list, we count the first element, then count the length of the rest of the list. We can test this method similarly to how we tested `isempty`:

    julia> @test length(Nil()) == 0
    Test Passed
      Expression: length(Nil()) == 0
       Evaluated: 0 == 0
    
    julia> @test length(Cons(1, Cons(2, Cons(3, Nil())))) == 3
    Test Passed
      Expression: length(Cons(1,Cons(2,Cons(3,Nil())))) == 3
       Evaluated: 3 == 3

## Next steps

This toy example is pretty far from implementing all of the functionality that would be desired in a linked list. It is missing, for instance, the iteration interface. However, it illustrates how dispatch can be used to write short and clear code.

  [1]: https://www.wikiod.com/julia-lang/functions#Introduction to Dispatch
  [2]: https://www.wikiod.com/julia-lang/unit-testing#Writing a Simple Test
  [3]: https://www.wikiod.com/julia-lang/iterables

## Immutable Types
The simplest composite type is an immutable type. Instances of immutable types, like [tuples][1], are values. Their fields cannot be changed after they are created. In many ways, an immutable type is like a `Tuple` with names for the type itself and for each field.

## Singleton types

Composite types, by definition, contain a number of simpler types. In Julia, this number can be zero; that is, an immutable type is allowed to contain _no_ fields. This is comparable to the empty tuple `()`.

Why might this be useful? Such immutable types are known as "singleton types", as only one instance of them could ever exist. The values of such types are known as "singleton values". The standard library `Base` contains many such singleton types. Here is a brief list:

- `Void`, the type of `nothing`. We can verify that `Void.instance` (which is special syntax for retrieving the singleton value of a singleton type) is indeed `nothing`.
- Any media type, such as `MIME"text/plain"`, is a singleton type with a single instance, `MIME("text/plain")`.
- The `Irrational{:π}`, `Irrational{:e}`, `Irrational{:φ}`, and similar types are singleton types, and their singleton instances are the irrational values `π = 3.1415926535897...`, etc.
- The iterator size traits `Base.HasLength`, `Base.HasShape`, `Base.IsInfinite`, and `Base.SizeUnknown` are all singleton types.
<!-- if version [gte 0.5.0] -->
- In version 0.5 and later, each [function][2] is a singleton instance of a singleton type! Like any other singleton value, we can recover the function `sin`, for example, from `typeof(sin).instance`.
<!-- end version if -->

Because they contain nothing, singleton types are incredibly lightweight, and they can frequently be optimized away by the compiler to have no runtime overhead. Thus, they are perfect for traits, special tag values, and for things like functions that one would like to specialize on.

To define a singleton type,

    julia> immutable MySingleton end

To define custom printing for the singleton type,

    julia> Base.show(io::IO, ::MySingleton) = print(io, "sing")

To access the singleton instance,

    julia> MySingleton.instance
    MySingleton()

Often, one assigns this to a constant:

    julia> const sing = MySingleton.instance
    MySingleton()

## Wrapper types

If zero-field immutable types are interesting and useful, then perhaps one-field immutable types are even more useful. Such types are commonly called "wrapper types" because they wrap some underlying data, providing an alternative interface to said data. An example of a wrapper type in `Base` is [`String`][3]. We will define a similar type to `String`, named `MyString`. This type will be backed by a vector (one-dimensional [array][4]) of bytes (`UInt8`).

First, the type definition itself and some customized showing:

    immutable MyString <: AbstractString
        data::Vector{UInt8}
    end

    function Base.show(io::IO, s::MyString)
        print(io, "MyString: ")
        write(io, s.data)
        return
    end

Now our `MyString` type is ready for use! We can feed it some raw UTF-8 data, and it displays as we like it to:

    julia> MyString([0x48,0x65,0x6c,0x6c,0x6f,0x2c,0x20,0x57,0x6f,0x72,0x6c,0x64,0x21])
    MyString: Hello, World!

Obviously, this string type needs a lot of work before it becomes as usable as the `Base.String` type.

## True composite types

Perhaps most commonly, many immutable types contain more than one field. An example is the standard library `Rational{T}` type, which contains two fieds: a `num` field for the numerator, and a `den` field for the denominator. It is fairly straightforward to emulate this type design:

    immutable MyRational{T}
        num::T
        den::T
        MyRational(n, d) = (g = gcd(n, d); new(n÷g, d÷g))
    end
    MyRational{T}(n::T, d::T) = MyRational{T}(n, d)

We have successfully implemented a constructor that simplifies our rational numbers:

    julia> MyRational(10, 6)
    MyRational{Int64}(5,3)

  [1]: https://www.wikiod.com/julia-lang/tuples
  [2]: https://www.wikiod.com/julia-lang/functions
  [3]: https://www.wikiod.com/julia-lang/strings
  [4]: https://www.wikiod.com/julia-lang/arrays

