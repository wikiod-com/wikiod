---
title: "Enums"
slug: "enums"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
- @enum EnumType val=1 val val
- :symbol

It is sometimes useful to have enumerated types where each instance is of a different type (often a [singleton immutable type][1]); this can be important for type stability. Traits are typically implemented with this paradigm. However, this results in additional compile-time overhead.


  [1]: https://www.wikiod.com/julia-lang/types#Immutable Types

## Defining an enumerated type
An [enumerated type](https://en.wikipedia.org/wiki/Enumerated_type) is a [type][1] that can hold one of a finite list of possible values. In Julia, enumerated types are typically called "enum types". For instance, one could use enum types to describe the seven days of the week, the twelve months of the year, the four suits of a [standard 52-card deck](https://en.wikipedia.org/wiki/Standard_52-card_deck), or other similar situations.

We can define enumerated types to model the suits and ranks of a standard 52-card deck. The `@enum` macro is used to define enum types.

    @enum Suit ♣ ♦ ♥ ♠
    @enum Rank ace=1 two three four five six seven eight nine ten jack queen king

This defines two types: `Suit` and `Rank`. We can check that the values are indeed of the expected types:

    julia> ♦
    ♦::Suit = 1
    
    julia> six
    six::Rank = 6

Note that each suit and rank has been associated with a number. By default, this number starts at zero. So the second suit, diamonds, was assigned the number 1. In the case of `Rank`, it may make more sense to start the number at one. This was achieved by annotating the definition of `ace` with a `=1` annotation.

Enumerated types come with a lot of functionality, such as equality (and indeed identity) and comparisons built in:

    julia> seven === seven
    true
    
    julia> ten ≠ jack
    true
    
    julia> two < three
    true

Like values of any other [immutable type][2], values of enumerated types can also be hashed and stored in `Dict`s.

We can complete this example by defining a `Card` type that has a `Rank` and a `Suit` field:

    immutable Card
        rank::Rank
        suit::Suit
    end

and hence we can create cards with

    julia> Card(three, ♣)
    Card(three::Rank = 3,♣::Suit = 0)

But enumerated types also come with their own `convert` methods, so we can indeed simply do

    julia> Card(7, ♠)
    Card(seven::Rank = 7,♠::Suit = 3)

and since `7` can be directly converted to `Rank`, this constructor works out of the box.

We might wish to define syntactic sugar for constructing these cards; implicit multiplication provides a convenient way to do it. Define

    julia> import Base.*
    
    julia> r::Int * s::Suit = Card(r, s)
    * (generic function with 156 methods)

and then

    julia> 10♣
    Card(ten::Rank = 10,♣::Suit = 0)
    
    julia> 5♠
    Card(five::Rank = 5,♠::Suit = 3)

once again taking advantage of the in-built `convert` functions.

  [1]: https://www.wikiod.com/julia-lang/types
  [2]: https://www.wikiod.com/julia-lang/types#Immutable Types

## Using symbols as lightweight enums
Although the `@enum` macro is quite useful for most use cases, it can be excessive in some use cases. Disadvantages of `@enum` include:

- It creates a new type
- It is a little harder to extend
- It comes with functionality such as conversion, enumeration, and comparison, which may be superfluous in some applications

In cases where a lighter-weight alternative is desired, the `Symbol` type can be used. Symbols are [interned strings][1]; they represent sequences of characters, much like [strings][2] do, but they are uniquely associated with numbers. This unique association enables fast symbol equality comparison.

We may again implement a `Card` type, this time using `Symbol` fields:

    const ranks = Set([:ace, :two, :three, :four, :five, :six, :seven, :eight, :nine,
                       :ten, :jack, :queen, :king])
    const suits = Set([:♣, :♦, :♥, :♠])
    immutable Card
        rank::Symbol
        suit::Symbol
        function Card(r::Symbol, s::Symbol)
            r in ranks || throw(ArgumentError("invalid rank: $r"))
            s in suits || throw(ArgumentError("invalid suit: $s"))
            new(r, s)
        end
    end

We implement the inner constructor to check for any incorrect values passed to the constructor. Unlike in the example using `@enum` types, `Symbol`s can contain any string, and so we must be careful about what kinds of `Symbol`s we accept. Note here the use of the [short-circuit][3] conditional operators.

Now we can construct `Card` objects like we expect:

    julia> Card(:ace, :♦)
    Card(:ace,:♦)
    
    julia> Card(:nine, :♠)
    Card(:nine,:♠)
    
    julia> Card(:eleven, :♠)
    ERROR: ArgumentError: invalid rank: eleven
     in Card(::Symbol, ::Symbol) at ./REPL[17]:5
    
    julia> Card(:king, :X)
    ERROR: ArgumentError: invalid suit: X
     in Card(::Symbol, ::Symbol) at ./REPL[17]:6

A major benefit of `Symbol`s is their runtime extensibility. If at runtime, we wish to accept (for example) `:eleven` as a new rank, it suffices to simply run `push!(ranks, :eleven)`. Such runtime extensibility is not possible with `@enum` types.


  [1]: https://en.wikipedia.org/wiki/String_interning
  [2]: https://www.wikiod.com/julia-lang/strings
  [3]: https://www.wikiod.com/julia-lang/conditionals#Short-circuit operators: && and ||

