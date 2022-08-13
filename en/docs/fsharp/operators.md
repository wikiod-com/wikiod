---
title: "Operators"
slug: "operators"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## How to compose values and functions using common operators
In Object Oriented Programming a common task is to compose objects (values).
In Functional Programming it is as common task to compose values as well as functions.

We are used to compose values from our experience of other programming languages
using operators like `+`, `-`, `*`, `/` and so on.

**Value composition**

    let x = 1 + 2 + 3 * 2

As functional programming composes functions as well as values it's not surprising
there are common operators for function composition like `>>`, `<<`, `|>` and `<|`.

**Function composition**

    // val f : int -> int
    let f v   = v + 1
    // val g : int -> int
    let g v   = v * 2

    // Different ways to compose f and g
    // val h : int -> int
    let h1 v  = g (f v)
    let h2 v  = v |> f |> g   // Forward piping of 'v'
    let h3 v  = g <| (f <| v) // Reverse piping of 'v' (because <| has left associcativity we need ())
    let h4    = f >> g        // Forward functional composition
    let h5    = g << f        // Reverse functional composition (closer to math notation of 'g o f')

In `F#` forward piping is preferred over reverse piping because:

  1. Type inference (generally) flows from left-to-right so it's natural for values and functions to flow left-to-right as well
  2. Because `<|` and `<<` should have right associativity but in `F#` they are left associative which forces us to insert ()
  3. Mixing forward and reverse piping generally don't work because they have the same precedence.

**Monad composition**

As Monads (like `Option<'T>` or `List<'T>`) are commonly used in functional programming
there are also common but less known operators to compose functions working with
Monads like `>>=`, `>=>`, `<|>` and `<*>`.

    let (>>=) t uf  = Option.bind uf t
    let (>=>) tf uf = fun v -> tf v >>= uf
    // val oinc   : int -> int option
    let oinc   v    = Some (v + 1)    // Increment v
    // val ofloat : int -> float option
    let ofloat v    = Some (float v)  // Map v to float

    // Different ways to compose functions working with Option Monad
    // val m : int option -> float option
    let m1 v  = Option.bind (fun v -> Some (float (v + 1))) v
    let m2 v  = v |> Option.bind oinc |> Option.bind ofloat
    let m3 v  = v >>= oinc >>= ofloat
    let m4    = oinc >=> ofloat

    // Other common operators are <|> (orElse) and <*> (andAlso)

    // If 't' has Some value then return t otherwise return u
    let (<|>) t u =
      match t with
      | Some _  -> t
      | None    -> u

    // If 't' and 'u' has Some values then return Some (tv*uv) otherwise return None
    let (<*>) t u =
      match t, u with
      | Some tv, Some tu  -> Some (tv, tu)
      | _                 -> None

    // val pickOne : 'a option -> 'a option -> 'a option
    let pickOne t u v = t <|> u <|> v

    // val combine : 'a option -> 'b option  -> 'c option -> (('a*'b)*'c) option
    let combine t u v = t <*> u <*> v

**Conclusion**

To new functional programmers function composition using operators might seem opaque
and obscure but that is because the meaning of these operators aren't as commonly
known as operators working on values. However, with some training using `|>`, `>>`
, `>>=` and `>=>` becomes as natural as using `+`, `-`, `*` and `/`.



## Latebinding in F# using ? operator
In a statically typed language like `F#` we work with types well-known at
compile-time. We consume external data sources in a type-safe manner using type
providers.

However, occassionally there's need to use late binding (like `dynamic` in `C#`).
For instance when working with `JSON` documents that have no well-defined schema.

To simplify working with late binding `F#` provides supports dynamic lookup operators `?` and `?<-`.

Example:

    // (?) allows us to lookup values in a map like this: map?MyKey
    let inline (?)   m k   = Map.tryFind k m
    // (?<-) allows us to update values in a map like this: map?MyKey <- 123
    let inline (?<-) m k v = Map.add k v m

    let getAndUpdate (map : Map<string, int>) : int option*Map<string, int> =
      let i = map?Hello       // Equivalent to map |> Map.tryFind "Hello"
      let m = map?Hello <- 3  // Equivalent to map |> Map.add "Hello" 3
      i, m

It turns out that the `F#` support for late binding is simple yet flexible.


