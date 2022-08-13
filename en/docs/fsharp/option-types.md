---
title: "Option types"
slug: "option-types"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Use Option<'T> over null values
In functional programming languages like `F#` `null` values are considered
potentially harmful and poor style (non-idiomatic).

Consider this `C#` code:

    string x = SomeFunction ();
    int    l = x.Length;

`x.Length` will throw if `x` is `null` let's add protection:

    string x = SomeFunction ();
    int    l = x != null ? x.Length : 0;

Or:

    string x = SomeFunction () ?? "";
    int    l = x.Length;

Or:

    string x = SomeFunction ();
    int    l = x?.Length;

In idiomatic `F#` `null` values aren't used so our code looks like this:

    let x = SomeFunction ()
    let l = x.Length

However, sometimes there's a need for representing empty or invalid values. Then
we can use `Option<'T>`:

    let SomeFunction () : string option = ...

`SomeFunction` either returns `Some` `string` value or `None`. We extract the 
`string` value using pattern matching

    let v =
      match SomeFunction () with
      | Some x  -> x.Length
      | None    -> 0

The reason this code is less fragile than: 

    string x = SomeFunction ();
    int    l = x.Length;

Is because we can't call `Length` on a `string option`. We need to extract the 
`string` value using pattern matching and by doing so we are guaranteed that the 
`string` value is safe to use.


## Option Module enables Railway Oriented Programming 
Error handling is important but can make an elegant algorithm into a mess.
[Railway Oriented Programming](http://fsharpforfunandprofit.com/rop/) (`ROP`)
is used to make error handling elegant and composable.

Consider the simple function `f`:

    let tryParse s =
      let b, v = System.Int32.TryParse s
      if b then Some v else None

    let f (g : string option) : float option =
      match g with
      | None    -> None
      | Some s  ->
        match tryParse s with           // Parses string to int
        | None              -> None
        | Some v when v < 0 -> None     // Checks that int is greater than 0
        | Some v -> v |> float |> Some  // Maps int to float

The purpose of `f` is to parse the input `string` value (if there is `Some`)
into an `int`. If the `int` is greater than `0` we cast it into a `float`. In all
other cases we bail out with `None`.

Although, an extremely simple function the nested `match` decrease readability
significantly.

`ROP` observes we have two kind of execution paths in our program

  1. Happy path - Will eventually compute `Some` value
  2. Error path - All other paths produces `None`

Since the error paths are more frequent they tend to take over the code.
We would like that the happy path code is the most visible code path.

An equivalent function `g` using `ROP` could look like this:

    let g (v : string option) : float option =
      v
      |> Option.bind    tryParse  // Parses string to int
      |> Option.filter  ((<) 0)   // Checks that int is greater than 0
      |> Option.map     float     // Maps int to float

It looks a lot like how we tend to process lists and sequences in `F#`.

One can see an `Option<'T>` like a `List<'T>` that only may contain `0` or `1` element
where `Option.bind` behaves like `List.pick` (conceptually `Option.bind` maps
better to `List.collect` but `List.pick` might be easier to understand).

`bind`, `filter` and `map` handles the error paths and `g` only contain the happy
path code.

All functions that directly accepts `Option<_>` and returns `Option<_>` are
directly composable with `|>` and `>>`.

`ROP` therefore increases readability and composability.

## Definition of Option
An `Option` is a discriminated union with two cases, `None` or `Some`.  

    type Option<'T> = Some of 'T | None

## Using Option types from C#
It is not a good idea to expose Option types to C# code, as C# does not have a way to handle them. The options are either to introduce `FSharp.Core` as a dependency in your C# project (which is what you'd have to do if you're consuming an F# library *not* designed for interop with C#), or to change `None` values to `null`.

# Pre-F# 4.0

The way to do this is create a conversion function of your own:

    let OptionToObject opt =
        match opt with
        | Some x -> x 
        | None -> null

For value types you'd have to resort to boxing them or using `System.Nullable`.

    let OptionToNullable x = 
        match x with 
        | Some i -> System.Nullable i
        | None -> System.Nullable ()

# F# 4.0

In F# 4.0, the functions `ofObj`, `toObj`, `ofNullable`, and `toNullable` where introduced to the `Option` module. In F# interactive they can be used as follows:

    let l1 = [ Some 1 ; None ; Some 2]
    let l2 = l1 |> List.map Option.toNullable;;

    // val l1 : int option list = [Some 1; null; Some 2]
    // val l2 : System.Nullable<int> list = [1; null; 2]

    let l3 = l2 |> List.map Option.ofNullable;;
    // val l3 : int option list = [Some 1; null; Some 2]

    // Equality
    l1 = l2 // fails to compile: different types
    l1 = l3 // true
    
Note that `None` compiles to `null` internally. However as far as F# is concerned it is a `None`.

    let lA = [Some "a"; None; Some "b"]
    let lB = lA |> List.map Option.toObj

    // val lA : string option list = [Some "a"; null; Some "b"]
    // val lB : string list = ["a"; null; "b"]

    let lC = lB |> List.map Option.ofObj
    // val lC : string option list = [Some "a"; null; Some "b"]

    // Equality
    lA = lB // fails to compile: different types
    lA = lC // true




    

