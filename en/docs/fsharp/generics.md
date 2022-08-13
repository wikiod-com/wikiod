---
title: "Generics"
slug: "generics"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Reversal of a list of any type
To reverse a list, it isn't important what type the list elements are, only what order they're in.  This is a perfect candidate for a generic function, so the same reverseal function can be used no matter what list is passed.

    let rev list =
        let rec loop acc = function
            | []           -> acc
            | head :: tail -> loop (head :: acc) tail
        loop [] list

The code makes no assumptions about the types of the elements.  The compiler (or F# interactive) will tell you that the type signature of this function is `'T list -> 'T list`.  The `'T` tells you that it's a generic type with no constraints.  You may also see `'a` instead of `'T` - the letter is unimportant because it's only a _generic_ placeholder.  We can pass an `int list` or a `string list`, and both will work successfully, returning an `int list` or a `string list` respectively.

For example, in F# interactive:

    > let rev list = ...
    val it : 'T list -> 'T list
    > rev [1; 2; 3; 4];;
    val it : int list = [4; 3; 2; 1]
    > rev ["one", "two", "three"];;
    val it : string list = ["three", "two", "one"]

## Mapping a list into a different type
    let map f list =
        let rec loop acc = function
            | []           -> List.rev acc
            | head :: tail -> loop (f head :: acc) tail
        loop [] list

The signature of this function is `('a -> 'b) -> 'a list -> 'b list`, which is the most generic it can be.  This does not prevent `'a` from being the same type as being `'b`, but it also allows them to be different.  Here you can see that the `'a` type that is the parameter to the function `f` must match the type of the `list` parameter.  This function is still generic, but there are some slight constraints on the inputs - if the types don't match, there will be a compile error.

Examples:

    > let map f list = ...
    val it : ('a -> 'b) -> 'a list -> 'b list
    > map (fun x -> float x * 1.5) [1; 2; 3; 4];;
    val it : float list = [1.5; 3.0; 4.5; 6.0]
    > map (sprintf "abc%.1f") [1.5; 3.0; 4.5; 6.0];;
    val it : string list = ["abc1.5"; "abc3.0"; "abc4.5"; "abc6.0"]
    > map (fun x -> x + 1) [1.0; 2.0; 3.0];;
    error FS0001: The type 'float' does not match the type 'int'

