---
title: "Statically Resolved Type Parameters"
slug: "statically-resolved-type-parameters"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
 - `s` is an instance of `^a` you want to accept at compile-time, which can be anything that implements the members you actually call using the syntax.
 - `^a` is similar to generics which would be `'a` (or `'A` or `'T` for example) but these are compile-time resolved, and allow for anything that fits all the requested usages within the method. (no interfaces required)

## Class, Interface, Record usage
    // Record
    type Ribbon = {Length:int}
    // Class
    type Line(len:int) = 
        member x.Length = len
    type IHaveALength =
        abstract Length:int
    
    let inline getLength s = (^a: (member Length: _) s)
    let ribbon = {Length=1}
    let line = Line(3)
    let someLengthImplementer =
        { new IHaveALength with
            member __.Length = 5}
    printfn "Our ribbon length is %i" (getLength ribbon)
    printfn "Our Line length is %i" (getLength line)
    printfn "Our Object expression length is %i" (getLength someLengthImplementer)


## Simple usage for anything that has a Length member
    let inline getLength s = (^a: (member Length: _) s)
    //usage:
    getLength "Hello World" // or "Hello World" |> getLength
    // returns 11


## Static member call
this will accept any type with a method named `GetLength` that takes nothing and returns an int:

`((^a : (static member GetLength : int) ()))` 





