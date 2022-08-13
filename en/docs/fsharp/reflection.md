---
title: "Reflection"
slug: "reflection"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Robust reflection using F# quotations 
Reflection is useful but fragile. Consider this:

    let mi  = typeof<System.String>.GetMethod "StartsWith"

The problems with this kind of code are:

  1. The code doesn't work because there are several overloads of `String.StartsWith`
  2. Even if there wouldn't be any overloads right now later versions of the library might add an overload causing a runtime crash
  3. Refactoring tools like `Rename methods` is broken with reflection.

This means we get a runtime crashes for something that is known compile-time. That seems suboptimal. 

Using `F#` quotations it's possible to avoid all the problems above. We define some helper functions:

    open FSharp.Quotations
    open System.Reflection

    let getConstructorInfo (e : Expr<'T>) : ConstructorInfo =
      match e with
      | Patterns.NewObject (ci, _) -> ci
      | _ -> failwithf "Expression has the wrong shape, expected NewObject (_, _) instead got: %A" e


    let getMethodInfo (e : Expr<'T>) : MethodInfo =
      match e with
      | Patterns.Call (_, mi, _) -> mi
      | _ -> failwithf "Expression has the wrong shape, expected Call (_, _, _) instead got: %A" e

We use the functions like this:

    printfn "%A" <| getMethodInfo <@ "".StartsWith "" @>
    printfn "%A" <| getMethodInfo <@ List.singleton 1 @>
    printfn "%A" <| getConstructorInfo <@ System.String [||] @>

This prints:

    Boolean StartsWith(System.String)
    Void .ctor(Char[])
    Microsoft.FSharp.Collections.FSharpList`1[System.Int32] Singleton[Int32](Int32)

`<@ ... @>` means that instead of executing the expression inside `F#` generates an expression tree representing the expression. 
`<@ "".StartsWith "" @>` generates an expression tree that looks like this: `Call (Some (Value ("")), StartsWith, [Value ("")])`.
This expression tree matches what `getMethodInfo` expects and it will return the correct method info.

This resolves all the problems listed above.

