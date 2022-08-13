---
title: "Discriminated Unions"
slug: "discriminated-unions"
draft: false
images: []
weight: 9892
type: docs
toc: true
---

## Naming elements of tuples within discriminated unions
When defining discriminated unions you can name elements of tuple types and use these names during pattern matching.

    type Shape = 
        | Circle of diameter:int
        | Rectangle of width:int * height:int
    
    let shapeIsTenWide = function
        | Circle(diameter=10) 
        | Rectangle(width=10) -> true
        | _ -> false

Additionally naming the elements of discriminated unions improves readability of the code and interoperability with C# - provided names will be used for properties' names and constructors' parameters. Default generated names in interop code are "Item", "Item1", "Item2"...

## Using Single-case Discriminated Unions as Records
Sometimes it is useful to create union types with only one case to implement record-like types:

```
type Point = Point of float * float

let point1 = Point(0.0, 3.0)

let point2 = Point(-2.5, -4.0)
```

These become very useful because they can be decomposed via pattern matching in the same way as tuple arguments can:

```
let (Point(x1, y1)) = point1
// val x1 : float = 0.0
// val y1 : float = 3.0

let distance (Point(x1,y1)) (Point(x2,y2)) =
    pown (x2-x1) 2 + pown (y2-y1) 2 |> sqrt
// val distance : Point -> Point -> float

distance point1 point2
// val it : float = 7.433034374
```



## Basic Discriminated Union Usage
Discriminated unions in F# offer a a way to define types which may hold any number of different data types. Their functionality is similar to C++ unions or VB variants, but with the additional benefit of being type safe.

    // define a discriminated union that can hold either a float or a string
    type numOrString = 
        | F of float
        | S of string

    let str = S "hi" // use the S constructor to create a string
    let fl = F 3.5 // use the F constructor to create a float
    
    // you can use pattern matching to deconstruct each type
    let whatType x = 
        match x with
            | F f -> printfn "%f is a float" f
            | S s -> printfn "%s is a string" s

    whatType str // hi is a string
    whatType fl // 3.500000 is a float

## RequireQualifiedAccess
With the `RequireQualifiedAccess` attribute, union cases must be referred to as `MyUnion.MyCase` instead of just `MyCase`. This prevents name collisions in the enclosing namespace or module:

    type [<RequireQualifiedAccess>] Requirements =
        None | Single | All

    // Uses the DU with qualified access
    let noRequirements = Requirements.None

    // Here, None still refers to the standard F# option case
    let getNothing () = None

    // Compiler error unless All has been defined elsewhere
    let invalid = All

If, for example, `System` has been opened, `Single` refers to `System.Single`. There is no collision with the union case `Requirements.Single`.

## Enum-style unions
Type information does not need to be included in the cases of a discriminated union. By omitting type information you can create a union that simply represents a set of choices, similar to an enum.

    // This union can represent any one day of the week but none of 
    // them are tied to a specific underlying F# type
    type DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday


## Converting to and from strings with Reflection
Sometimes it's necessary to convert a Discriminated Union to and from a string:

    module UnionConversion 
        open Microsoft.FSharp.Reflection
        
        let toString (x: 'a) = 
            match FSharpValue.GetUnionFields(x, typeof<'a>) with
            | case, _ -> case.Name

        let fromString<'a> (s : string) =
            match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with 
            | [|case|] -> Some(FSharpValue.MakeUnion(case, [||])) :?> 'a)
            | _ -> None



## Single case discriminated union
A single case discriminated union is like any other discriminated union except that it only has one case. 

    // Define single-case discriminated union type.
    type OrderId = OrderId of int
    // Construct OrderId type.
    let order = OrderId 123
    // Deconstruct using pattern matching. 
    // Parentheses used so compiler doesn't think it is a function definition.   
    let (OrderId id) = order

It is useful for enforcing type safety and commonly used in F# as opposed to C# and Java where creating new types comes with more overhead.

The following two alternative type definitions result in the same single-case discriminated union being declared:

    type OrderId = | OrderId of int

    type OrderId =
          | OrderId of int





## Recursive discriminated unions
# Recursive type

Discriminated unions can be recursive, that is they can refer to themselves in their definition. The prime example here is a tree:

    type Tree =
        | Branch of int * Tree list
        | Leaf of int

As an example, let's define the following tree:

        1
      2   5
    3   4

We can define this tree using our recursive discriminated union as follows:

    let leaf1 = Leaf 3
    let leaf2 = Leaf 4
    let leaf3 = Leaf 5

    let branch1 = Branch (2, [leaf1; leaf2])
    let tip = Branch (1, [branch1; leaf3])

Iterating over the tree is then just a matter of pattern matching:

    let rec toList tree =
        match tree with
        | Leaf x -> [x]
        | Branch (x, xs) -> x :: (List.collect toList xs)

    let treeAsList = toList tip // [1; 2; 3; 4; 5]

# Mutually dependent recursive types
One way to achieve recursion is to have nested mutually dependent types.

    // BAD
    type Arithmetic = {left: Expression; op:string; right: Expression}
    // illegal because until this point, Expression is undefined
    type Expression = 
    | LiteralExpr of obj
    | ArithmeticExpr of Arithmetic

Defining a record type directly inside a discriminated union is deprecated:

    // BAD
    type Expression = 
    | LiteralExpr of obj
    | ArithmeticExpr of {left: Expression; op:string; right: Expression}
    // illegal in recent F# versions

You can use the `and` keyword to chain mutually dependent definitions:

    // GOOD
    type Arithmetic = {left: Expression; op:string; right: Expression}
    and Expression = 
    | LiteralExpr of obj
    | ArithmeticExpr of Arithmetic

