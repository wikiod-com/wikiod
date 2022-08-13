---
title: "Type and Module Extensions"
slug: "type-and-module-extensions"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

In all cases when extending types and modules, the extending code must be added/loaded before the code that is to call it. It must also be made available to the calling code by [opening/importing][1] the relevant namespaces.


  [1]: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/import-declarations-the-open-keyword-%5Bfsharp%5D

## Adding new methods/properties to existing types
F# allows functions to be added as "members" to types when they are defined (for example, [Record Types][1]). However F# also allows new instance members to be added to _existing_ types - even ones declared elsewhere and in other .net languages.

The following example adds a new instance method `Duplicate` to all instances of `String`.

    type System.String with
        member this.Duplicate times = 
            Array.init times (fun _ -> this)

**Note**: `this` is an arbitrarily chosen variable name to use to refer to the instance of the type that is being extended - `x` would work just as well, but would perhaps be less self-describing. 

It can then be called in the following ways.

    // F#-style call
    let result1 = "Hi there!".Duplicate 3

    // C#-style call
    let result2 = "Hi there!".Duplicate(3)

    // Both result in three "Hi there!" strings in an array

This functionality is very similar to [Extension Methods][2] in C#.

New properties can also be added to existing types in the same way.  They will automatically become properties if the new member takes no arguments.

    type System.String with
        member this.WordCount =
            ' ' // Space character
            |> Array.singleton
            |> fun xs -> this.Split(xs, StringSplitOptions.RemoveEmptyEntries)
            |> Array.length

    let result = "This is an example".WordCount
    // result is 4

  [1]: https://www.wikiod.com/docs/f%23/1136/records/3661/add-member-functions-to-records#t=201607230429137611809
  [2]: https://msdn.microsoft.com/en-AU/library/bb383977.aspx

## Adding new functions to existing modules and types using Modules
Modules can be used to add new functions to existing Modules and Types.

    namespace FSharp.Collections
    
    module List =
        let pair item1 item2 = [ item1; item2 ]
    
The new function can then be called as if it was an original member of List.

    open FSharp.Collections    

    module Testing =
        let result = List.pair "a" "b"
        // result is a list containing "a" and "b"


## Adding new static functions to existing types
F# allow existing types to be extended with new static functions.

    type System.String with
        static member EqualsCaseInsensitive (a, b) = String.Equals(a, b, StringComparison.OrdinalIgnoreCase)
    
This new function can be invoked like this:

    let x = String.EqualsCaseInsensitive("abc", "aBc")
    // result is True

This feature can mean that rather than having to create "utility" libraries of functions, they can be added to relevant existing types.  This can be useful to create more F#-friendly versions of functions that allow features such as [currying][1].

    type System.String with
        static member AreEqual comparer a b = System.String.Equals(a, b, comparer)
    
    let caseInsensitiveEquals = String.AreEqual StringComparison.OrdinalIgnoreCase
    
    let result = caseInsensitiveEquals "abc" "aBc"
    // result is True


  [1]: http://stackoverflow.com/q/8448/4289902

