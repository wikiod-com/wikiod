---
title: "Active Patterns"
slug: "active-patterns"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

## Simple Active Patterns
Active patterns are a special type of pattern matching where you can specify named categories that your data may fall into, and then use those categories in `match` statements.

To define an active pattern that classifies numbers as positive, negative or zero:

    let (|Positive|Negative|Zero|) num = 
        if num > 0 then Positive 
        elif num < 0 then Negative
        else Zero

This can then be used in a pattern matching expression:

    let Sign value = 
        match value with
        | Positive -> printf "%d is positive" value
        | Negative -> printf "%d is negative" value
        | Zero -> printf "The value is zero"

    Sign -19 // -19 is negative
    Sign 2 // 2 is positive
    Sign 0 // The value is zero

## Active Patterns can be used to validate and transform function arguments
An interesting but rather unknown usage of Active Patterns in `F#` is that they can be used to validate and transform function arguments.

Consider the classic way to do argument validation:

    // val f : string option -> string option -> string
    let f v u =
      let v = defaultArg v "Hello"
      let u = defaultArg u "There"
      v + " " + u

    // val g : 'T -> 'T (requires 'T null)
    let g v =
      match v with
      | null  -> raise (System.NullReferenceException ())
      | _     -> v.ToString ()

Typically we add code in the method to verify that arguments are correct. 
Using Active Patterns in `F#` we can generalize this and declare the intent in the argument declaration.

The following code is equivalent to the code above:

    let inline (|DefaultArg|) dv ov = defaultArg ov dv

    let inline (|NotNull|) v =
      match v with
      | null  -> raise (System.NullReferenceException ())
      | _     -> v

    // val f : string option -> string option -> string
    let f (DefaultArg "Hello" v) (DefaultArg "There" u) = v + " " + u

    // val g : 'T -> string (requires 'T null)
    let g (NotNull v) = v.ToString ()

For the user of function `f` and `g` there's no difference between the two different versions.

    printfn "%A" <| f (Some "Test") None  // Prints "Test There"
    printfn "%A" <| g "Test"              // Prints "Test"
    printfn "%A" <| g null                // Will throw

A concern is if Active Patterns adds performance overhead. Let's use `ILSpy` to decompile `f` and `g` to see if that is the case.

    public static string f(FSharpOption<string> _arg2, FSharpOption<string> _arg1)
    {
      return Operators.DefaultArg<string>(_arg2, "Hello") + " " + Operators.DefaultArg<string>(_arg1, "There");
    }

    public static string g<a>(a _arg1) where a : class
    {
      if (_arg1 != null)
      {
        a a = _arg1;
        return a.ToString();
      }
      throw new NullReferenceException();
    }

Thanks to `inline` the Active Patterns adds no extra overhead compared to the classic way of the doing argument validation.

## Active Patterns as .NET API wrappers
Active Patterns can be used to make calling some .NET API's feel more natural, particularly those that use an output parameter to return more than just the function return value. 

For example, you'd normally call the `System.Int32.TryParse` method as follows:

    let couldParse, parsedInt = System.Int32.TryParse("1")
    if couldParse then printfn "Successfully parsed int: %i" parsedInt
    else printfn "Could not parse int"

You can improve this a bit using pattern matching:

    match System.Int32.TryParse("1") with 
    | (true, parsedInt) -> printfn "Successfully parsed int: %i" parsedInt
    | (false, _) -> printfn "Could not parse int"

However, we can also define the following Active Pattern that wraps the `System.Int32.TryParse` function:

    let (|Int|_|) str =
        match System.Int32.TryParse(str) with
        | (true, parsedInt) -> Some parsedInt
        | _ -> None

Now we can do the following:

    match "1" with
    | Int parsedInt -> printfn "Successfully parsed int: %i" parsedInt
    | _ -> printfn "Could not parse int"

Another good candidate for being wrapped in an Active Patterns are the regular expressions API's:


    let (|MatchRegex|_|) pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern) 
        if m.Success then Some m.Groups.[1].Value 
        else None  

    match "bad" with
    | MatchRegex "(good|great)" mood -> 
        printfn "Wow, it's a %s day!" mood
    | MatchRegex "(bad|terrible)" mood -> 
        printfn "Unfortunately, it's a %s day." mood
    | _ -> 
        printfn "Just a normal day"

## Active Patterns with parameters
Active patterns are just simple functions. 

Like functions you can define additional parameters:

    let (|HasExtension|_|) expected (uri : string) = 
        let result = uri.EndsWith (expected, StringComparison.CurrentCultureIgnoreCase)
        match result with
        | true -> Some true
        | _ -> None

This can be used in a pattern matching this way:

        let isXMLFile uri =
            match uri with
            | HasExtension ".xml" _ -> true
            | _ -> false

## Complete and Partial Active Patterns
There are two types of Active Patterns that somewhat differ in usage - Complete and Partial.

Complete Active Patterns can be used when you are able to enumerate all the outcomes, like "is a number odd or even?"

    let (|Odd|Even|) number = 
      if number % 2 = 0
      then Even
      else Odd

Notice the Active Pattern definition lists both possible cases and nothing else, and the body returns one of the listed cases. When you use it in a match expression, this is a complete match:

    let n = 13
    match n with
    | Odd -> printf "%i is odd" n
    | Even -> printf "%i is even" n

This is handy when you want to break down the input space into known categories that cover it completely.

Partial Active Patterns on the other hand let you explicitly ignore some possible results by returning an `option`. Their definition uses a special case of `_` for the unmatched case.

    let (|Integer|_|) str =
      match Int32.TryParse(str) with
      | (true, i) -> Some i
      | _ -> None

This way we can match even when some cases cannot be handled by our parsing function.

    let s = "13"
    match s with
    | Integer i -> "%i was successfully parsed!" i
    | _ -> "%s is not an int" s

Partial Active Patterns can be used as a form of testing, whether the input falls into a specific category in the input space while ignoring other options.

