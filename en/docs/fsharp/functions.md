---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Pipe Forward and Backward
Pipe operators are used to pass parameters to a function in a simple and elegant way. It allows to eliminate intermediate values and make function calls easier to read.

In F#, there are two pipe operators:

 - **Forward** (`|>`): Passing parameters from left to right

        let print message =
            printf "%s" message
        
        // "Hello World" will be passed as a parameter to the print function
        "Hello World" |> print

 - **Backward** (`<|`): Passing parameters from right to left

        let print message =
            printf "%s" message
        
        // "Hello World" will be passed as a parameter to the print function
        print <| "Hello World"

Here is an example without pipe operators:

    // We want to create a sequence from 0 to 10 then:
    // 1 Keep only even numbers
    // 2 Multiply them by 2
    // 3 Print the number

    let mySeq = seq { 0..10 }
    let filteredSeq = Seq.filter (fun c -> (c % 2) = 0) mySeq
    let mappedSeq = Seq.map ((*) 2) filteredSeq
    let finalSeq = Seq.map (sprintf "The value is %i.") mappedSeq
    
    printfn "%A" finalSeq

We can shorten the previous example and make it cleaner with the forward pipe operator:

    // Using forward pipe, we can eliminate intermediate let binding
    let finalSeq = 
        seq { 0..10 }
        |> Seq.filter (fun c -> (c % 2) = 0)
        |> Seq.map ((*) 2)
        |> Seq.map (sprintf "The value is %i.")

    printfn "%A" finalSeq

Each function result is passed as a parameter to the next function.

If you want to pass multiple parameters to the pipe operator, you have to add a `|` for each additional parameter and create a Tuple with the parameters. Native F# pipe operator supports up to three parameters (|||> or <|||).

    let printPerson name age =
        printf "My name is %s, I'm %i years old" name age
    
    ("Foo", 20) ||> printPerson



## Functions of more than one parameter
In F#, **all functions take exactly one parameter**. This seems an odd statement, since it's trivially easy to declare more than one parameter in a function declaration:

    let add x y = x + y

But if you type that function declaration into the F# interactive interpreter, you'll see that its type signature is:

    val add : x:int -> y:int -> int

Without the parameter names, that signature is `int -> int -> int`. The `->` operator is right-associative, which means that this signature is equivalent to `int -> (int -> int)`. In other words, `add` is a function that takes one `int` parameter, and returns **a function that takes one `int` and returns `int`**. Try it:

    let addTwo = add 2
    // val addTwo : (int -> int)
    let five = addTwo 3
    // val five : int = 5

However, you can also call a function like `add` in a more "conventional" manner, directly passing it two parameters, and it will work like you'd expect:

    let three = add 1 2
    // val three : int = 3

This applies to functions with as many parameters as you want:

    let addFiveNumbers a b c d e = a + b + c + d + e
    // val addFiveNumbers : a:int -> b:int -> c:int -> d:int -> e:int -> int
    let addFourNumbers = addFiveNumbers 0
    // val addFourNumbers : (int -> int -> int -> int -> int)
    let addThreeNumbers = addFourNumbers 0
    // val addThreeNumbers : (int -> int -> int -> int)
    let addTwoNumbers = addThreeNumbers 0
    // val addTwoNumbers : (int -> int -> int)
    let six = addThreeNumbers 1 2 3  // This will calculate 0 + 0 + 1 + 2 + 3
    // val six : int = 6

This method of thinking about multi-parameter functions as being functions that take one parameter and return new functions (which may in turn take one parameter and return new functions, until you reach the final function that takes the final parameter and finally returns a result) is called **currying**, in honor of mathematician Haskell Curry, who is famous for developing the concept. (It was invented by someone else, but Curry deservedly gets most of the credit for it.)

This concept is used throughout F#, and you'll want to be familiar with it.

## Basics of functions
Most functions in F# are created with the `let` syntax:

    let timesTwo x = x * 2

This defines a function named `timesTwo` that takes a single parameter `x`. If you run an interactive F# session (`fsharpi` on OS X and Linux, `fsi.exe` on Windows) and paste that function in (and add the `;;` that tells `fsharpi` to evaluate the code you just typed), you'll see that it replies with:

    val timesTwo : x:int -> int

This means that `timesTwo` is a function that takes a single parameter `x` of type `int`, and returns an `int`. Function signatures are often written without the parameter names, so you'll often see this function type written as `int -> int`.

But wait! How did F# know that `x` was an integer parameter, since you never specified its type? That's due to **type inference**. Because in the function body, you multiplied `x` by `2`, the types of `x` and `2` must be the same. (As a general rule, F# will not implicitly cast values to different types; you must explicitly specify any typecasts you want).

If you want to create a function that doesn't take any parameters, this is the **wrong** way to do it:

    let hello =  // This is a value, not a function
        printfn "Hello world"

The **right** way to do it is:

    let hello () =
        printfn "Hello world"

This `hello` function has the type `unit -> unit`, which is explained in https://www.wikiod.com/docs/f%23/2513/the-unit-type.

## Curried vs Tupled Functions
There are two ways to define functions with multiple parameters in F#, Curried functions and Tupled functions.

    let curriedAdd x y = x + y // Signature: x:int -> y:int -> int
    let tupledAdd (x, y) = x + y // Signature:  x:int * y:int -> int

All functions defined from outside F# (such as the .NET framework) are used in F# with the Tupled form. Most functions in F# core modules are used with Curried form.

The Curried form is considered idiomatic F#, because it allows for partial application. Neither of the following two examples are possible with the Tupled form.

    let addTen = curriedAdd 10 // Signature: int -> int

    // Or with the Piping operator
    3 |> curriedAdd 7 // Evaluates to 10

The reason behind that is that the Curried function, when called with one parameter, returns a function. Welcome to functional programming !!

    let explicitCurriedAdd x = (fun y -> x + y) // Signature: x:int -> y:int -> int
    let veryExplicitCurriedAdd = (fun x -> (fun y -> x + y)) // Same signature

You can see it is exactly the same signature.

However, when interfacing with other .NET code, as in when writing libraries, it is important to define functions using the Tupled form.

## Inlining
Inlining allows you to replace a call to a function with the body of the function. 

This is sometimes useful for performance reason on critical part of the code. But the counterpart is that your assembly will takes much space since the body of the function is duplicated everywhere a call occurred. You have to be careful when deciding whether to inline a function or not.

A function can be inlined with the `inline` keyword:

    // inline indicate that we want to replace each call to sayHello with the body 
    // of the function
    let inline sayHello s1 =
        sprintf "Hello %s" s1
   
    // Call to sayHello will be replaced with the body of the function
    let s = sayHello "Foo"
    // After inlining -> 
    // let s = sprintf "Hello %s" "Foo"

    printfn "%s" s
    // Output
    // "Hello Foo"

Another example with local value:

    let inline addAndMulti num1 num2 =
        let add = num1 + num2
        add * 2
    
    let i = addAndMulti 2 2
    // After inlining ->
    // let add = 2 + 2
    // let i = add * 2

    printfn "%i" i
    // Output
    // 8

