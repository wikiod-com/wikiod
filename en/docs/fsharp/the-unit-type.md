---
title: "The unit type"
slug: "the-unit-type"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## What good is a 0-tuple?
A 2-tuple or a 3-tuple represent a group of related items. (Points in 2D space, RGB values of a color, etc.) A 1-tuple is not very useful since it could easily be replaced with a single `int`.

A 0-tuple seems even more useless since it contains absolutely *nothing*. Yet it has properties that make it very useful in functional languages like F#. For example, the 0-tuple type has exactly *one* value, usually represented as `()`. All 0-tuples have this value so it's essentially a singleton type. In most functional programming languages, including F#, this is called the `unit` type.

Functions that return `void` in C# will return the `unit` type in F#:

    let printResult = printfn "Hello"

Run that in the F# interactive interpreter, and you'll see:

    val printResult : unit = ()

This means that the value `printResult` is of type `unit`, and has the value `()` (the empty tuple, the one and only value of the `unit` type).

Functions can take the `unit` type as a parameter, too. In F#, functions may look like they're taking no parameters. But in fact, they're taking a single parameter of type `unit`. This function:

    let doMath() = 2 + 4

is actually equivalent to:

    let doMath () = 2 + 4

That is, a function that takes one parameter of type `unit` and returns the `int` value 6. If you look at the type signature that the F# interactive interpreter prints when you define this function, you'll see:

    val doMath : unit -> int

The fact that all functions will take at least one parameter and return a value, even if that value is sometimes a "useless" value like `()`, means that function composition is a lot easier in F# than in languages that don't have the `unit` type. But that's a more advanced subject which we'll get to later on. For now, just remember that when you see `unit` in a function signature, or `()` in a function's parameters, that's the 0-tuple type that serves as the way to say "This function takes, or returns, no meaningful values."

## Deferring execution of code
We can use the `unit` type as a function argument to define functions that we don't want executed until later.  This is often useful in asynchronous background tasks, when the main thread may want trigger some predefined functionality of the background thread, like maybe moving it to a new file, or if a a let-binding should not be run immediately:

    module Time =
        let now = System.DateTime.Now   // value is set and fixed for duration of program
        let now() = System.DateTime.Now // value is calculated when function is called (each time)

In the following code, we define code to start a "worker" which simply prints out the value it is working on every 2 seconds.  The worker then returns two functions which may be used to control it - one which moves it to the next value to work on, and one which stops it from working.  These must be functions, because we do not want their bodies to be executed until we choose to, otherwise the worker would immediately move to the second value and shutdown without having done anything.

    let startWorker value =
        let current = ref value
        let stop = ref false
        let nextValue () = current := !current + 1
        let stopOnNextTick () = stop := true
        let rec loop () = async {
            if !stop then
                printfn "Stopping work."
                return ()
            else
                printfn "Working on %d." !current
                do! Async.Sleep 2000
                return! loop () }
        Async.Start (loop ())
        nextValue, stopOnNextTick

We can then start a worker by doing

    let nextValue, stopOnNextTick = startWorker 12

and the work will begin - if we are in F# interactive, we will see the messages printed out in the console every two seconds.  We can then run

    nextValue ()

and we will see the messages indicating that value being worked on has moved to the next one.

When it is time to finish working, we can run the

    stopOnNextTick ()

function, which will print out the closing message, then exit.

The `unit` type is important here to signify "no input" - the functions already have all the information they need to work built into them, and the caller is not allowed to change that.

