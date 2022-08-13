---
title: "Sequence"
slug: "sequence"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Generate sequences
There are multiple ways to create a sequence.

You can use functions from the Seq module:

    // Create an empty generic sequence
    let emptySeq = Seq.empty 
   
    // Create an empty int sequence
    let emptyIntSeq = Seq.empty<int> 
    
    // Create a sequence with one element    
    let singletonSeq = Seq.singleton 10 

    // Create a sequence of n elements with the specified init function
    let initSeq = Seq.init 10 (fun c -> c * 2) 

    // Combine two sequence to create a new one
    let combinedSeq = emptySeq |> Seq.append singletonSeq

    // Create an infinite sequence using unfold with generator based on state
    let naturals = Seq.unfold (fun state -> Some(state, state + 1)) 0
    
   

You can also use sequence expression:

    // Create a sequence with element from 0 to 10
    let intSeq = seq { 0..10 }

    // Create a sequence with an increment of 5 from 0 to 50
    let intIncrementSeq = seq{ 0..5..50 }

    // Create a sequence of strings, yield allow to define each element of the sequence
    let stringSeq = seq {
        yield "Hello"
        yield "World"
    }

    // Create a sequence from multiple sequence, yield! allow to flatten sequences
    let flattenSeq = seq {
        yield! seq { 0..10 }
        yield! seq { 11..20 }
    }






## Introduction to sequences
A sequence is a series of elements that can be enumerated.  It is an alias of System.Collections.Generic.IEnumerable and lazy. It stores a series of elements of the same type (can be any value or object, even another sequence). Functions from the Seq.module can be used to operate on it. 

Here is a simple example of a sequence enumeration:

    let mySeq = { 0..20 } // Create a sequence of int from 0 to 20
    mySeq
    |> Seq.iter (printf "%i ") // Enumerate each element of the sequence and print it

Output:

    0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20



## Seq.map
    let seq = seq {0..10}

    s |> Seq.map (fun x -> x * 2)

    > val it : seq<int> = seq [2; 4; 6; 8; ...]

Apply a function to every element of a sequence using Seq.map

## Seq.filter
Suppose that we have a sequence of integers and we want to create a sequence that contains only the even integers. We can obtain the latter by using the ```filter``` function of the Seq module. The ```filter``` function has the type signature ```('a -> bool) -> seq<'a> -> seq<'a>```; this indicates that it accepts a function that returns true or false (sometimes called a predicate) for a given input of type ```'a``` and a sequence that comprises values of type ```'a``` to yield a sequence that comprises values of type ```'a```.

    // Function that tests if an integer is even
    let isEven x = (x % 2) = 0
    
    // Generates an infinite sequence that contains the natural numbers
    let naturals = Seq.unfold (fun state -> Some(state, state + 1)) 0
     
    // Can be used to filter the naturals sequence to get only the even numbers
    let evens = Seq.filter isEven naturals 

## Infinite repeating sequences
    let data = [1; 2; 3; 4; 5;]
    let repeating = seq {while true do yield! data}

Repeating sequences can be created using a `seq {}` computation expression

