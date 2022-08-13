---
title: "Memoization"
slug: "memoization"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Simple memoization
Memoization consists of caching function results to avoid computing the same result multiple times. This is useful when working with functions that perform costly computations.

We can use a simple factorial function as an example:

    let factorial index =
        let rec innerLoop i acc =
            match i with
            | 0 | 1 -> acc
            | _ -> innerLoop (i - 1) (acc * i)
    
        innerLoop index 1

Calling this function multiple times with the same parameter results in the same computation again and again.

Memoization will help us to cache the factorial result and return it if the same parameter is passed again.

Here is a simple memoization implementation :

    // memoization takes a function as a parameter
    // This function will be called every time a value is not in the cache
    let memoization f =
        // The dictionary is used to store values for every parameter that has been seen
        let cache = Dictionary<_,_>()
        fun c ->
            let exist, value = cache.TryGetValue (c)
            match exist with
            | true -> 
                // Return the cached result directly, no method call
                printfn "%O -> In cache" c
                value
            | _ -> 
                // Function call is required first followed by caching the result for next call with the same parameters
                printfn "%O -> Not in cache, calling function..." c
                let value = f c
                cache.Add (c, value)
                value

The `memoization` function simply takes a function as a parameter and returns a function with the same signature. You could see this in the method signature `f:('a -> 'b) -> ('a -> 'b)`. This way you can use memoization the same way as if you were calling the factorial method. 

The `printfn` calls are to show what happens when we call the function multiple times; they can be removed safely.

Using memoization is easy:

    // Pass the function we want to cache as a parameter via partial application
    let factorialMem = memoization factorial

    // Then call the memoized function
    printfn "%i" (factorialMem 10)
    printfn "%i" (factorialMem 10)
    printfn "%i" (factorialMem 10)
    printfn "%i" (factorialMem 4)
    printfn "%i" (factorialMem 4)

    // Prints
    // 10 -> Not in cache, calling function...
    // 3628800
    // 10 -> In cache
    // 3628800
    // 10 -> In cache
    // 3628800
    // 4 -> Not in cache, calling function...
    // 24
    // 4 -> In cache
    // 24

## Memoization in a recursive function
Using the previous example of calculating the factorial of an integer, put in the hash table all values of factorial calculated inside the recursion, that do not appear in the table.

As in the article about [memoization](https://fr.wikipedia.org/wiki/M%C3%A9mo%C3%AFsation), we declare a function ``f`` that accepts a function parameter ``fact`` and a integer parameter. This function ``f``, includes instructions to calculate the factorial of ``n`` from ``fact(n-1)``.

This allows to handle recursion by the function returned by ``memorec`` and not by ``fact`` itself and possibly stop the calculation if the ``fact(n-1)`` value has been already calculated and is located in the hash table.

    let memorec f =
       let cache = Dictionary<_,_>()
       let rec frec n = 
           let value = ref 0
           let exist = cache.TryGetValue(n, value)
           match exist with
           | true -> 
               printfn "%O -> In cache" n
           | false ->
               printfn "%O -> Not in cache, calling function..." n
               value := f frec n
               cache.Add(n, !value)
           !value
       in frec
    
    let f = fun fact n -> if n<2 then 1 else n*fact(n-1)
    
    [<EntryPoint>]
    let main argv = 
        let g = memorec(f)
        printfn "%A" (g 10)
        printfn "%A" "---------------------"
        printfn "%A" (g 5)
        Console.ReadLine()
        0

Result:

    10 -> Not in cache, calling function...
    9 -> Not in cache, calling function...
    8 -> Not in cache, calling function...
    7 -> Not in cache, calling function...
    6 -> Not in cache, calling function...
    5 -> Not in cache, calling function...
    4 -> Not in cache, calling function...
    3 -> Not in cache, calling function...
    2 -> Not in cache, calling function...
    1 -> Not in cache, calling function...
    3628800
    "---------------------"
    5 -> In cache
    120


