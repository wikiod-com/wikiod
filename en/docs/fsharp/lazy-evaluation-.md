---
title: "Lazy Evaluation "
slug: "lazy-evaluation-"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Lazy Evaluation Introduction
Most programming languages, including F#, evaluate computations immediately in accord with a model called Strict Evaluation. However, in Lazy Evaluation, computations are not evaluated until they are needed. F# allows us to use lazy evaluation through both the ```lazy``` keyword and [`sequences`][1].

    // define a lazy computation
    let comp = lazy(10 + 20)

    // we need to force the result
    let ans = comp.Force()
    


In addition, when using Lazy Evaluation, the results of the computation are cached so if we force the result after our first instance of forcing it, the expression itself won't be evaluated again

    let rec factorial n = 
      if n = 0 then 
        1
      else 
        (factorial (n - 1)) * n

    
    let computation = lazy(printfn "Hello World\n"; factorial 10)
    
    // Hello World will be printed
    let ans = computation.Force()
    
    // Hello World will not be printed here
    let ansAgain = computation.Force()
    
[1]:https://www.wikiod.com/docs/f%23/2354/sequence#t=201607251355155668897

## Introduction to Lazy Evaluation in F#
F#, like most programming languages, uses Strict Evaluation by default. In Strict Evaluation, computations are executed immediately. In contrast, Lazy Evaluation, defers execution of computations until their results are needed. Moreover, the results of a computation under Lazy Evaluation are cached, thereby obviating the need for the re-evaluation of an expression.

We can use Lazy evaluation in F# through both the ```lazy``` keyword and [`Sequences`][1]

    // 23 * 23 is not evaluated here
    // lazy keyword creates lazy computation whose evaluation is deferred 
    let x = lazy(23 * 23)

    // we need to force the result
    let y = x.Force()

    // Hello World not printed here
    let z = lazy(printfn "Hello World\n"; 23424)
    
    // Hello World printed and 23424 returned
    let ans1 = z.Force()

    // Hello World not printed here as z as already been evaluated, but 23424 is
    // returned
    let ans2 = z.Force()


[1]:https://www.wikiod.com/docs/f%23/2354/sequence#t=201607251355155668897

