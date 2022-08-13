---
title: "Functions and Partial Application"
slug: "functions-and-partial-application"
draft: false
images: []
weight: 9880
type: docs
toc: true
---

## Syntax
- -- defining a function with no arguments looks the same as simply defining a value  
language = "Elm"
- -- calling a function with no arguments by stating its name  
language
- -- parameters are separated by spaces and follow the function's name  
add x y = x + y
- -- call a function in the same way  
add 5 2
- -- partially apply a function by providing only some of its parameters  
increment = add 1
- -- use the |> operator to pass the expression on the left to the function on the right  
ten = 9 |> increment
- -- the <| operator passes the expression on the right to the function on the left  
increment <| add 5 4
- -- chain/compose two functions together with the >> operator  
backwardsYell = String.reverse >> String.toUpper
- -- the << works the same in the reverse direction  
backwardsYell = String.toUpper << String.reverse
- -- a function with a non-alphanumeric name in parentheses creates a new operator  
(#) x y = x * y  
ten = 5 # 2 
- -- any infix operator becomes a normal function when you wrap it in parentheses  
ten = (+) 5 5
- -- optional type annotations appear above function declarations  
isTen : Int -> Bool  
isTen n = if n == 10 then True else False

## Partial Application
Partial application means calling a function with less arguments than it has and saving the result as another function (that waits for the rest of the arguments).

<!-- language: lang-elm -->

    multiplyBy: Int -> Int -> Int    
    multiplyBy x y =
        x * y
    
    
    multiplyByTwo : Int -> Int  -- one Int has disappeared! we now know what x is.
    multiplyByTwo =
        multiplyBy 2


    > multiplyByTwo 2
    4 : Int

    > multiplyByTwo 4
    8 : Int

As an academic sidenote, Elm can do this because of [currying][1] behind the scenes.


  [1]: https://en.wikipedia.org/wiki/Currying

## Local variables
It is possible to define local variables inside a function to

 - reduce code repetition
 - give name to subexpressions
 - reduce the amount of passed arguments.

The construct for this is `let ... in ...`.

<!-- language: lang-elm -->

    bigNumbers =
        let
            allNumbers =
                [1..100]

            isBig number =
                number > 95
        in
            List.filter isBig allNumbers

    > bigNumbers
    [96,97,98,99,100] : List number

    > allNumbers
    -- error, doesn't know what allNumbers is!

The order of definitions in the first part of `let` doesn't matter!

<!-- language: lang-elm -->

    outOfOrder =
        let
            x =
                y + 1  -- the compiler can handle this

            y =
                100
        in
            x + y

    > outOfOrder
    201 : number

## Overview
Function application syntax in Elm does not use parenthesis or commas, and is instead whitespace-sensitive.

To define a function, specify its name `multiplyByTwo` and arguments `x`, any operations after equal sign `=` is what returned from a function. 

<!-- language: lang-elm -->

    multiplyByTwo x =
        x * 2

To call a function, specify its name and arguments:

<!-- language: lang-elm -->

    multiplyByTwo 2  -- 4

Note that syntax like `multiplyByTwo(2)` is not necessary (even though the compiler doesn't complain). The parentheses only serve to resolve precedence:

<!-- language: lang-elm -->

    > multiplyByTwo multiplyByTwo 2
    -- error, thinks it's getting two arguments, but it only needs one

    > multiplyByTwo (multiplyByTwo 2)
    4 : number

    > multiplyByTwo 2 + 2
    6 : number
    -- same as (multiplyByTwo 2) + 2

    > multiplyByTwo (2 + 2)
    8 : number



## Lambda expressions
Elm has a special syntax for lambda expressions or anonymous functions:

<!-- language: lang-elm -->

    \arguments -> returnedValue

For example, as seen in `List.filter`:

<!-- language: lang-elm -->

    > List.filter (\num -> num > 1) [1,2,3]
    [2,3] : List number

More to the depth, a backward slash, `\`, is used to mark the beginning of lambda expression, and the arrow, `->`, is used to delimit arguments from the function body. If there are more arguments, they get separated by a space:

<!-- language: lang-elm -->

    normalFunction x y = x + y
    -- is equivalent to
    lambdaFunction = \x y -> x + y

    > normalFunction 1 2
    3 : number

    > lambdaFunction 1 2
    3 : number

## Strict and delayed evaluation
In elm, a function's value is computed when the last argument is applied.  In the example below, the diagnostic from `log` will be printed when f is invoked with 3 arguments or a curried form of f is applied with the last argument.

<!-- language: lang-hs -->

    import String
    import Debug exposing (log)

    f a b c = String.join "," (log "Diagnostic" [a,b,c]) -- <function> : String -> String -> String -> String

    f2 = f "a1" "b2" -- <function> : String -> String

    f "A" "B" "C"
    -- Diagnostic: ["A","B","C"]
    "A,B,C" : String

    f2 "c3"
    -- Diagnostic: ["a1","b2","c3"]
    "a1,b2,c3" : String

At times you'll want to prevent a function from being applied right away.  A typical use in elm is [`Lazy.lazy`](http://package.elm-lang.org/packages/elm-lang/lazy/1.0.0/Lazy#lazy) which provides an abstraction for controlling when functions are applied.

    lazy : (() -> a) -> Lazy a

Lazy computations take a function of one `()` or `Unit` type argument.  The unit type is conventionally the type of a placeholder argument.  In an argument list, the corresponding argument is specified as `_`, indicating that the value isn't used.  The unit value in elm is specified by the special symbol `()` which can conceptually represent an empty tuple, or a hole.  It resembles the empty argument list in C, Javascript and other languages that use parenthesis for function calls, but it's an ordinary value.

In our example, `f` can be protected from being evaluated immediately with a lambda:

<!-- language: lang-hs -->

    doit f = f () -- <function> : (() -> a) -> a
    whatToDo = \_ -> f "a" "b" "c" -- <function> : a -> String
    -- f is not evaluated yet

    doit whatToDo
    -- Diagnostic: ["a","b","c"]
    "a,b,c" : String

Function evaluation is delayed any time a function is partially applied.

<!-- language: lang-hs -->

    defer a f = \_ -> f a -- <function> : a -> (a -> b) -> c -> b

    delayF = f "a" "b" |> defer "c" -- <function> : a -> String

    doit delayF
    -- Diagnostic: ["a","b","c"]
    "a,b,c" : String

Elm has an [`always`](http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Basics#always) function, which cannot be used to delay evaluation.  Because elm evaluates all function arguments regardless of whether and when the result of the function application is used, wrapping a function application in `always` won't cause a delay, because `f` is fully applied as a parameter to `always`.

<!-- language: lang-hs -->

    alwaysF = always (f "a" "b" "c") -- <function> : a -> String
    -- Diagnostic: ["a","b","c"] -- Evaluation wasn't delayed.
    


## Infix operators and infix notation
Elm allows the definition of custom infix operators.

Infix operators are defined using parenthesis around the name of a function.

Consider this example of infix operator for construction Tuples ` 1 => True -- (1, True)`:

<!-- language: lang-elm -->

    (=>) : a -> b -> ( a, b )
    (=>) a b =
        ( a, b )

   
Most of the functions in Elm are defined in prefix notation.

Apply any function using infix notation by specifying the first argument before the function name enclosed with grave accent character:

<!-- language: lang-elm -->

    import List exposing (append)
    
    
    append [1,1,2] [3,5,8]   -- [1,1,2,3,5,8]
    [1,1,2] `append` [3,5,8] -- [1,1,2,3,5,8]


