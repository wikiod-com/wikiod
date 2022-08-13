---
title : Recursion Tutorial
slug : recursion-tutorial
weight : 9986
draft : false
images : []
type : docs
---

Recursion refers to something being defined in terms of itself. In the context of programming it is either the practice of using functions that call themselves or recursive types. 

# Recursive Functions

There are two parts to a recursive function:

 - One or more base cases 
 - A recursive step

Because a recursive function calls itself the string of function calls could go on forever. In order to make sure the function terminates base cases must be included. These will tell the function when to return a value instead of performing another recursive call. Typically, recursive functions will check their inputs against the base case and return if the base case has been reached. If the base case has not been reached then the function will proceed to the recursive step.

The recursive step is when a function will call itself. Many recursive functions will return some calculation performed on the result of the recursive call. The Fibonacci sequence example demonstrates this. That function recursively calls itself twice and then returns the sum of the results of those calls

One common use for recursion is navigating and performing calculations on large data structures such as trees and graphs. This works well because recursion breaks the problem down to solve smaller problems and builds upon those solutions to solve the larger problem. Another common use for recursion is performing iterative operations (looping) in languages (typically functional languages) that have no built in looping structures.

# Recursive Types

A recursive type is one whose values are composed from values of the same type, or phrased differently: a recursive type is defined in terms of itself. For an example, a list is a recursive type as every subset of the list is itself a list. Trees are another example, as nodes are removed from a tree the remaining construct is still a tree.

Formally, the set of values of a recursive type, _T_, will be dfefined by a recursive set equation on the form:

_T = ... T ..._

A recursive set equation can have many solutions, but such an equation always has a least solution that is a subset of every other solution.

As a practical example consider this Haskell definition for a list type,

``` haskell
data List a = Nil | Cons a (List a)
``` 

meaning that a list of `a`'s is either an empty list or a cons cell containing an '`a`' (the "head" of the list) and another list (the "tail").

