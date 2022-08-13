---
title: "Operators"
slug: "operators"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## The Pipe Operator
The Pipe Operator `|>` takes the result of an expression on the left and feeds it as the first parameter to a function on the right.

    expression |> function

Use the Pipe Operator to chain expressions together and to visually document the flow of a series of functions.

Consider the following:

    Oven.bake(Ingredients.Mix([:flour, :cocoa, :sugar, :milk, :eggs, :butter]), :temperature)

In the example, `Oven.bake` comes before `Ingredients.mix`, but it is executed last. Also, it may not be obvious that `:temperature` is a parameter of `Oven.bake`

Rewriting this example using the Pipe Operator:

    [:flour, :cocoa, :sugar, :milk, :eggs, :butter]
    |> Ingredients.mix
    |> Oven.bake(:temperature)

gives the same result, but the order of execution is clearer. Furthermore, it is clear that `:temperature` is a parameter to the `Oven.bake` call.

Note that when using the Pipe Operator, the first parameter for each function is relocated to before the Pipe Operator, and so the function being called appears to have one fewer parameter. For instance:

    Enum.each([1, 2, 3], &(&1+1)) # produces [2, 3, 4]

is the same as:
    
    [1, 2, 3]
    |> Enum.each(&(&1+1))

## Boolean operators
There are two kinds of boolean operators in Elixir:

 - boolean operators (they expect either `true` or `false` as their first argument)


    x or y      # true if x is true, otherwise y
    
    x and y     # false if x is false, otherwise y

    not x       # false if x is true, otherwise true

All of booleans operators will raise `ArgumentError` if first argument won't be strictly boolean value, which means only  `true` or `false` (`nil` is not boolean).

    iex(1)> false and 1 # return false
    iex(2)> false or 1 # return 1 
    iex(3)> nil and 1 # raise (ArgumentError) argument error: nil



 - relaxed boolean operators (work with any type, everything that neither `false` nor `nil` is considered as `true`)


    x || y     # x if x is true, otherwise y

    x && y     # y if x is true, otherwise false

    !x         # false if x is true, otherwise true

Operator `||` will always return first argument if it's truthy (Elixir treats everything except `nil` and `false` to be true in comparisions), otherwise will return second one. 

    iex(1)> 1 || 3 # return 1, because 1 is truthy
    iex(2)> false || 3 # return 3
    iex(3)> 3 || false # return 3
    iex(4)> false || nil # return nil
    iex(5)> nil || false # return false

Operator `&&` will always return second argument if it's truthy. Otherwise will return respectively to the arguments, `false` or `nil`.

    iex(1)> 1 && 3 # return 3, first argument is truthy
    iex(2)> false && 3 # return false
    iex(3)> 3 && false # return false 
    iex(4)> 3 && nil # return nil
    iex(5)> false && nil # return false
    iex(6)> nil && false # return nil

Both `&&` and `||` are  short-circuit operators. They only execute the right side if the left side is not enough to determine the result.

Operator `!` will return boolean value of negation of current term:

    iex(1)> !2 # return false
    iex(2)> !false # return true
    iex(3)> !"Test" # return false
    iex(4)> !nil # return true

Simple way to get boolean value of selected term is to simply double this operator:

    iex(1)> !!true # return true
    iex(2)> !!"Test" # return true
    iex(3)> !!nil # return false
    iex(4)> !!false # return false


## Pipe operator and parentheses
Parentheses are needed to avoid ambiguity:

    foo 1 |> bar 2 |> baz 3

Should be written as:

    foo(1) |> bar(2) |> baz(3)

## Comparison operators
Equality:
 - value equality `x == y` (`1 == 1.0 # true`)
 - value inequality `x == y` (`1 != 1.0 # false`)
 - strict equality `x === y` (`1 === 1.0 # false`)
 - strict inequality `x === y` (`1 !== 1.0 # true`)

Comparison:
 - `x > y`
 - `x >= y`
 - `x < y`
 - `x <= y`

If types are compatible, comparison uses natural ordering. Otherwise there is general types comparison rule:

`number < atom < reference < function < port < pid < tuple < map < list < binary`

## Join operators
You can join (concatenate) binaries (including strings) and lists:

    iex(1)> [1, 2, 3] ++ [4, 5]
    [1, 2, 3, 4, 5]

    iex(2)> [1, 2, 3, 4, 5] -- [1, 3]
    [2, 4, 5]

    iex(3)> "qwe" <> "rty"
    "qwerty"

## 'In' operator
`in` operator allows you to check whether a list or a range includes an item:

    iex(4)> 1 in [1, 2, 3, 4]
    true

    iex(5)> 0 in (1..5)
    false

