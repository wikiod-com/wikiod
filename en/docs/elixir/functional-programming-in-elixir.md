---
title: "Functional programming in Elixir"
slug: "functional-programming-in-elixir"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Let's try to implement the basic higher orders functions like map and reduce using Elixir

## Map
Map is a function which will take an array and a function and return an array after applying that function to ***each element*** in that list

    defmodule MyList do
      def map([], _func) do
        []
      end

      def map([head | tail], func) do
        [func.(head) | map(tail, func)]
      end
    end

Copy paste in `iex` and execute: 

`MyList.map [1,2,3], fn a -> a * 5 end`

Shorthand syntax is `MyList.map [1,2,3], &(&1 * 5)`

## Reduce
Reduce is a function which will take an array, function and accumulator and use ***accumulator as seed to start the iteration with the first element to give next accumulator and the iteration continues for all the elements in the array*** (refer below example)

    defmodule MyList do
      def reduce([], _func, acc) do
        acc
      end
    
      def reduce([head | tail], func, acc) do
        reduce(tail, func, func.(acc, head))
      end
    end

Copy paste the above snippet in iex:

 

 1. To add all numbers in an array: `MyList.reduce [1,2,3,4], fn acc, element -> acc + element end, 0`
 2. To mutliply all numbers in an array: `MyList.reduce [1,2,3,4], fn acc, element -> acc * element end, 1`

**Explanation for example 1:**

    Iteration 1 => acc = 0, element = 1 ==> 0 + 1 ===> 1 = next accumulator
    Iteration 2 => acc = 1, element = 2 ==> 1 + 2 ===> 3 = next accumulator
    Iteration 3 => acc = 3, element = 3 ==> 3 + 3 ===> 6 = next accumulator
    Iteration 4 => acc = 6, element = 4 ==> 6 + 4 ===> 10 = next accumulator = result(as all elements are done)

**Filter the list using reduce**
      
    MyList.reduce [1,2,3,4], fn acc, element -> if rem(element,2) == 0 do acc else acc ++ [element] end end, []


