---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Anonymous Functions
In Elixir, a common practice is to use anonymous functions. Creating an anonymous function is simple:
    
    iex(1)> my_func = fn x -> x * 2 end
    #Function<6.52032458/1 in :erl_eval.expr/5>

The general syntax is:

    fn args -> output end

For readability, you may put parenthesis around the arguments:

    iex(2)> my_func = fn (x, y) -> x*y end
    #Function<12.52032458/2 in :erl_eval.expr/5>

To invoke an anonymous function, call it by the assigned name and add `.` between the name and arguments.

    iex(3)>my_func.(7, 5)
    35
   
It is possible to declare anonymous functions without arguments:

    iex(4)> my_func2 = fn -> IO.puts "hello there" end
    iex(5)> my_func2.()
    hello there
    :ok

# Using the capture operator

To make anonymous functions more concise you can use the **capture operator** `&`. For example, instead of:

    iex(5)> my_func = fn (x) -> x*x*x end

You can write:

    iex(6)> my_func = &(&1*&1*&1)

With multiple parameters, use the number corresponding to each argument, counting from `1`:

    iex(7)> my_func = fn (x, y) -> x + y end

    iex(8)> my_func = &(&1 + &2)   # &1 stands for x and &2 stands for y

    iex(9)> my_func.(4, 5)
    9

# Multiple bodies

An anonymous function can also have multiple bodies (as a result of [pattern matching][1]):

    my_func = fn
      param1 -> do_this
      param2 -> do_that
    end

When you call a function with multiple bodies Elixir attempts to match the parameters you have provided with the proper function body.


  [1]: https://www.wikiod.com/elixir/pattern-matching

## Capture functions
Use `&` to capture functions from other modules. You can use the captured functions directly as function parameters or within anonymous functions.

    Enum.map(list, fn(x) -> String.capitalize(x) end)

Can be made more concise using `&`:

    Enum.map(list, &String.capitalize(&1))

Capturing functions without passing any arguments require you to explicitly specify its arity, e.g. `&String.capitalize/1`:

    defmodule Bob do
      def say(message, f \\ &String.capitalize/1) do
        f.(message)
      end
    end



## Keyword lists as function parameters
Use keyword lists for 'options'-style parameters that contains multiple key-value pairs:

    def myfunc(arg1, opts \\ []) do
      # Function body
    end

We can call the function above like so:

    iex> myfunc "hello", pizza: true, soda: false

which is equivalent to:

    iex> myfunc("hello", [pizza: true, soda: false])

The argument values are available as `opts.pizza` and `opts.soda` respectively.  
Alternatively, you could use atoms: `opts[:pizza]` and `opts[:soda]`.

## Default Parameters
You can pass default parameters to any named function using the syntax: `param \\ value`:

``` elixir
defmodule Example do
    def func(p1, p2 \\ 2) do
        IO.inspect [p1, p2]
    end
end

Example.func("a")    # => ["a", 2]
Example.func("b", 4) # => ["b", 4]
```

## Named Functions & Private Functions
**Named Functions**

    defmodule Math do
        # one way
        def add(a, b) do
            a + b
        end

        # another way
        def subtract(a, b), do: a - b
    end

    iex> Math.add(2, 3)
    5
    :ok
    iex> Math.subtract(5, 2)
    3
    :ok

**Private Functions**

    defmodule Math do
        def sum(a, b) do
            add(a, b)
        end

        # Private Function
        defp add(a, b) do
            a + b
        end
    end

    iex> Math.add(2, 3)
    ** (UndefinedFunctionError) undefined function Math.add/2
    Math.add(3, 4)
    iex> Math.sum(2, 3)
    5

## Pattern Matching
Elixir matches a function call to its body based on the value of its arguments.

``` elixir
defmodule Math do
    def factorial(0): do: 1
    def factorial(n): do: n * factorial(n - 1)
end
```

Here, factorial of positive numbers matches the second clause, while `factorial(0)` matches the first. (ignoring negative numbers for the sake of simplicity). Elixir tries to match the functions from top to bottom. If the second function is written above the first, we will an unexpected result as it goes to an endless recursion. Because `factorial(0)` matches to `factorial(n)`


## Guard clauses
Guard clauses enables us to check the arguments before executing the function. Guard clauses are usually preferred to `if` and `cond` due to their readability, and to make [a certain optimization technique][1] easier for the compiler. The first function definition where all guards match is executed. 

Here is an example implementation of the factorial function using guards and pattern matching. 

``` elixir
defmodule Math do
    def factorial(0), do: 1
    def factorial(n) when n > 0: do: n * factorial(n - 1)
end
```

The first pattern matches if (and only if) the argument is `0`. If the argument is not `0`, the pattern match fails and the next function below is checked. 

That second function definition has a guard clause: `when n > 0`. This means that this function only matches if the argument `n` is greater than `0`. After all, the mathematical factorial function is not defined for negative integers.

If neither function definition (including their pattern matching and guard clauses) match, a `FunctionClauseError` will be raised. This happens for this function when we pass a negative number as the argument, since it is not defined for negative numbers. 

Note that this `FunctionClauseError` itself, is not a mistake. Returning `-1` or `0` or some other "error value" as is common in some other languages would hide the fact that you called an undefined function, hiding the source of the error, possibly creating a huge painful bug for a future developer.


  [1]: http://erlang.org/doc/efficiency_guide/functions.html

