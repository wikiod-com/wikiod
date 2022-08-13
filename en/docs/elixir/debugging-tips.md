---
title: "Debugging Tips"
slug: "debugging-tips"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Debug in pipe
    defmodule Demo do
      def foo do
        1..10
        |> Enum.map(&(&1 * &1))          |> p
        |> Enum.filter(&rem(&1, 2) == 0) |> p
        |> Enum.take(3)                  |> p
      end
    
      defp p(e) do
        require Logger
        Logger.debug inspect e, limit: :infinity
        e
      end
    end

----------

    iex(1)> Demo.foo
    
    23:23:55.171 [debug] [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
    
    23:23:55.171 [debug] [4, 16, 36, 64, 100]
    
    23:23:55.171 [debug] [4, 16, 36]
    
    [4, 16, 36]



## Pry in pipe
    defmodule Demo do
      def foo do
        1..10
        |> Enum.map(&(&1 * &1))
        |> Enum.filter(&rem(&1, 2) == 0) |> pry
        |> Enum.take(3)
      end
    
      defp pry(e) do
        require IEx
        IEx.pry
        e
      end
    end

----------

    iex(1)> Demo.foo
    Request to pry #PID<0.117.0> at lib/demo.ex:11
    
          def pry(e) do
            require IEx
            IEx.pry
            e
          end
    
    Allow? [Yn] Y
    
    Interactive Elixir (1.3.2) - press Ctrl+C to exit (type h() ENTER for help)
    pry(1)> e
    [4, 16, 36, 64, 100]
    pry(2)> respawn
    
    Interactive Elixir (1.3.2) - press Ctrl+C to exit (type h() ENTER for help)
    [4, 16, 36]
    iex(1)>

## Debugging with IEX.pry/0
Debugging with `IEx.pry/0` is quite simple. 
1) `require IEx` in your module
2) Find the line of code you want to inspect
3) Add `IEx.pry` after the line

Now start your project (e.g. `iex -S mix`).

When the line with `IEx.pry/0` is reached the program will stop and you have the chance to inspect. It is like a breakpoint in a traditional debugger. 

When you are finished just type `respawn` into the console.

    require IEx;
    
    defmodule Example do
      def double_sum(x, y) do
        IEx.pry
        hard_work(x, y)
      end
    
      defp hard_work(x, y) do
        2 * (x + y)
      end
    end

## Debugging with IO.inspect/1
It is possible to use IO.inspect/1 as a tool to debug an elixir program.

    defmodule MyModule do
      def myfunction(argument_1, argument_2) do
        IO.inspect(argument_1)
        IO.inspect(argument_2)
      end
    end

It will print out argument_1 and argument_2 to the console. Since `IO.inspect/1` returns its argument it is very easy to include it in function calls or pipelines without breaking the flow:

    do_something(a, b)
    |> do_something_else(c)

    # can be adorned with IO.inspect, with no change in functionality:

    do_something(IO.inspect(a), IO.inspect(b))
    |> IO.inspect
    do_something(IO.inspect(c))

