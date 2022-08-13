---
title: "Doctests"
slug: "doctests"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Introduction
When you document your code with `@doc`, you can supply code examples like so:

    # myproject/lib/my_module.exs
    
    defmodule MyModule do
      @doc """
      Given a number, returns `true` if the number is even, otherwise `false`.
    
      ## Example
        iex> MyModule.even?(2)
        true
        iex> MyModule.even?(3)
        false
      """
      def even?(number) do
        rem(number, 2) == 0
      end
    end

You can add the code examples as test cases into one of your test suites:

    # myproject/test/doc_test.exs
    
    defmodule DocTest do
      use ExUnit.Case
      doctest MyModule
    end

Then, you can then run your tests with `mix test`.



## Generating HTML documentation based on doctest
Because generating documentation is based on markdown, you have to do 2 things : 

1/ Write your doctest and make your doctest examples clear to improve readability (It is better to give a headline, like "examples" or "tests").  When you write your tests, do not forget to give 4 spaces to your tests code so that it will be formatting as code in the HTML documentation.   

2/ Then, enter "mix docs" in console at the root of your elixir project to generate the HTML documentation in the doc directory located in the root of your elixir project.  
`$> mix docs`   



## Multiline doctests
You can do a multiline doctest by using '...>' for the lines following the first

    iex> Foo.Bar.somethingConditional("baz")
    ...>   |> case do
    ...>       {:ok, _} -> true
    ...>       {:error, _} -> false
    ...>      end
    true

