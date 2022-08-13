---
title: "Metaprogramming"
slug: "metaprogramming"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Generate tests at compile time
    defmodule ATest do
      use ExUnit.Case

      [{1, 2, 3}, {10, 20, 40}, {100, 200, 300}]
      |> Enum.each(fn {a, b, c} ->
        test "#{a} + #{b} = #{c}" do
          assert unquote(a) + unquote(b) = unquote(c)
        end
      end)
    end

Output:

    .

      1) test 10 + 20 = 40 (Test.Test)
         test.exs:6
         match (=) failed
         code: 10 + 20 = 40
         rhs:  40
         stacktrace:
           test.exs:7

    .

    Finished in 0.1 seconds (0.1s on load, 0.00s on tests)
    3 tests, 1 failure



