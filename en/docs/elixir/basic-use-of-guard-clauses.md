---
title: "basic use of guard clauses"
slug: "basic-use-of-guard-clauses"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## basic uses of guard clauses
In Elixir, one can create multiple implementations of a function with the same name, and specify rules which will be applied to the parameters of the function _before calling the function_ in order to determine which implementation to run.

These rules are marked by the keyword `when`, and they go between the `def function_name(params)` and the `do` in the function definition. A trivial example:

```source:elixir
defmodule Math do

  def is_even(num) when num === 1 do
    false
  end
  def is_even(num) when num === 2 do
    true
  end

  def is_odd(num) when num === 1 do
    true
  end
  def is_odd(num) when num === 2 do
    false
  end

end
```

Say I run `Math.is_even(2)` with this example. There are two implementations of `is_even`, with differing guard clauses. The system will look at them in order, and run the first implementation where the parameters satisfy the guard clause. The first one specifies that `num === 1` which is not true, so it moves on to the next one. The second one specifies that `num === 2`, which is true, so this is the implementation that is used, and the return value will be `true`.

What if I run `Math.is_odd(1)`? The system looks at the first implementation, and sees that since `num` is `1` the guard clause of the first implementation is satisfied. It will then use that implementation and return `true`, and not bother looking at any other implementations.

Guards are limited in the types of operations they can run. [The Elixir documentation lists every allowed operation](http://elixir-lang.org/getting-started/case-cond-and-if.html#expressions-in-guard-clauses); in a nutshell they allow comparisons, math, binary operations, type-checking (e.g. `is_atom`), and a handful of small convenience functions (e.g. `length`). It is possible to define custom guard clauses, but it requires creating macros and is best left for a more advanced guide.

---

Note that guards do not throw errors; they are treated as normal failures of the guard clause, and the system moves on to look at the next implementation. If you find that you're getting `(FunctionClauseError) no function clause matching` when calling a guarded function with params you expect to work, it may be that a guard clause which you expect to work is throwing an error which is being swallowed up.

To see this for yourself, create and then call a function with a guard which makes no sense, such as this which tries to divide by zero:

```
defmodule BadMath do
  def divide(a) when a / 0 === :foo do
    :bar
  end
end
```

Calling `BadMath.divide("anything")` will provide the somewhat-unhelpful error `(FunctionClauseError) no function clause matching in BadMath.divide/1` — whereas if you had tried to run `"anything" / 0` directly, you would get a more helpful error: `(ArithmeticError) bad argument in arithmetic expression`.












