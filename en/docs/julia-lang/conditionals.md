---
title: "Conditionals"
slug: "conditionals"
draft: false
images: []
weight: 9922
type: docs
toc: true
---

## Syntax
- if cond; body; end
- if cond; body; else; body; end
- if cond; body; elseif cond; body; else; end
- if cond; body; elseif cond; body; end
- cond ? iftrue : iffalse
- cond && iftrue
- cond || iffalse
- ifelse(cond, iftrue, iffalse)

All conditional operators and functions involve using boolean conditions (`true` or `false`). In Julia, the type of booleans is `Bool`. Unlike some other languages, other kinds of numbers (like `1` or `0`), strings, arrays, and so forth _cannot_ be used directly in conditionals.

Typically, one uses either predicate functions (functions that return a `Bool`) or [comparison operators][1] in the condition of a conditional operator or function.


  [1]: https://www.wikiod.com/julia-lang/comparisons

## Short-circuit operators: && and ||
## For branching

The short-circuiting conditional operators `&&` and `||` can be used as lightweight replacements for the following constructs:

 - `x && y` is equivalent to `x ? y : x`
 - `x || y` is equivalent to `x ? x : y`

One use for short-circuit operators is as a more concise way to test a condition and perform a certain action depending on that condition. For instance, the following code uses the `&&` operator to throw an error if the argument `x` is negative:

    function mysqrt(x)
        x < 0 && throw(DomainError("x is negative"))
        x ^ 0.5
    end

The `||` operator can also be used for error checking, except that it triggers the error _unless_ a condition holds, instead of _if_ the condition holds:

    function halve(x::Integer)
        iseven(x) || throw(DomainError("cannot halve an odd number"))
        x ÷ 2
    end

Another useful application of this is to supply a default value to an object, only if it is not previously defined:

    isdefined(:x) || (x = NEW_VALUE)

Here, this checks if the symbol x is defined (i.e. if there is an value assigned to the object `x`).  If so, then nothing happens.  But, if not, then `x` will be assigned `NEW_VALUE`. Note that this example will only work at toplevel scope.

## In conditions

The operators are also useful because they can be used to test two conditions, the second of which is only evaluated depending on the result of the first condition.  From the Julia [documentation](http://docs.julialang.org/en/release-0.4/manual/control-flow/#man-short-circuit-evaluation):

> In the expression `a && b`, the subexpression `b` is only evaluated if `a` evaluates to `true`
>
> In the expression `a || b`, the subexpression `b` is only evaluated if `a` evaluates to `false`

Thus, while both `a & b` and `a && b` will yield `true` if both `a` and `b` are `true`, their behavior if `a` is `false` is different.

  For instance, suppose we wish to check if an object is a positive number, where it is possible that it might not even be a number.  Consider the differences between these two attempted implementations:

    CheckPositive1(x) = (typeof(x)<:Number) & (x > 0) ? true : false
    CheckPositive2(x) = (typeof(x)<:Number) && (x > 0) ? true : false

    CheckPositive1("a")
    CheckPositive2("a")

`CheckPositive1()` will yield an error if a non-numeric type is supplied to it as an argument.  This is because it evaluates *both* expressions, regardless of the result of the first, and the second expression will yield an error when one tries to evaluate it for a non-numeric type.

`CheckPositive2()`, however, will yield `false` (rather than an error) if a non-numeric type is supplied to it, since the second expression is only evaluated if the first is `true`.

More than one short-circuit operator can be strung together.  E.g.:

    1 > 0 && 2 > 0 && 3 > 5

## The ifelse function
    shift(x) = ifelse(x > 10, x + 1, x - 1)

Usage:

    julia> shift(10)
    9

    julia> shift(11)
    12

    julia> shift(-1)
    -2

The `ifelse` function will evaluate both branches, even the one that is not selected. This can be useful either when the branches have side effects that must be evaluated, or because it can be faster if both branches themselves are cheap.

## if...else expression
The most common conditional in Julia is the `if`...`else` expression. For instance, below we implement the [Euclidean algorithm](https://en.wikipedia.org/wiki/Euclidean_algorithm) for computing the [greatest common divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor), using a conditional to handle the base case:

    mygcd(a, b) = if a == 0
        abs(b)
    else
        mygcd(b % a, a)
    end

The `if`...`else` form in Julia is actually an expression, and has a value; the value is the expression in tail position (that is, the last expression) on the branch that is taken. Consider the following sample input:

    julia> mygcd(0, -10)
    10

Here, `a` is `0` and `b` is `-10`. The condition `a == 0` is `true`, so the first branch is taken. The returned value is `abs(b)` which is `10`.

    julia> mygcd(2, 3)
    1

Here, `a` is `2` and `b` is `3`. The condition `a == 0` is false, so the second branch is taken, and we compute `mygcd(b % a, a)`, which is `mygcd(3 % 2, 2)`. The `%` operator returns the remainder when `3` is divided by `2`, in this case `1`. Thus we compute `mygcd(1, 2)`, and this time `a` is `1` and `b` is `2`. Once again, `a == 0` is false, so the second branch is taken, and we compute `mygcd(b % a, a)`, which is `mygcd(0, 1)`. This time, `a == 0` at last and so `abs(b)` is returned, which gives the result `1`.

## if...else statement
    name = readline()
    if startswith(name, "A")
        println("Your name begins with A.")
    else
        println("Your name does not begin with A.")
    end

Any expression, such as the `if`...`else` expression, can be put in statement position. This ignores its value but still executes the expression for its side effects.

## if statement
Like any other expression, the return value of an `if`...`else` expression can be ignored (and hence discarded). This is generally only useful when the body of the expression has side effects, such as writing to a file, mutating variables, or printing to the screen.

Furthermore, the `else` branch of an `if`...`else` expression is optional. For instance, we can write the following code to output to screen only if a particular condition is met:

    second = Dates.second(now())
    if iseven(second)
        println("The current second, $second, is even.")
    end

In the example above, we use [time and date][1] functions to get the current second; for instance, if it is currently 10:55:27, the variable `second` will hold `27`. If this number is even, then a line will be printed to screen. Otherwise, nothing will be done.


  [1]: https://www.wikiod.com/julia-lang/time

## Ternary conditional operator
    pushunique!(A, x) = x in A ? A : push!(A, x)

The ternary conditional operator is a less wordy `if`...`else` expression.

The syntax specifically is:

    [condition] ? [execute if true] : [execute if false]

In this example, we add `x` to the collection `A` only if `x` is not already in `A`.  Otherwise, we just leave `A` unchanged.

Ternary operator References:

 - [Julia Documentation](http://docs.julialang.org/en/release-0.4/manual/control-flow/#man-conditional-evaluation)
 - [Wikibooks](https://en.wikibooks.org/wiki/Introducing_Julia/Controlling_the_flow#Ternary_expressions)

## if statement with multiple branches
    d = Dates.dayofweek(now())
    if d == 7
        println("It is Sunday!")
    elseif d == 6
        println("It is Saturday!")
    elseif d == 5
        println("Almost the weekend!")
    else
        println("Not the weekend yet...")
    end

Any number of `elseif` branches may be used with an `if` statement, possibly with or without a final `else` branch.  Subsequent conditions will only be evaluated if all prior conditions have been found to be `false`.

