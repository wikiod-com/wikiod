---
title: "Combinators"
slug: "combinators"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Although combinators have limited practical use, they are a useful tool in education to understand how programming is fundamentally linked to logic, and how very simple building blocks can combine to create very complex behaviour. In the context of Julia, learning how to create and use combinators will strengthen an understanding of how to program in a functional style in Julia.

## The Y or Z Combinator
Although Julia is not a purely functional language, it has full support for many of the cornerstones of functional programming: first-class [functions](https://www.wikiod.com/julia-lang/functions), lexical scope, and [closures](https://www.wikiod.com/julia-lang/closures).

The [fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator) is a key combinator in functional programming. Because Julia has [eager evaluation](https://en.wikipedia.org/wiki/Eager_evaluation) semantics (as do many functional languages, including Scheme, which Julia is heavily inspired by), Curry's original Y-combinator will not work out of the box:

    Y(f) = (x -> f(x(x)))(x -> f(x(x)))

However, a close relative of the Y-combinator, the Z-combinator, will indeed work:

    Z(f) = x -> f(Z(f), x)

This combinator takes a function and returns a function that when called with argument `x`, gets passed itself and `x`. Why would it be useful for a function to be passed itself? This allows recursion without actually referencing the name of the function at all!

    fact(f, x) = x == 0 ? 1 : x * f(x)

Hence, `Z(fact)` becomes a recursive implementation of the factorial function, despite no recursion being visible in this function definition. (Recursion is evident in the definition of the `Z` combinator, of course, but that is inevitable in an eager language.) We can verify that our function indeed works:

    julia> Z(fact)(10)
    3628800

Not only that, but it is as fast as we can expect from a recursive implementation. The LLVM code demonstrates that the result is compiled into a plain old branch, subtract, call, and multiply:

    julia> @code_llvm Z(fact)(10)

    define i64 @"julia_#1_70252"(i64) #0 {
    top:
      %1 = icmp eq i64 %0, 0
      br i1 %1, label %L11, label %L8

    L8:                                               ; preds = %top
      %2 = add i64 %0, -1
      %3 = call i64 @"julia_#1_70060"(i64 %2) #0
      %4 = mul i64 %3, %0
      br label %L11

    L11:                                              ; preds = %top, %L8
      %"#temp#.0" = phi i64 [ %4, %L8 ], [ 1, %top ]
      ret i64 %"#temp#.0"
    }



## The SKI Combinator System
The [SKI combinator system](https://en.wikipedia.org/wiki/SKI_combinator_calculus) is sufficient to represent any lambda calculus terms. (In practice, of course, lambda abstractions blow up to exponential size when they are translated into SKI.) Due to the simplicity of the system, implementing the S, K, and I combinators is extraordinarily simple:

## A Direct Translation from Lambda Calculus

    const S = f -> g -> z -> f(z)(g(z))
    const K = x -> y -> x
    const I = x -> x

We can confirm, using the [unit testing](https://www.wikiod.com/julia-lang/unit-testing) system, that each combinator has the expected behaviour.

The I combinator is easiest to verify; it should return the given value unchanged:

    using Base.Test
    @test I(1) === 1
    @test I(I) === I
    @test I(S) === S

The K combinator is also fairly straightforward: it should discard its second argument.

    @test K(1)(2) === 1
    @test K(S)(I) === S

The S combinator is the most complex; its behaviour can be summarized as applying the first two arguments to the third argument, the applying the first result to the second. We can most easily test the S combinator by testing some of its curried forms. `S(K)`, for instance, should simply return its second argument and discard its first, as we see happens:

    @test S(K)(S)(K) === K
    @test S(K)(S)(I) === I

`S(I)(I)` should apply its argument to itself:

    @test S(I)(I)(I) === I
    @test S(I)(I)(K) === K(K)
    @test S(I)(I)(S(I)) === S(I)(S(I))

`S(K(S(I)))(K)` applies its second argument to its first:

    @test S(K(S(I)))(K)(I)(I) === I
    @test S(K(S(I)))(K)(K)(S(K)) === S(K)(K)

The I combinator described above has a name in standard `Base` Julia: `identity`. Thus, we could have rewritten the above definitions with the following alternative definition of `I`:

    const I = identity

## Showing SKI Combinators

One weakness with the approach above is that our functions do not show as nicely as we might like. Could we replace

    julia> S
    (::#3) (generic function with 1 method)

    julia> K
    (::#9) (generic function with 1 method)

    julia> I
    (::#13) (generic function with 1 method)

with some more informative displays? The answer is yes! Let's restart the REPL, and this time define how each function is to be shown:

    const S = f -> g -> z -> f(z)(g(z));
    const K = x -> y -> x;
    const I = x -> x;
    for f in (:S, :K, :I)
        @eval Base.show(io::IO, ::typeof($f)) = print(io, $(string(f)))
        @eval Base.show(io::IO, ::MIME"text/plain", ::typeof($f)) = show(io, $f)
    end

It's important to avoid showing anything until we have finished defining functions. Otherwise, we risk invalidating the method cache, and our new methods will not seem to immediately take effect. This is why we have put semicolons in the above definitions. The semicolons suppress the REPL's output.

This makes the functions display nicely:

    julia> S
    S

    julia> K
    K

    julia> I
    I

However, we still run into problems when we try to display a closure:

    julia> S(K)
    (::#2) (generic function with 1 method)

It would be nicer to display that as `S(K)`. To do that, we must exploit that the closures have their own individual types. We can access these types and add methods to them through reflection, using `typeof` and the `primary` field of the `name` field of the type. Restart the REPL again; we will make further changes:

    const S = f -> g -> z -> f(z)(g(z));
    const K = x -> y -> x;
    const I = x -> x;
    for f in (:S, :K, :I)
        @eval Base.show(io::IO, ::typeof($f)) = print(io, $(string(f)))
        @eval Base.show(io::IO, ::MIME"text/plain", ::typeof($f)) = show(io, $f)
    end
    Base.show(io::IO, s::typeof(S(I)).name.primary) = print(io, "S(", s.f, ')')
    Base.show(io::IO, s::typeof(S(I)(I)).name.primary) =
        print(io, "S(", s.f, ')', '(', s.g, ')')
    Base.show(io::IO, k::typeof(K(I)).name.primary) = print(io, "K(", k.x, ')')
    Base.show(io::IO, ::MIME"text/plain", f::Union{
        typeof(S(I)).name.primary,
        typeof(S(I)(I)).name.primary,
        typeof(K(I)).name.primary
    }) = show(io, f)

And now, at last, things display as we would like them to:

    julia> S(K)
    S(K)

    julia> S(K)(I)
    S(K)(I)

    julia> K
    K

    julia> K(I)
    K(I)

    julia> K(I)(K)
    I


