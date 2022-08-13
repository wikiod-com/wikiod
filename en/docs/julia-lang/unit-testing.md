---
title: "Unit Testing"
slug: "unit-testing"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Syntax
- @test [expr]
- @test_throws [Exception] [expr]
- @testset "[name]" begin; [tests]; end
- Pkg.test([package])

The standard library documentation for [`Base.Test`](http://docs.julialang.org/en/latest/stdlib/test/) covers additional material beyond that shown in these examples.

## Testing a Package
To run the unit tests for a package, use the `Pkg.test` function. For a package named `MyPackage`, the command would be

    julia> Pkg.test("MyPackage")

An expected output would be similar to

    INFO: Computing test dependencies for MyPackage...
    INFO: Installing BaseTestNext v0.2.2
    INFO: Testing MyPackage
    Test Summary: | Pass  Total
      Data        |   66     66
    Test Summary: | Pass  Total
      Monetary    |  107    107
    Test Summary: | Pass  Total
      Basket      |   47     47
    Test Summary: | Pass  Total
      Mixed       |   13     13
    Test Summary: | Pass  Total
      Data Access |   35     35
    INFO: MyPackage tests passed
    INFO: Removing BaseTestNext v0.2.2

though obviously, one cannot expect it to match the above exactly, since different packages use different frameworks.

This command runs the package's `test/runtests.jl` file in a clean environment.

One can test all installed packages at once with

    julia> Pkg.test()

but this usually takes a very long time.

## Writing a Test Set
<!-- if version [gte 0.5.0] -->
In version v0.5, test sets are built into the standard library `Base.Test` module, and you don't have to do anything special (besides `using Base.Test`) to use them.
<!-- end version if -->

<!-- if version [eq 0.4.0] -->
Test sets are not part of Julia v0.4's `Base.Test` library. Instead, you have to `REQUIRE` the `BaseTestNext` module, and add `using BaseTestNext` to your file. To support both version 0.4 and 0.5, you could use

    if VERSION ≥ v"0.5.0-dev+7720"
        using Base.Test
    else
        using BaseTestNext
        const Test = BaseTestNext
    end
<!-- end version if -->

It is helpful to group related `@test`s together in a test set. In addition to clearer test organization, test sets offer better output and more customizability.

To define a test set, simply wrap any number of `@test`s with a `@testset` block:

    @testset "+" begin
        @test 1 + 1 == 2
        @test 2 + 2 == 4
    end

    @testset "*" begin
        @test 1 * 1 == 1
        @test 2 * 2 == 4
    end

Running these test sets prints the following output:

    Test Summary: | Pass  Total
      +           |    2      2
    
    Test Summary: | Pass  Total
      *           |    2      2

Even if a test set contains a failing test, the entire test set will be run to completion, and the failures will be recorded and reported:

    @testset "-" begin
        @test 1 - 1 == 0
        @test 2 - 2 == 1
        @test 3 - () == 3
        @test 4 - 4 == 0
    end

Running this test set results in


    -: Test Failed
      Expression: 2 - 2 == 1
       Evaluated: 0 == 1
     in record(::Base.Test.DefaultTestSet, ::Base.Test.Fail) at ./test.jl:428
        ...
    -: Error During Test
      Test threw an exception of type MethodError
      Expression: 3 - () == 3
      MethodError: no method matching -(::Int64, ::Tuple{})
        ...
    Test Summary: | Pass  Fail  Error  Total
      -           |    2     1      1      4
    ERROR: Some tests did not pass: 2 passed, 1 failed, 1 errored, 0 broken.
        ...

Test sets can be nested, allowing for arbitrarily deep organization

    @testset "Int" begin
        @testset "+" begin
            @test 1 + 1 == 2
            @test 2 + 2 == 4
        end
        @testset "-" begin
            @test 1 - 1 == 0
        end
    end

If the tests pass, then this will only show the results for the outermost test set:

    Test Summary: | Pass  Total
      Int         |    3      3

But if the tests fail, then a drill-down into the exact test set and test causing the failure is reported.

The `@testset` macro can be used with a [`for` loop](https://www.wikiod.com/julia-lang/for-loops) to create many test sets at once:

    @testset for i in 1:5
        @test 2i == i + i
        @test i^2 == i * i
        @test i ÷ i == 1
    end

which reports

    Test Summary: | Pass  Total
      i = 1       |    3      3
    Test Summary: | Pass  Total
      i = 2       |    3      3
    Test Summary: | Pass  Total
      i = 3       |    3      3
    Test Summary: | Pass  Total
      i = 4       |    3      3
    Test Summary: | Pass  Total
      i = 5       |    3      3

A common structure is to have outer test sets test components or types. Within these outer test sets, inner test sets test behaviour. For instance, suppose we created a type `UniversalSet` with a singleton instance that contains everything. Before we even implement the type, we can use [test-driven development](https://en.wikipedia.org/wiki/Test-driven_development) principles and implement the tests:

    @testset "UniversalSet" begin
        U = UniversalSet.instance
        @testset "egal/equal" begin
            @test U === U
            @test U == U
        end
    
        @testset "in" begin
            @test 1 in U
            @test "Hello World" in U
            @test Int in U
            @test U in U
        end
    
        @testset "subset" begin
            @test Set() ⊆ U
            @test Set(["Hello World"]) ⊆ U
            @test Set(1:10) ⊆ U
            @test Set([:a, 2.0, "w", Set()]) ⊆ U
            @test U ⊆ U
        end
    end

We can then start implementing our functionality until it passes our tests. The first step is to define the type:

    immutable UniversalSet <: Base.AbstractSet end

Only two of our tests pass right now. We can implement `in`:

    immutable UniversalSet <: Base.AbstractSet end
    Base.in(x, ::UniversalSet) = true

This also makes some of our subset tests to pass. However, the `issubset` (`⊆`) fallback doesn't work for `UniversalSet`, because the fallback tries to iterate over elements, which we can't do. We can simply define a specialization that makes `issubset` return `true` for any set:

    immutable UniversalSet <: Base.AbstractSet end
    Base.in(x, ::UniversalSet) = true
    Base.issubset(x::Base.AbstractSet, ::UniversalSet) = true

And now, all our tests pass!

## Writing a Simple Test
Unit tests are declared in the `test/runtests.jl` file in a package. Typically, this file begins

    using MyModule
    using Base.Test

The basic unit of testing is the `@test` macro. This macro is like an assertion of sorts. Any boolean expression can be tested in the `@test` macro:

    @test 1 + 1 == 2
    @test iseven(10)
    @test 9 < 10 || 10 < 9

We can try out the `@test` macro in the REPL:

    julia> using Base.Test

    julia> @test 1 + 1 == 2
    Test Passed
      Expression: 1 + 1 == 2
       Evaluated: 2 == 2

    julia> @test 1 + 1 == 3
    Test Failed
      Expression: 1 + 1 == 3
       Evaluated: 2 == 3
    ERROR: There was an error during testing
     in record(::Base.Test.FallbackTestSet, ::Base.Test.Fail) at ./test.jl:397
     in do_test(::Base.Test.Returned, ::Expr) at ./test.jl:281

The test macro can be used in just about anywhere, such as in loops or functions:

    # For positive integers, a number's square is at least as large as the number
    for i in 1:10
        @test i^2 ≥ i
    end

    # Test that no two of a, b, or c share a prime factor
    function check_pairwise_coprime(a, b, c)
        @test gcd(a, b) == 1
        @test gcd(a, c) == 1
        @test gcd(b, c) == 1
    end

    check_pairwise_coprime(10, 23, 119)

## Testing Exceptions
Exceptions encountered while running a test will fail the test, and if the test is not in a test set, terminate the test engine. Usually, this is a good thing, because in most situations exceptions are not the desired result. But sometimes, one wants to test specifically that a certain exception is raised. The `@test_throws` macro facilitates this.

    julia> @test_throws BoundsError [1, 2, 3][4]
    Test Passed
      Expression: ([1,2,3])[4]
          Thrown: BoundsError

If the wrong exception is thrown, `@test_throws` will still fail:

    julia> @test_throws TypeError [1, 2, 3][4]
    Test Failed
      Expression: ([1,2,3])[4]
        Expected: TypeError
          Thrown: BoundsError
    ERROR: There was an error during testing
     in record(::Base.Test.FallbackTestSet, ::Base.Test.Fail) at ./test.jl:397
     in do_test_throws(::Base.Test.Threw, ::Expr, ::Type{T}) at ./test.jl:329

and if no exception is thrown, `@test_throws` will fail also:

    julia> @test_throws BoundsError [1, 2, 3, 4][4]
    Test Failed
      Expression: ([1,2,3,4])[4]
        Expected: BoundsError
      No exception thrown
    ERROR: There was an error during testing
     in record(::Base.Test.FallbackTestSet, ::Base.Test.Fail) at ./test.jl:397
     in do_test_throws(::Base.Test.Returned, ::Expr, ::Type{T}) at ./test.jl:329



## Testing Floating Point Approximate Equality
What's the deal with the following?

    julia> @test 0.1 + 0.2 == 0.3
    Test Failed
      Expression: 0.1 + 0.2 == 0.3
       Evaluated: 0.30000000000000004 == 0.3
    ERROR: There was an error during testing
     in record(::Base.Test.FallbackTestSet, ::Base.Test.Fail) at ./test.jl:397
     in do_test(::Base.Test.Returned, ::Expr) at ./test.jl:281

The error is caused by the fact that none of `0.1`, `0.2`, and `0.3` are represented in the computer as exactly those values — `1//10`, `2//10`, and `3//10`. Instead, they are approximated by values that are very close. But as seen in the test failure above, when adding two approximations together, the result can be a slightly worse approximation than is possible. There is [much more to this subject](http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html) that cannot be covered here.

But we aren't out of luck! To test that the combination of rounding to a floating point number and floating point arithmetic is _approximately_ correct, even if not exact, we can use the `isapprox` function (which corresponds to operator `≈`). So we can rewrite our test as

    julia> @test 0.1 + 0.2 ≈ 0.3
    Test Passed
      Expression: 0.1 + 0.2 ≈ 0.3
       Evaluated: 0.30000000000000004 isapprox 0.3

Of course, if our code was entirely wrong, the test will still catch that:

    julia> @test 0.1 + 0.2 ≈ 0.4
    Test Failed
      Expression: 0.1 + 0.2 ≈ 0.4
       Evaluated: 0.30000000000000004 isapprox 0.4
    ERROR: There was an error during testing
     in record(::Base.Test.FallbackTestSet, ::Base.Test.Fail) at ./test.jl:397
     in do_test(::Base.Test.Returned, ::Expr) at ./test.jl:281

The `isapprox` function uses heuristics based off the size of the numbers and the precision of the floating point type to determine the amount of error to be tolerated. It's not appropriate for all situations, but it works in most, and saves a lot of effort implementing one's own version of `isapprox`.

