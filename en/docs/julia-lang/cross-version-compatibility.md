---
title: "Cross-Version Compatibility"
slug: "cross-version-compatibility"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
- using Compat
- Compat.String
- Compat.UTF8String
- @compat f.(x, y)

It is sometimes very difficult to get new syntax to play well with multiple versions. As Julia is still undergoing active development, it is often useful simply to drop support for older versions and instead target just the newer ones.

## Version numbers
Julia has a built-in implementation of [semantic versioning](http://semver.org/) exposed through the `VersionNumber` type.

To construct a `VersionNumber` as a literal, the `@v_str` [string macro][1] can be used:

    julia> vers = v"1.2.0"
    v"1.2.0"

Alternatively, one can call the `VersionNumber` constructor; note that the constructor accepts up to five arguments, but all except the first are optional.

    julia> vers2 = VersionNumber(1, 1)
    v"1.1.0"

Version numbers can be compared using [comparison operators][2], and thus can be sorted:

    julia> vers2 < vers
    true

    julia> v"1" < v"0"
    false

    julia> sort([v"1.0.0", v"1.0.0-dev.100", v"1.0.1"])
    3-element Array{VersionNumber,1}:
     v"1.0.0-dev.100"
     v"1.0.0"
     v"1.0.1"

Version numbers are used in several places across Julia. For instance, the `VERSION` constant is a `VersionNumber`:

    julia> VERSION
    v"0.5.0"

This is commonly used for conditional code evaluation, depending on the Julia version. For example, to run different code on v0.4 and v0.5, one can do

    if VERSION < v"0.5"
        println("v0.5 prerelease, v0.4 or older")
    else
        println("v0.5 or newer")
    end

Each installed [package][3] is also associated with a current version number:

    julia> Pkg.installed("StatsBase")
    v"0.9.0"


  [1]: https://www.wikiod.com/julia-lang/string-macros
  [2]: https://www.wikiod.com/julia-lang/comparisons
  [3]: https://www.wikiod.com/julia-lang/packages

## Using Compat.jl
The [Compat.jl package](https://github.com/JuliaLang/Compat.jl) enables using some new Julia features and syntax with older versions of Julia. Its features are documented on its README, but a summary of useful applications is given below.

<!-- if version [lt 0.5.0] -->

## Unified String type

In Julia v0.4, there were many different types of [strings][1]. This system was considered overly complex and confusing, so in Julia v0.5, there remains only the `String` type. `Compat` allows using the `String` type and constructor on version 0.4, under the name `Compat.String`. For example, this v0.5 code

    buf = IOBuffer()
    println(buf, "Hello World!")
    String(buf)  # "Hello World!\n"

can be directly translated to this code, which works on both v0.5 and v0.4:

    using Compat
    buf = IOBuffer()
    println(buf, "Hello World!")
    Compat.String(buf)  # "Hello World!\n"

Note that there are some caveats.

- On v0.4, `Compat.String` is typealiased to `ByteString`, which is `Union{ASCIIString, UTF8String}`. Thus, types with `String` fields will not be type stable. In these situations, `Compat.UTF8String` is advised, as it will mean `String` on v0.5, and `UTF8String` on v0.4, both of which are concrete types.
- One has to be careful to use `Compat.String` or `import Compat: String`, because `String` itself has a meaning on v0.4: it is a deprecated alias for `AbstractString`. A sign that `String` was accidentally used instead of `Compat.String` is if at any point, the following warnings appear:


    WARNING: Base.String is deprecated, use AbstractString instead.
      likely near no file:0
    WARNING: Base.String is deprecated, use AbstractString instead.
      likely near no file:0

## Compact broadcasting syntax

Julia v0.5 introduces syntactic sugar for `broadcast`. The syntax

    f.(x, y)

is lowered to `broadcast(f, x, y)`. Examples of using this syntax include `sin.([1, 2, 3])` to take the sine of multiple numbers at once.

On v0.5, the syntax can be used directly:

    julia> sin.([1.0, 2.0, 3.0])
    3-element Array{Float64,1}:
     0.841471
     0.909297
     0.14112 

However, if we try the same on v0.4, we get an error:

    julia> sin.([1.0, 2.0, 3.0])
    ERROR: TypeError: getfield: expected Symbol, got Array{Float64,1}

Luckily, `Compat` makes this new syntax usable from v0.4 also. Once again, we add `using Compat`. This time, we surround the expression with the `@compat` macro:

    julia> using Compat

    julia> @compat sin.([1.0, 2.0, 3.0])
    3-element Array{Float64,1}:
     0.841471
     0.909297
     0.14112 

<!-- end version if -->

  [1]: https://www.wikiod.com/julia-lang/strings

