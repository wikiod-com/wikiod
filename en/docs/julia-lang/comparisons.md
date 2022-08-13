---
title: "Comparisons"
slug: "comparisons"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
- x < y   # if `x` is strictly less than `y`
- x > y   # if `x` is strictly greater than `y`
- x == y  # if `x` is equal to `y`
- x === y # alternatively `x ≡ y`, if `x` is egal to `y`
- x ≤ y   # alternatively `x <= y`, if `x` is less than or equal to `y`
- x ≥ y   # alternatively `x >= y`, if `x` is greater than or equal to `y`
- x ≠ y   # alternatively `x != y`, if `x` is not equal to `y`
- x ≈ y   # if `x` is approximately equal to `y`

Be careful about flipping comparison signs around. Julia defines many comparison functions by default without defining the corresponding flipped version. For instance, one can run

    julia> Set(1:3) ⊆ Set(0:5)
    true

but it does not work to do

    julia> Set(0:5) ⊇ Set(1:3)
    ERROR: UndefVarError: ⊇ not defined



## Standard Operators
Julia supports a very large set of comparison operators. These include

 1. All of the following unicode sequences: `> < >= ≥ <= ≤ == === ≡ != ≠ !== ≢ ∈ ∉ ∋ ∌ ⊆ ⊈ ⊂ ⊄ ⊊ ∝ ∊ ∍ ∥ ∦ ∷ ∺ ∻ ∽ ∾ ≁ ≃ ≄ ≅ ≆ ≇ ≈ ≉ ≊ ≋ ≌ ≍ ≎ ≐ ≑ ≒ ≓ ≔ ≕ ≖ ≗ ≘ ≙ ≚ ≛ ≜ ≝ ≞ ≟ ≣ ≦ ≧ ≨ ≩ ≪ ≫ ≬ ≭ ≮ ≯ ≰ ≱ ≲ ≳ ≴ ≵ ≶ ≷ ≸ ≹ ≺ ≻ ≼ ≽ ≾ ≿ ⊀ ⊁ ⊃ ⊅ ⊇ ⊉ ⊋ ⊏ ⊐ ⊑ ⊒ ⊜ ⊩ ⊬ ⊮ ⊰ ⊱ ⊲ ⊳ ⊴ ⊵ ⊶ ⊷ ⋍ ⋐ ⋑ ⋕ ⋖ ⋗ ⋘ ⋙ ⋚ ⋛ ⋜ ⋝ ⋞ ⋟ ⋠ ⋡ ⋢ ⋣ ⋤ ⋥ ⋦ ⋧ ⋨ ⋩ ⋪ ⋫ ⋬ ⋭ ⋲ ⋳ ⋴ ⋵ ⋶ ⋷ ⋸ ⋹ ⋺ ⋻ ⋼ ⋽ ⋾ ⋿ ⟈ ⟉ ⟒ ⦷ ⧀ ⧁ ⧡ ⧣ ⧤ ⧥ ⩦ ⩧ ⩪ ⩫ ⩬ ⩭ ⩮ ⩯ ⩰ ⩱ ⩲ ⩳ ⩴ ⩵ ⩶ ⩷ ⩸ ⩹ ⩺ ⩻ ⩼ ⩽ ⩾ ⩿ ⪀ ⪁ ⪂ ⪃ ⪄ ⪅ ⪆ ⪇ ⪈ ⪉ ⪊ ⪋ ⪌ ⪍ ⪎ ⪏ ⪐ ⪑ ⪒ ⪓ ⪔ ⪕ ⪖ ⪗ ⪘ ⪙ ⪚ ⪛ ⪜ ⪝ ⪞ ⪟ ⪠ ⪡ ⪢ ⪣ ⪤ ⪥ ⪦ ⪧ ⪨ ⪩ ⪪ ⪫ ⪬ ⪭ ⪮ ⪯ ⪰ ⪱ ⪲ ⪳ ⪴ ⪵ ⪶ ⪷ ⪸ ⪹ ⪺ ⪻ ⪼ ⪽ ⪾ ⪿ ⫀ ⫁ ⫂ ⫃ ⫄ ⫅ ⫆ ⫇ ⫈ ⫉ ⫊ ⫋ ⫌ ⫍ ⫎ ⫏ ⫐ ⫑ ⫒ ⫓ ⫔ ⫕ ⫖ ⫗ ⫘ ⫙ ⫷ ⫸ ⫹ ⫺ ⊢ ⊣`;
 2. All symbols in point 1, preceded by a dot (`.`) to be made elementwise;
 3. The operators `<:`, `>:`, `.!`, and `in`, which cannot be preceded by a dot (`.`).

Not all of these have a definition in the standard `Base` library. However, they are available for other packages to define and use as appropriate.

In everyday use, most of these comparison operators are not relevant. The most common ones used are the standard mathematical functions for ordering; see the Syntax section for a list.

Like most other operators in Julia, comparison operators are [functions][1] and can be called as functions. For instance, `(<)(1, 2)` is identical in meaning to `1 < 2`.


  [1]: https://www.wikiod.com/julia-lang/functions

## Chained Comparisons
Multiple comparison operators used together are chained, as if connected via the [`&&` operator][1]. This can be useful for readable and mathematically concise comparison chains, such as

    # same as 0 < i && i <= length(A)
    isinbounds(A, i)       = 0 < i ≤ length(A)

    # same as Set() != x && issubset(x, y)
    isnonemptysubset(x, y) = Set() ≠ x ⊆ y

However, there is an important difference between `a > b > c` and `a > b && b > c`; in the latter, the term `b` is evaluated twice. This does not matter much for plain old symbols, but could matter if the terms themselves have side effects. For instance,

    julia> f(x) = (println(x); 2)
    f (generic function with 1 method)

    julia> 3 > f("test") > 1
    test
    true

    julia> 3 > f("test") && f("test") > 1
    test
    test
    true

Let’s take a deeper look at chained comparisons, and how they work, by seeing how they are parsed and lowered into [expressions][2]. First, consider the simple comparison, which we can see is just a plain old function call:

    julia> dump(:(a > b))
    Expr
      head: Symbol call
      args: Array{Any}((3,))
        1: Symbol >
        2: Symbol a
        3: Symbol b
      typ: Any

Now if we chain the comparison, we notice that the parsing has changed:

    julia> dump(:(a > b >= c))
    Expr
      head: Symbol comparison
      args: Array{Any}((5,))
        1: Symbol a
        2: Symbol >
        3: Symbol b
        4: Symbol >=
        5: Symbol c
      typ: Any

After parsing, the expression is then lowered to its final form:

    julia> expand(:(a > b >= c))
    :(begin 
            unless a > b goto 3
            return b >= c
            3: 
            return false
        end)

and we note indeed that this is the same as for `a > b && b >= c`:

    julia> expand(:(a > b && b >= c))
    :(begin 
            unless a > b goto 3
            return b >= c
            3: 
            return false
        end)


  [1]: https://www.wikiod.com/julia-lang/conditionals#Short-circuit operators: && and ||
  [2]: https://www.wikiod.com/julia-lang/expressions

## Ordinal Numbers
We will look at how to implement custom comparisons by implementing a custom type, [ordinal numbers](https://en.wikipedia.org/wiki/Ordinal_number). To simplify the implementation, we will focus on a small subset of these numbers: all ordinal numbers up to but not including ε₀. Our implementation is focused on simplicity, not speed; however, the implementation is not slow either.

We store ordinal numbers by their [Cantor normal form](https://en.wikipedia.org/wiki/Ordinal_arithmetic#Cantor_normal_form). Because ordinal arithmetic is not commutative, we will take the common convention of storing most significant terms first.

    immutable OrdinalNumber <: Number
        βs::Vector{OrdinalNumber}
        cs::Vector{Int}
    end

Since the Cantor normal form is unique, we may test equality simply through recursive equality:

<!-- if version [gte 0.5.0] -->

In version v0.5, there is a very nice syntax for doing this compactly:

    import Base: ==
    α::OrdinalNumber == β::OrdinalNumber = α.βs == β.βs && α.cs == β.cs

<!-- end version if -->

<!-- if version [lt 0.5.0] -->

Otherwise, define the function as is more typical:

    import Base: ==
    ==(α::OrdinalNumber, β::OrdinalNumber) = α.βs == β.βs && α.cs == β.cs

<!-- end version if -->

To finish our order, because this type has a total order, we should overload the `isless` function:

    import Base: isless
    function isless(α::OrdinalNumber, β::OrdinalNumber)
        for i in 1:min(length(α.cs), length(β.cs))
            if α.βs[i] < β.βs[i]
                return true
            elseif α.βs[i] == β.βs[i] && α.cs[i] < β.cs[i]
                return true
            end
        end
        return length(α.cs) < length(β.cs)
    end

To test our order, we can create some methods to make ordinal numbers.
Zero, of course, is obtained by having no terms in the Cantor normal form:

    const ORDINAL_ZERO = OrdinalNumber([], [])
    Base.zero(::Type{OrdinalNumber}) = ORDINAL_ZERO

We can defined an `expω` to compute `ω^α`, and use that to compute 1 and ω:

    expω(α) = OrdinalNumber([α], [1])
    const ORDINAL_ONE = expω(ORDINAL_ZERO)
    Base.one(::Type{OrdinalNumber}) = ORDINAL_ONE
    const ω = expω(ORDINAL_ONE)

We now have a fully functional ordering function on ordinal numbers:

    julia> ORDINAL_ZERO < ORDINAL_ONE < ω < expω(ω)
    true

    julia> ORDINAL_ONE > ORDINAL_ZERO
    true

    julia> sort([ORDINAL_ONE, ω, expω(ω), ORDINAL_ZERO])
    
    4-element Array{OrdinalNumber,1}:
                                                                                                           OrdinalNumber(OrdinalNumber[],Int64[])
                                                                         OrdinalNumber(OrdinalNumber[OrdinalNumber(OrdinalNumber[],Int64[])],[1])
                                       OrdinalNumber(OrdinalNumber[OrdinalNumber(OrdinalNumber[OrdinalNumber(OrdinalNumber[],Int64[])],[1])],[1])
     OrdinalNumber(OrdinalNumber[OrdinalNumber(OrdinalNumber[OrdinalNumber(OrdinalNumber[OrdinalNumber(OrdinalNumber[],Int64[])],[1])],[1])],[1])

In the last example, we see that the printing of ordinal numbers could be better, but the result is as expected.

## Using ==, ===, and isequal
There are three equality operators: `==`, `===`, and `isequal`. (The last is not really an operator, but it is a function and all operators are functions.)

## When to use `==`

`==` is _value_ equality. It returns `true` when two objects represent, in their present state, the same value.

For instance, it is obvious that

    julia> 1 == 1
    true

but furthermore

    julia> 1 == 1.0
    true
    
    julia> 1 == 1.0 + 0.0im
    true
    
    julia> 1 == 1//1
    true

The right hand sides of each equality above are of a different [type][1], but they still represent the same value.

For mutable objects, like [arrays][2], `==` compares their present value.

    julia> A = [1, 2, 3]
    3-element Array{Int64,1}:
     1
     2
     3
    
    julia> B = [1, 2, 3]
    3-element Array{Int64,1}:
     1
     2
     3
    
    julia> C = [1, 3, 2]
    3-element Array{Int64,1}:
     1
     3
     2
    
    julia> A == B
    true
    
    julia> A == C
    false
    
    julia> A[2], A[3] = A[3], A[2]  # swap 2nd and 3rd elements of A
    (3,2)
    
    julia> A
    3-element Array{Int64,1}:
     1
     3
     2
    
    julia> A == B
    false
    
    julia> A == C
    true

Most of the time, `==` is the right choice.

## When to use `===`

`===` is a far stricter operation than `==`. Instead of value equality, it measures egality. Two objects are egal if they cannot be distinguished from each other by the program itself. Thus we have

    julia> 1 === 1
    true

as there is no way to tell a `1` apart from another `1`. But

    julia> 1 === 1.0
    false

because although `1` and `1.0` are the same value, they are of different types, and so the program can tell them apart.

Furthermore,

    julia> A = [1, 2, 3]
    3-element Array{Int64,1}:
     1
     2
     3
    
    julia> B = [1, 2, 3]
    3-element Array{Int64,1}:
     1
     2
     3
    
    julia> A === B
    false
    
    julia> A === A
    true

which may at first seem surprising! How could the program distinguish between the two vectors `A` and `B`? Because vectors are mutable, it could modify `A`, and then it would behave differently from `B`. But no matter how it modifies `A`, `A` will always behave the same as `A` itself. So `A` is egal to `A`, but not egal to `B`.

Continuing along this vein, observe

    julia> C = A
    3-element Array{Int64,1}:
     1
     2
     3
    
    julia> A === C
    true

By assigning `A` to `C`, we say that `C` has _aliased_ `A`. That is, it has become just another name for `A`. Any modifications done to `A` will be observed by `C` also. Therefore, there is no way to tell the difference between `A` and `C`, so they are egal.

## When to use `isequal`

The difference between `==` and `isequal` is very subtle. The biggest difference is in how floating point numbers are handled:

    julia> NaN == NaN
    false

This possibly surprising result is [defined][3] by the IEEE standard for floating point types (IEEE-754). But this is not useful in some cases, such as sorting. `isequal` is provided for those cases:

    julia> isequal(NaN, NaN)
    true

On the flip side of the spectrum, `==` treats IEEE negative zero and positive zero as the same value (also as specified by IEEE-754). These values have distinct representations in memory, however.

    julia> 0.0
    0.0
    
    julia> -0.0
    -0.0

    julia> 0.0 == -0.0
    true

Again for sorting purposes, `isequal` distinguishes between them.

    julia> isequal(0.0, -0.0)
    false

  [1]: https://www.wikiod.com/julia-lang/types
  [2]: https://www.wikiod.com/julia-lang/arrays
  [3]: https://stackoverflow.com/questions/1565164/what-is-the-rationale-for-all-comparisons-returning-false-for-ieee754-nan-values


