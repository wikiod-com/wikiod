---
title: "Arithmetic"
slug: "arithmetic"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
 - +x
 - -x
 - a + b
 - a - b
 - a * b
 - a / b
 - a ^ b
 - a % b
 - 4a
 - sqrt(a)

## Quadratic Formula
Julia uses similar binary operators for basic arithmetic operations as does mathematics or other programming languages. Most operators can be written in infix notation (that is, placed in between the values being computed). Julia has an order of operations that matches the common convention in mathematics.

For instance, the below code implements the [quadratic formula](https://en.wikipedia.org/wiki/Quadratic_formula), which demonstrates the `+`, `-`, `*`, and `/` operators for addition, subtraction, multiplication, and division respectively. Also shown is _implicit multiplication_, where a number can be placed directly before a symbol to mean multiplication; that is, `4a` means the same as `4*a`.

    function solvequadratic(a, b, c)
        d = sqrt(b^2 - 4a*c)
        (-b - d) / 2a, (-b + d) / 2a
    end

Usage:

    julia> solvequadratic(1, -2, -3)
    (-1.0,3.0)


## Sieve of Eratosthenes
The remainder operator in Julia is the `%` operator. This operator behaves similarly to the `%` in languages such as C and C++. `a % b` is the signed remainder left over after dividing `a` by `b`.

This operator is very useful for implementing certain algorithms, such as the following implementation of the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).

    iscoprime(P, i) = !any(x -> i % x == 0, P)

    function sieve(n)
        P = Int[]
        for i in 2:n
            if iscoprime(P, i)
                push!(P, i)
            end
        end
        P
    end

Usage:

    julia> sieve(20)
    8-element Array{Int64,1}:
      2
      3
      5
      7
     11
     13
     17
     19


## Matrix Arithmetic
Julia uses the standard mathematical meanings of arithmetic operations when applied to matrices. Sometimes, elementwise operations are desired instead. These are marked with a full stop (`.`) preceding the operator to be done elementwise. (Note that elementwise operations are often not as efficient as loops.)

## Sums

The `+` operator on matrices is a matrix sum. It is similar to an elementwise sum, but it does not broadcast shape. That is, if `A` and `B` are the same shape, then `A + B` is the same as `A .+ B`; otherwise, `A + B` is an error, whereas `A .+ B` may not necessarily be.

    julia> A = [1 2
                3 4]
    2??2 Array{Int64,2}:
     1  2
     3  4

    julia> B = [5 6
                7 8]
    2??2 Array{Int64,2}:
     5  6
     7  8
    
    julia> A + B
    2??2 Array{Int64,2}:
      6   8
     10  12
    
    julia> A .+ B
    2??2 Array{Int64,2}:
      6   8
     10  12
    
    julia> C = [9, 10]
    2-element Array{Int64,1}:
      9
     10

    julia> A + C
    ERROR: DimensionMismatch("dimensions must match")
     in promote_shape(::Tuple{Base.OneTo{Int64},Base.OneTo{Int64}}, ::Tuple{Base.OneTo{Int64}}) at ./operators.jl:396
     in promote_shape(::Array{Int64,2}, ::Array{Int64,1}) at ./operators.jl:382
     in _elementwise(::Base.#+, ::Array{Int64,2}, ::Array{Int64,1}, ::Type{Int64}) at ./arraymath.jl:61
     in +(::Array{Int64,2}, ::Array{Int64,1}) at ./arraymath.jl:53
    
    julia> A .+ C
    2??2 Array{Int64,2}:
     10  11
     13  14

Likewise, `-` computes a matrix difference. Both `+` and `-` can also be used as unary operators.

## Products

The `*` operator on matrices is the [matrix product][1] (not the elementwise product). For an elementwise product, use the `.*` operator. Compare (using the same matrices as above):

    julia> A * B
    2??2 Array{Int64,2}:
     19  22
     43  50

    julia> A .* B
    2??2 Array{Int64,2}:
      5  12
     21  32

## Powers

The `^` operator computes [matrix exponentiation][2]. Matrix exponentiation can be useful for computing values of certain recurrences quickly. For instance, the [Fibonacci numbers][3] can be generated by the [matrix expression][4]

    fib(n) = (BigInt[1 1; 1 0]^n)[2]

As usual, the `.^` operator can be used where elementwise exponentiation is the desired operation.

  [1]: https://en.wikipedia.org/wiki/Matrix_multiplication
  [2]: https://en.wikipedia.org/wiki/Matrix_multiplication#Powers_of_matrices
  [3]: https://en.wikipedia.org/wiki/Fibonacci_number
  [4]: https://en.wikipedia.org/wiki/Fibonacci_number#Matrix_form

