---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
- [1,2,3]
- [1 2 3]
- [1 2 3; 4 5 6; 7 8 9]
- Array(type, dims...)
- ones(type, dims...)
- zeros(type, dims...)
- trues(type, dims...)
- falses(type, dims...)
- push!(A, x)
- pop!(A)
- unshift!(A, x)
- shift!(A)

## Parameters
| Parameters | Remarks |
|------------|---------|
| **For**    | `push!(A, x)`, `unshift!(A, x)`
| `A`        | The array to add to. |
| `x`        | The element to add to the array.

## Manual construction of a simple array
One can initialize a Julia array by hand, using the square-brackets syntax:

```julia
julia> x = [1, 2, 3]
3-element Array{Int64,1}:
 1
 2
 3
```

The first line after the command shows the size of the array you created. It also shows the type of its elements and its dimensionality (int this case `Int64` and `1`, repectively). For a two-dimensional array, you can use spaces and semi-colon:

```julia
julia> x = [1 2 3; 4 5 6]
2x3 Array{Int64,2}:
 1  2  3
 4  5  6
```

To create an uninitialized array, you can use the `Array(type, dims...)` method:

```julia
julia> Array(Int64, 3, 3)
3x3 Array{Int64,2}:
 0  0  0
 0  0  0
 0  0  0
```

The functions `zeros`, `ones`, `trues`, `falses` have methods that behave exactly the same way, but produce arrays full of `0.0`, `1.0`, `True` or `False`, respectively.

## Array types
In Julia, Arrays have types parametrized by two variables: a type `T` and a dimensionality `D` (`Array{T, D}`). For a 1-dimensional array of integers, the type is:

```julia
julia> x = [1, 2, 3];
julia> typeof(x)
Array{Int64, 1}
```

If the array is a 2-dimensional matrix, `D` equals to 2:

```julia
julia> x = [1 2 3; 4 5 6; 7 8 9]
julia> typeof(x)
Array{Int64, 2}
```

The element type can also be abstract types:

```julia
julia> x = [1 2 3; 4 5 "6"; 7 8 9]
3x3 Array{Any,2}:
 1  2  3   
 4  5 "6"
 7  8  9
```

Here `Any` (an abstract type) is the type of the resulting array.

**Specifying Types when Creating Arrays**

When we create an Array in the way described above, Julia will do its best to infer the proper type that we might want.  In the initial examples above, we entered inputs that looked like integers, and so Julia defaulted to the default `Int64` type.  At times, however, we might want to be more specific.  In the following example, we specify that we want the type to be instead `Int8`:

    x1 = Int8[1 2 3; 4 5 6; 7 8 9]
    typeof(x1)  ## Array{Int8,2}

We could even specify the type as something such as `Float64`, even if we write the inputs in a way that might otherwise be interpreted as integers by default (e.g. writing `1`  instead of `1.0`).  e.g.

    x2 = Float64[1 2 3; 4 5 6; 7 8 9]


## Initialize an Empty Array
We can use the `[]` to create an empty Array in Julia.  The simplest example would be:

    A = [] # 0-element Array{Any,1}

Arrays of type `Any` will generally not perform as well as those with a specified type.  Thus, for instance, we can use:

    B = Float64[]  ## 0-element Array{Float64,1}
    C = Array{Float64}[]  ## 0-element Array{Array{Float64,N},1}
    D = Tuple{Int, Int}[] ## 0-element Array{Tuple{Int64,Int64},1}

See [Initialize an Empty Array of Tuples in Julia](http://stackoverflow.com/questions/19419124/initialize-an-empty-array-of-tuples-in-julia) for source of last example.



## Arrays of Arrays - Properties and Construction
In Julia, you can have an Array that holds other Array type objects.  Consider the following examples of initializing various types of Arrays:

    A = Array{Float64}(10,10)  # A single Array, dimensions 10 by 10, of Float64 type objects

    B = Array{Array}(10,10,10)  # A 10 by 10 by 10 Array.  Each element is an Array of unspecified type and dimension.

    C = Array{Array{Float64}}(10)  ## A length 10, one-dimensional Array.  Each element is an Array of Float64 type objects but unspecified dimensions

    D = Array{Array{Float64, 2}}(10)  ## A length 10, one-dimensional Array.  Each element of is an 2 dimensional array of Float 64 objects

Consider for instance, the differences between C and D here:

    julia> C[1] = rand(3)
    3-element Array{Float64,1}:
     0.604771
     0.985604
     0.166444

    julia> D[1] = rand(3)
    ERROR: MethodError: 

`rand(3)` produces an object of type `Array{Float64,1}`.  Since the only specification for the elements of `C` are that they be Arrays with elements of type Float64, this fits within the definition of `C`.  But, for `D` we specified that the elements must be 2 dimensional Arrays.  Thus, since `rand(3)` does not produce a 2 dimensional array, we cannot use it to assign a value to a specific element of `D`

**Specify Specific Dimensions of Arrays within an Array**

Although we can specify that an Array will hold elements which are of type Array, and we can specify that, e.g. those elements should be 2-dimensional Arrays, we cannot directly specify the dimenions of those elements.  E.g. we can't directly specify that we want an Array holding 10 Arrays, each of which being 5,5.  We can see this from the syntax for the `Array()` function used to construct an Array:

> **Array{T}(dims)** 
>
> constructs an uninitialized dense array with element type T. dims may be a tuple or a series of integer arguments. The syntax Array(T, dims) is also available, but deprecated.

The type of an Array in Julia encompasses the number of the dimensions but not the size of those dimensions.  Thus, there is no place in this syntax to specify the precise dimensions.  Nevertheless, a similar effect could be achieved using an Array comprehension:

    E = [Array{Float64}(5,5) for idx in 1:10]

Note: this documentation mirrors the following [SO Answer](http://stackoverflow.com/a/39335125/3541976)

## Vectors
Vectors are one-dimensional arrays, and support mostly the same interface as their multi-dimensional counterparts. However, vectors also support additional operations.

First, note that `Vector{T}` where `T` is some type means the same as `Array{T,1}`.

    julia> Vector{Int}
    Array{Int64,1}
    
    julia> Vector{Float64}
    Array{Float64,1}

One reads `Array{Int64,1}` as "one-dimensional array of `Int64`".

Unlike multi-dimensional arrays, vectors can be resized. Elements can be added or removed from the front or back of the vector. These operations are all [constant amortized time][1].

    julia> A = [1, 2, 3]
    3-element Array{Int64,1}:
     1
     2
     3
    
    julia> push!(A, 4)
    4-element Array{Int64,1}:
     1
     2
     3
     4
    
    julia> A
    4-element Array{Int64,1}:
     1
     2
     3
     4
    
    julia> pop!(A)
    4
    
    julia> A
    3-element Array{Int64,1}:
     1
     2
     3
    
    julia> unshift!(A, 0)
    4-element Array{Int64,1}:
     0
     1
     2
     3
    
    julia> A
    4-element Array{Int64,1}:
     0
     1
     2
     3
    
    julia> shift!(A)
    0
    
    julia> A
    3-element Array{Int64,1}:
     1
     2
     3

As is convention, each of these functions `push!`, `pop!`, `unshift!`, and `shift!` ends in an exclamation mark to indicate that they are mutate their argument. The functions `push!` and `unshift!` return the array, whereas `pop!` and `shift!` return the element removed.

  [1]: https://stackoverflow.com/questions/200384/constant-amortized-time

## Concatenation
It is often useful to build matrices out of smaller matrices.

## Horizontal Concatenation

Matrices (and vectors, which are treated as column vectors) can be horizontally concatenated using the `hcat` function.

    julia> hcat([1 2; 3 4], [5 6 7; 8 9 10], [11, 12])
    2×6 Array{Int64,2}:
     1  2  5  6   7  11
     3  4  8  9  10  12

There is convenience syntax available, using square bracket notation and spaces:

    julia> [[1 2; 3 4] [5 6 7; 8 9 10] [11, 12]]
    2×6 Array{Int64,2}:
     1  2  5  6   7  11
     3  4  8  9  10  12

This notation can closely match the notation for block matrices used in linear algebra:

    julia> A = [1 2; 3 4]
    2×2 Array{Int64,2}:
     1  2
     3  4
    
    julia> B = [5 6; 7 8]
    2×2 Array{Int64,2}:
     5  6
     7  8
    
    julia> [A B]
    2×4 Array{Int64,2}:
     1  2  5  6
     3  4  7  8

Note that you cannot horizontally concatenate a single matrix using the `[]` syntax, as that would instead create a one-element vector of matrices:

    julia> [A]
    1-element Array{Array{Int64,2},1}:
     [1 2; 3 4]

## Vertical Concatenation

Vertical concatenation is like horizontal concatenation, but in the vertical direction. The function for vertical concatenation is `vcat`.

    julia> vcat([1 2; 3 4], [5 6; 7 8; 9 10], [11 12])
    6×2 Array{Int64,2}:
      1   2
      3   4
      5   6
      7   8
      9  10
     11  12

Alternatively, square bracket notation can be used with semicolons `;` as the delimiter:

    julia> [[1 2; 3 4]; [5 6; 7 8; 9 10]; [11 12]]
    6×2 Array{Int64,2}:
      1   2
      3   4
      5   6
      7   8
      9  10
     11  12

[Vectors][1] can be vertically concatenated too; the result is a vector:

    julia> A = [1, 2, 3]
    3-element Array{Int64,1}:
     1
     2
     3
    
    julia> B = [4, 5]
    2-element Array{Int64,1}:
     4
     5
    
    julia> [A; B]
    5-element Array{Int64,1}:
     1
     2
     3
     4
     5

Horizontal and vertical concatenation can be combined:

    julia> A = [1 2
                3 4]
    2×2 Array{Int64,2}:
     1  2
     3  4
    
    julia> B = [5 6 7]
    1×3 Array{Int64,2}:
     5  6  7
    
    julia> C = [8, 9]
    2-element Array{Int64,1}:
     8
     9
    
    julia> [A C; B]
    3×3 Array{Int64,2}:
     1  2  8
     3  4  9
     5  6  7

  [1]: https://www.wikiod.com/julia-lang/arrays#Vectors

