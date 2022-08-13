---
title: "Iterables"
slug: "iterables"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
- start(itr)
- next(itr, s)
- done(itr, s)
- take(itr, n)
- drop(itr, n)
- cycle(itr)
- Base.product(xs, ys)

## Parameters
| Parameter | Details |
|-----------|---------|
| **For**   | **All Functions**|
| `itr`     | The iterable to operate on. |
| **For**   | **`next` and `done`** |
| `s`       | An iterator state describing the current position of the iteration. |
| **For**   | **`take` and `drop`** |
| `n`       | The number of elements to take or drop. |
| **For**   | **`Base.product`** |
| `xs`      | The iterable to take first elements of pairs from. |
| `ys`      | The iterable to take second elements of pairs from. |
| ...       | (Note that `product` accepts any number of arguments; if more than two are provided, it will construct tuples of length greater than two.) |

## New iterable type
In Julia, when looping through an iterable object `I` is done with the [`for`][1] syntax:

```julia
for i = I   # or  "for i in I"
    # body
end
```

Behind the scenes, this is translated to:

```julia
state = start(I)
while !done(I, state)
    (i, state) = next(I, state)
    # body
end
```

Therefore, if you want `I` to be an iterable, you need to define `start`, `next` and `done` methods for its type. Suppose you define a [type][2] `Foo` containing an [array][3] as one of the fields:

```julia
type Foo
    bar::Array{Int,1}
end
```

We instantiate a `Foo` object by doing:

```
julia> I = Foo([1,2,3])
Foo([1,2,3])

julia> I.bar
3-element Array{Int64,1}:
 1
 2
 3
```

If we want to iterate through `Foo`, with each element `bar` being returned by each iteration, we define the methods:

```julia
import Base: start, next, done

start(I::Foo) = 1

next(I::Foo, state) = (I.bar[state], state+1)

function done(I::Foo, state)
    if state == length(I.bar)
        return true
    end
    return false
end
```

Note that since these [functions][4] belong to the `Base` module, we must first `import` their names before adding new methods to them.

After the methods are defined, `Foo` is compatible with the iterator interface:

```julia
julia> for i in I
           println(i)
       end

1
2
3
```


  [1]: https://www.wikiod.com/julia-lang/for-loops
  [2]: https://www.wikiod.com/julia-lang/types
  [3]: https://www.wikiod.com/julia-lang/arrays
  [4]: https://www.wikiod.com/julia-lang/functions

## Combining Lazy Iterables
The standard library comes with a rich collection of lazy iterables (and libraries such as [Iterators.jl](https://github.com/JuliaLang/Iterators.jl) provide even more). Lazy iterables can be composed to create more powerful iterables in constant time. The most important lazy iterables are [take and drop](http://docs.julialang.org/en/release-0.5/stdlib/collections/#Base.take), from which many other functions can be created.

## Lazily slice an iterable

Arrays can be sliced with slice notation. For instance, the following returns the 10th to 15th elements of an array, inclusive:

    A[10:15]

However, slice notation does not work with all iterables. For instance, we cannot slice a generator expression:

    julia> (i^2 for i in 1:10)[3:5]
    ERROR: MethodError: no method matching getindex(::Base.Generator{UnitRange{Int64},##1#2}, ::UnitRange{Int64})

Slicing [strings][1] may not have the expected Unicode behaviour:

    julia> "αααα"[2:3]
    ERROR: UnicodeError: invalid character index
     in getindex(::String, ::UnitRange{Int64}) at ./strings/string.jl:130
    
    julia> "αααα"[3:4]
    "α"

We can define a function `lazysub(itr, range::UnitRange)` to do this kind of slicing on arbitrary iterables. This is defined in terms of `take` and `drop`:

    lazysub(itr, r::UnitRange) = take(drop(itr, first(r) - 1), last(r) - first(r) + 1)

The implementation here works because for `UnitRange` value `a:b`, the following steps are performed:

- drops the first `a-1` elements
- takes the `a`th element, `a+1`th element, and so forth, until the `a+(b-a)=b`th element

In total, `b-a` elements are taken. We can confirm our implementation is correct in each case above:

    julia> collect(lazysub("αααα", 2:3))
    2-element Array{Char,1}:
     'α'
     'α'
    
    julia> collect(lazysub((i^2 for i in 1:10), 3:5))
    3-element Array{Int64,1}:
      9
     16
     25

## Lazily shift an iterable circularly

The `circshift` operation on arrays will shift the array as if it were a circle, then relinearize it. For example,

    julia> circshift(1:10, 3)
    10-element Array{Int64,1}:
      8
      9
     10
      1
      2
      3
      4
      5
      6
      7

Can we do this lazily for all iterables? We can use the `cycle`, `drop`, and `take` iterables to implement this functionality.

    lazycircshift(itr, n) = take(drop(cycle(itr), length(itr) - n), length(itr))

Along with lazy types being more performant in many situations, this lets us do `circshift`-like functionality on types that would otherwise not support it:

    julia> circshift("Hello, World!", 3)
    ERROR: MethodError: no method matching circshift(::String, ::Int64)
    Closest candidates are:
      circshift(::AbstractArray{T,N}, ::Real) at abstractarraymath.jl:162
      circshift(::AbstractArray{T,N}, ::Any) at abstractarraymath.jl:195
    
    julia> String(collect(lazycircshift("Hello, World!", 3)))
    "ld!Hello, Wor"

<!-- if version [gte 0.5.0] -->

## Making a multiplication table

Let's make a [multiplication table](https://en.wikipedia.org/wiki/Multiplication_table) using lazy iterable functions to create a matrix.

The key functions to use here are:

- `Base.product`, which computes a [Cartesian product](https://en.wikipedia.org/wiki/CartesianProduct).
- `prod`, which computes a regular product (as in multiplication)
- `:`, which creates a range
- [`map`][2], which is a higher order function applying a function to each element of a collection

The solution is:

    julia> map(prod, Base.product(1:10, 1:10))
    10×10 Array{Int64,2}:
      1   2   3   4   5   6   7   8   9   10
      2   4   6   8  10  12  14  16  18   20
      3   6   9  12  15  18  21  24  27   30
      4   8  12  16  20  24  28  32  36   40
      5  10  15  20  25  30  35  40  45   50
      6  12  18  24  30  36  42  48  54   60
      7  14  21  28  35  42  49  56  63   70
      8  16  24  32  40  48  56  64  72   80
      9  18  27  36  45  54  63  72  81   90
     10  20  30  40  50  60  70  80  90  100

<!-- end version if -->


  [1]: https://www.wikiod.com/julia-lang/strings
  [2]: https://www.wikiod.com/julia-lang/higher-order-functions#Map, filter, and reduce

## Lazily-Evaluated Lists
It's possible to make a simple lazily-evaluated list using mutable types and [closures][1]. A lazily-evaluated list is a list whose elements are not evaluated when it's constructed, but rather when it is accessed. Benefits of lazily evaluated lists include the possibility of being infinite.

    import Base: getindex
    type Lazy
        thunk
        value
        Lazy(thunk) = new(thunk)
    end
    
    evaluate!(lazy::Lazy) = (lazy.value = lazy.thunk(); lazy.value)
    getindex(lazy::Lazy) = isdefined(lazy, :value) ? lazy.value : evaluate!(lazy)
    
    import Base: first, tail, start, next, done, iteratorsize, HasLength, SizeUnknown
    abstract List
    immutable Cons <: List
        head
        tail::Lazy
    end
    immutable Nil <: List end
    
    macro cons(x, y)
        quote
            Cons($(esc(x)), Lazy(() -> $(esc(y))))
        end
    end
    
    first(xs::Cons) = xs.head
    tail(xs::Cons) = xs.tail[]
    start(xs::Cons) = xs
    next(::Cons, xs) = first(xs), tail(xs)
    done(::List, ::Cons) = false
    done(::List, ::Nil) = true
    iteratorsize(::Nil) = HasLength()
    iteratorsize(::Cons) = SizeUnknown()

Which indeed works as it would in a language like [Haskell][2], where all lists are lazily-evaluated:

    julia> xs = @cons(1, ys)
    Cons(1,Lazy(false,#3,#undef))
    
    julia> ys = @cons(2, xs)
    Cons(2,Lazy(false,#5,#undef))
    
    julia> [take(xs, 5)...]
    5-element Array{Int64,1}:
     1
     2
     1
     2
     1

In practice, it is better to use the [Lazy.jl](https://github.com/MikeInnes/Lazy.jl) package. However, the implementation of the lazy list above sheds lights into important details about how to construct one's own iterable type.


  [1]: https://www.wikiod.com/julia-lang/closures
  [2]: https://www.wikiod.com/haskell/lists

