---
title: "sub2ind"
slug: "sub2ind"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
 - sub2ind(dims::Tuple{Vararg{Integer}}, I::Integer...)
 - sub2ind{T<:Integer}(dims::Tuple{Vararg{Integer}}, I::AbstractArray{T<:Integer,1}...)

## Parameters
| parameter | details | 
|---|---|
| dims::Tuple{Vararg{Integer}} | size of the array |
| I::Integer... | subscripts(scalar) of the array |
| I::AbstractArray{T<:Integer,1}... | subscripts(vector) of the array |

The second example shows that the result of `sub2ind` might be very buggy in some specific cases. 

## Convert subscripts to linear indices
    julia> sub2ind((3,3), 1, 1)
    1
    
    julia> sub2ind((3,3), 1, 2)
    4
    
    julia> sub2ind((3,3), 2, 1)
    2

    julia> sub2ind((3,3), [1,1,2], [1,2,1])
    3-element Array{Int64,1}:
     1
     4
     2

    



## Pits & Falls
    # no error, even the subscript is out of range.
    julia> sub2ind((3,3), 3, 4)
    12
    
One cannot determine whether a subscript is in the range of an array by comparing its index:

    julia> sub2ind((3,3), -1, 2)
    2

    julia> 0 < sub2ind((3,3), -1, 2) <= 9
    true
    
    



