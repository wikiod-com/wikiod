---
title: "Set operations"
slug: "set-operations"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
 1. C = union(A,B);
 2. C = intersect(A,B);
 3. C = setdiff(A,B);
 4. a = ismember(A,x);

## Parameters
| Parameter | Details |
| ------ | ------ |
| A,B | sets, possibly matrices or vectors   |
| x | possible element of a set |

## Elementary set operations
It's possible to perform elementary set operations with Matlab. Let's assume we have given two vectors or arrays

    A = randi([0 10],1,5);
    B = randi([-1 9], 1,5);
and we want to find all elements which are in `A` and in `B`. For this we can use

    C = intersect(A,B);
`C` will include all numbers which are part of `A` and part of `B`.
If we also want to find the position of these elements we call

    [C,pos] = intersect(A,B);
`pos` is the position of these elements such that `C == A(pos)`.

Another basic operation is the union of two sets

    D = union(A,B);
Herby contains `D` all elements of `A` and `B`.

Note that `A` and `B` are hereby treated as sets which means that it does not matter how often an element is part of `A` or `B`. To clarify this one can check 
`D == union(D,C)`.

If we want to obtain the data that is in 'A' but not in 'B' we can use the following function

    E = setdiff(A,B);

We want to note again that this are sets such that following statement holds `D == union(E,B)`.

Suppose we want to check if 

    x = randi([-10 10],1,1);
is an element of either `A` or `B` we can execute the command

    a = ismember(A,x);
    b = ismember(B,x);
If `a==1` then `x` is element of `A` and `x` is no element is `a==0`. The same goes for `B`. If `a==1 && b==1` `x` is also an element of `C`. If `a == 1 || b == 1` `x` is element of `D` and if `a == 1 || b == 0` it's also element of `E`.

 

