---
title: "Data Structures"
slug: "data-structures"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Association lists
In all serious Prolog systems, **association lists** are available to allow faster than linear access to a collection of elements. These association lists are typically based on *balanced trees* like **AVL&nbsp;trees**. There is a public domain library called `library(assoc)` that ships with many Prolog systems and provides *O(log(N))* operations for inserting, fetching and changing elements to a collection.

## Lists
**Lists** are a special kind of *compound term*. Lists are defined inductively:

- the atom `[]` is a list, denoting the *empty list*.
- *if* `Ls` is a list, then the term `'.'(L, Ls)` is *also* a list.

There is a special syntax for denoting lists conveniently in Prolog:

1. The list `'.'(a, '.'(b, '.'(c, [])))` can also be written as `[a,b,c]`.
2. The term `'.'(L, Ls)` can also be written as `[L|Ls]`.

These notations can be combined in any way. For example, the term `[a,b|Ls]` is a list *iff* `Ls` is a list.


**Creating lists**

A list consisting of literals unified with the variable List:

    ?- List = [1,2,3,4].
    List = [1, 2, 3, 4].

Building a list by consing:

    ?- Tail = [2, 3, 4], List = [1|Tail].
    Tail = [2, 3, 4],
    List = [1, 2, 3, 4].

Building a list of unknown values using the built-in `length/2`:

    ?- length(List,5).
    List = [_G496, _G499, _G502, _G505, _G508].

Since in Prolog everything is in essence a Term, lists behave heterogeneous:

    ?- List = [1, 2>1, this, term(X), 7.3, a-A].
    List = [1, 2>1, this, term(X), 7.3, a-A].

This means a list can also contain other lists, also called inner lists:

    List = [[1,2],[3,[4]]].


## Pairs
By convention, the functor `(-)/2` is often used to denote **pairs** of elements in Prolog. For example, the term `-(A, B)` denotes the pair of elements `A` and&nbsp;`B`. In Prolog, `(-)/2` is defined as an *infix&nbsp;operator*. Therefore, the term can be written equivalently as&nbsp;`A-B`.

Many commonly available predicates also use this syntax to denote pairs. Examples of this are `keysort/2` and `pairs_keys_values/3`.

## Terms
On a very high level, Prolog only has a single data type, called **term**. In Prolog, all data is represented by Prolog&nbsp;terms. Terms are defined inductively:

- an **atom** is a term. Examples of atoms are: `x`, `test` and `'quotes and space'`.
- a **variable** is a term. Variables start with an uppercase letter or underscore `_`.
- integers and floating point numbers are terms. Examples: `42` and `42.42`.
- a **compound term** is a term, defined inductively as follows: *If* `T1`, `T2`, ..., `T_n` are terms, *then* <i>F</i>(`T1`,`T2`,...,`T_n`) is also a term, where _F_ is called the **functor** of the compound term.

## Terms with named fields using library(record)
The `[record][1]` library provides the ability to create compound terms with named fields. The directive ```:- record/1 <spec>``` compiles to a collection of predicates that initialize, set and get fields in the term defined by ```<spec>```. 

For example, we can  define a `point` data structure with named fields `x` and `y`: 


    :- use_module(library(record)).
    
    :- record point(x:integer=0,
                    y:integer=0).
    
    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    ?- default_point(Point), point_x(Point, X), set_x_of_point(10, Point, Point1).
    Point = point(0, 0),
    X = 0,
    Point1 = point(10, 0).
    
    ?- make_point([y(20)], Point). 
    Point = point(0, 20).
    
    ?-  is_point(X). 
    false.
    
    ?- is_point(point(_, _)).
    false.
    
    ?- is_point(point(1, a)).
    false.
    
    ?- is_point(point(1, 1)).
    true.
    
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


  [1]: http://www.swi-prolog.org/pldoc/man?section=record



