---
title: "Higher-Order Programming"
slug: "higher-order-programming"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## call/N predicates
The `call/N` family of predicates can call arbitrary Prolog goals at run time:

    ?- G=true, call(G).
    true.

    ?- G=(true,false), call(G).
    false.

## maplist/[2,3]
`maplist/2` and `maplist/3` are higher-order predicates, which allow the definition of a predicate to be lifted about a single element to *lists* of such elements. These predicates can be defined using `call/2` and `call/3` as building blocks and ship with many Prolog systems.

For example:

    ?- maplist(dif(a), [X,Y,Z]).
    dif(X, a),
    dif(Y, a),
    dif(Z, a).

## foldl/4
A *fold* (from the left) is a higher-order relation between:

- a predicate with 3 arguments
- a list of elements
- an initial state
- a final state, which is the result of applying the predicate to successive elements while carrying through intermediate states.

For example: Use `foldl/4` to express the *sum* of all elements in a list, using a predicate as a building block to define the sum of *two* elements:

    ?- foldl(plus, [2,3,4], 0, S).
    S = 9.

## Meta-call
In Prolog, the so-called **meta-call** is a built-in language feature. All Prolog code is represented by Prolog *terms*, allowing goals to be constructed dynamically and be used like other goals without additional predicates:

    ?- Goal = dif(X, Y), Goal.
    dif(X, Y).

Using this mechanism, other higher-order predicates can be defined in Prolog itself.


## Call a list of goals
To call a list of goals as if it were a conjunction of goals, combine the higher-order predicates call/1 and maplist/2: 

```prolog
?- Gs = [X = a, Y = b], maplist(call, Gs).
Gs = [a=a, b=b],
X = a,
Y = b.
```

