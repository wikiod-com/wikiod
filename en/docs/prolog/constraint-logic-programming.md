---
title: "Constraint Logic Programming"
slug: "constraint-logic-programming"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## CLP(FD)
**CLP(FD) constraints** (*Finite Domains*) implement arithmetic over **integers**. They are available in all serious Prolog implementations.



There are two major use cases of CLP(FD) constraints:

 -  Declarative integer arithmetic
 -  Solving combinatorial problems such as planning, scheduling and allocation tasks.

Examples:

    ?- X #= 1+2.
    X = 3.
    
    ?- 3 #= Y+2.
    Y = 1.

Note that if `is/2` were to be used in the second query, an instantiation error would occur:

    ?- 3 is Y+2.
    ERROR: is/2: Arguments are not sufficiently instantiated

## CLP(Q)
**CLP(Q)** implements reasoning over *rational* numbers.

Example:

    ?- { 5/6 = X/2 + 1/3 }.
    X = 1.

## CLP(H)
Prolog itself can be considered as **CLP(H)**: Constraint Logic Programming over *Herbrand&nbsp;terms*. With this perspective, a Prolog program posts constraints over *terms*. For example:

    ?- X = f(Y), Y = a.
    X = f(a),
    Y = a.


