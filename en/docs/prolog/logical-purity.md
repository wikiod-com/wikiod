---
title: "Logical Purity"
slug: "logical-purity"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## dif/2
The predicate `dif/2` is a **pure** predicate: It can be used in all directions and with all instantiation patterns, *always* meaning that its two arguments are *different*.

## Unification
**Unification** is a **pure** relation. It does not produce side effects and can be used in all directions, with either or both arguments fully or only partially instantiated.

In Prolog, unification can happen

- **explicitly**, using built-in predicates like `(=)/2` or `unify_with_occurs_check/2`
- **implicitly**, when unification is used for selecting a suitable clause.

## CLP(FD) constraints
CLP(FD) constraints are completely pure relations. They can be used in all directions for declarative integer arithmetic:

    ?- X #= 1+2.
    X = 3.

    ?- 3 #= Y+2.
    Y = 1.

