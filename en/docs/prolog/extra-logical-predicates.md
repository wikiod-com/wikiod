---
title: "Extra-logical Predicates"
slug: "extra-logical-predicates"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Predicates with side effects
Predicates that produce **side effects** leave the realm of pure logic. These are for example:

- `writeq/1`
- `read/1`
- `format/2`

Side effects are phenomena that cannot be reasoned about within the program. For example, deletion of a file or output on the system terminal.



## Meta-logical predicates
Predicates that reason about *instantiations* are called **meta-logical**. Examples are:

- `var/1`
- `ground/1`
- `integer/1`

These predicates are outside the realm of pure monotonic logic programs, because they break properties like *commutativity* of conjunction.

Other predicates that are meta-logical include:

- `arg/3`
- `functor/3`
- `(=..)/2`

These predicates could *in principle* be modeled within first-order logic, but require an infinite number of clauses.

## All-solutions predicates
Predicates that reason about *all solutions* are extra-logical. These are for example:

- `setof/3`
- `findall/3`
- `bagof/3`


## !/0 and related predicates
Predicates that impede or prohibit a **declarative** reading of Prolog programs are extra-logical. Examples of such predicates are:

- `!/0`
- `(->)/2` and if-then-else
- `(\+)/1`

These predicates can only be understood procedurally, by taking into account the actual control flow of the interpreter, and as such are beyond the realm of pure logic.

