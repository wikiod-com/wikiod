---
title: "Monotonicity"
slug: "monotonicity"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Monotonic alternatives for non-monotonic constructs
Here are  examples of how to use **monotonic** predicates *instead* of impure, non-monotonic constructs in your programs:

- `dif/2` is meant to be used *instead* of non-monotonic constructs like `(\=)/2`
- arithmetic **constraints** (CLP(FD), CLP(Q) and others) are meant to be used *instead* of moded arithmetic predicates
- `!/0` almost always leads to non-monotonic programs and should be **avoided** entirely.
- **instantiation errors** can be raised in situations where you cannot make a sound decision at this point in time.


## Reasoning about monotonic predicates
**Monotonic** predicates can be debugged by applying *declarative* reasoning.

In pure Prolog, a programming mistake can lead to one or all of the following phenomena:

1. the predicate incorrectly *succeeds* in a case where it should *fail*
2. the predicate incorrectly *fails* in a case where it should *succeed*
3. the predicate unexpectedly *loops* where it should only produce a finite set of answers.

As an example, consider how we can debug case (2) by declarative reasoning: We can systematically *remove* goals of the predicate's clauses and see if the query *still* fails. In monotonic code, removing goals can at most make the resulting program *more general*. Hence, we can pinpoint errors by seeing which of the goals leads to the unexpected failure.




## Examples of monotonic predicates
Examples of **monotonic** predicates are:

- **unification** with `(=)/2` or `unify_with_occurs_check/2`
- `dif/2`, expressing disequality of terms
- **CLP(FD) constraints** like `(#=)/2` and `(#>)/2`, using a monotonic execution mode.

Prolog predicates that only use monotonic goals are themselves monotonic.

Monotonic predicates allow for declarative reasoning:

1. Adding a constraint (i.e., a goal) to a query can at most *reduce*, never extend, the set of solutions.
2. Removing a goal of such predicates can at most *extend*, never reduce, the set of solutions.




## Combining monotonicity with efficiency
It is sometimes argued that, for the sake of efficiency, we must accept the use of non-monotonic constructs in real-world Prolog programs.

There is no evidence for this. Recent research indicates that the pure monotonic subset of Prolog may not only be sufficient to express most real-world programs, but also acceptably efficient in practice. A construct that has recently been discovered and encourages this view is `if_/3`: It combines monotonicity with a reduction of choice points. See [**Indexing dif/2**](https://arxiv.org/abs/1607.01590).

For example, code of the form:

<pre>
pred(L, Ls) :-
    <b>condition(L)</b>,
    then(Ls).
pred(L, Ls) :-
    <b>\+ condition(L)</b>,
    else(Ls).
</pre>

Can be written with `if_/3` as:

<pre>
pred(L, Ls) :-
    if_(<b>condition(L)</b>,
        then(Ls),
        else(Ls)).
</pre>

and *combines* monotonicity with determinism.



## Non-monotonic predicates
Here are examples of predicates that are **not** monotonic:

- meta-logical predicates like `var/1`, `integer/1` etc.
- term comparison predicates like `(@<)/2` and `(@>=)/2`
- predicates that use `!/0`, `(\+)/1` and other constructs that break monotonicity
- *all-solutions predicates* like `findall/3` and `setof/3`.

If these predicates are used, then *adding* goals can lead to more solutions, which runs counter to the important declarative property known from logic that adding constraints can at most *reduce*, never extend, the set of solutions. 

As a consequence, other properties that we rely for declarative debugging and other reasoning are also broken. For example, non-monotonic predicates break the fundamental notion of **commutativity** of conjunction known from first-order logic. The following example illustrates this:

    ?- var(X), X = a.
    X = a.

    ?- X = a, var(X).
    false.

All-solutions predicates like `findall/3` also break monotonicity: *Adding* clauses can lead to the *failure* of goals that previously *had&nbsp;held*. This also runs counter to montonicity as known from first-order logic, where *adding* facts can at most *increase*, never *reduce* the set of consequences.

