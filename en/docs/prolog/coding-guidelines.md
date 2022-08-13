---
title: "Coding guidelines"
slug: "coding-guidelines"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Indentation
There are only a few language constructs in Prolog, and several ways for indenting them are common.

No matter which style is chosen, one principle that should always be adhered to is to **never** place `(;)/2` at the *end* of a line. This is because `;` and&nbsp;`,` look very similar, and `,`&nbsp;frequently occurs at the end of a line. Therefore, clauses that use a disjunction should for example be written as:

<pre>
(  Goal1
;  Goal2 
)
</pre>

## Naming
When programming in Prolog, we must pick two kinds of names:

- names of **predicates**
- names of **variables**.

A good *predicate* name makes clear what each argument means. By convention, **underscores** are used in names to separate the description of different arguments. This is because `underscores_keep_even_longer_names_readable`, whereas `mixingTheCasesDoesNotDoThisToTheSameExtent`.

Examples of good predicates names are:

- `parent_child/2`
- `person_likes/2`
- `route_to/2` 

Note that *descriptive* names are used. Imperatives are avoided. Using descriptive names is advisable because Prolog predicates can typically be used in *multiple* directions, and the name should be applicable also of all or none of the arguments are instantiated.

Mixed capitalization is more common when selecting names of *variables*. For example: `BestSolutions`, `MinElement`, `GreatestDivisor`. A common convention for naming variables that denote successive *states* is using `S0`, `S1`, `S2`, ..., `S`, where `S` represents the final&nbsp;state.

## Order of arguments
Ideally, Prolog predicates can be used in all directions. For many pure predicates, this is also actually the case. However, some predicates only work in particular *modes*, which means instantiation patterns of their arguments.

By convention, the most common argument order for such predicates is:

- **input** arguments are placed first. These arguments must be instantiated *before* the predicate is called. 
- *pairs* of arguments that belong together are placed adjacently, such as `p(..., State0, State, ...)`
- intended **output** arguments are placed last. These predicates are instantiated by the predicate.

