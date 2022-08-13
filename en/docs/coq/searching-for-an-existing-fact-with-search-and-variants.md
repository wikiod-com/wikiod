---
title: "Searching for an existing fact with Search and variants"
slug: "searching-for-an-existing-fact-with-search-and-variants"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
- `Search qualid.` (* for Coq 8.4 and newer versions *)
- `SearchAbout qualid.` (* deprecated synonym. *)

## Parameters
| **Parameter** | **Description** |
| ------------- | ----------------|
| `qualid` | The identifier or pattern to search for. It can involve notations |

Before Coq 8.4, `Search` had the meaning of the current `SearchHead`: only search for facts where the pattern matches in the conclusion of the statement.

## Facts about a particular identifier
To see all the facts involving the `le` relation from the prelude:

    Coq < Search le.
    le_n: forall n : nat, n <= n
    le_S: forall n m : nat, n <= m -> n <= S m
    ...
    max_l: forall n m : nat, m <= n -> Nat.max n m = n
    max_r: forall n m : nat, n <= m -> Nat.max n m = m
    ...

To search about all the facts involving the notation `<`:

    Coq < Search "<".
    exists_lt: forall (Q : nat -> Prop) (k l : nat), exists_between Q k l -> k < l
    in_int_intro: forall p q r : nat, p <= r -> r < q -> in_int p q r
    in_int_lt: forall p q r : nat, in_int p q r -> p < q
    ...

## Searching for a pattern
Search for all facts involving a pattern in an hypothesis or conclusion:

    Coq < Search (_ + O).
    plus_n_O: forall n : nat, n = n + 0

The `_` character serves as a wildcard, it can be used multiple times:

    Coq < Search (S _ <= _).
    le_S_n: forall n m : nat, S n <= S m -> n <= m
    le_n_S: forall n m : nat, n <= m -> S n <= S m

You can also search for non-linear patterns:

    Coq < Search (?x <= ?x).
    le_n: forall n : nat, n <= n

## Searching for a pattern in the conclusion of a lemma
Search for a lemma when you know what its conclusion ought to be:

    Coq < SearchPattern (S _ <= _).
    le_n_S: forall n m : nat, n <= m -> S n <= S m

You can also search on a partial conclusion (the conclusion and one or several last hypotheses).

    Coq < Require Import Arith.
    Coq < SearchPattern (?x <= ?y -> ?y <= _ -> ?x <= _).
    Nat.le_trans: forall n m p : nat, n <= m -> m <= p -> n <= p

Warning: if you mix up the order of the hypotheses, you won't find anything:

    Coq < SearchPattern (?y <= _ -> ?x <= ?y -> ?x <= _).

