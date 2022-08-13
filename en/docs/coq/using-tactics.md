---
title: "Using Tactics"
slug: "using-tactics"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

This section includes information on how to use various Coq tactics and techniques (case analysis, proof by induction, auto, etc.) to prove theorems.

## Trivial example of a case analysis
In Coq, `destruct` more or less corresponds to a case analysis. It is similar to induction except that there's no induction hypothesis. Here is a (admittedly rather trivial) example of this tactic:

    Require Import Coq.Arith.Lt.

    Theorem atLeastZero : forall a,
    0 <= a.
    Proof.
      intros.
      destruct a. (* Case analysis *)
      - reflexivity. (* 0 >= 0 *)
      - apply le_0_n. (* S a is always greater than zero *)
      Qed.

