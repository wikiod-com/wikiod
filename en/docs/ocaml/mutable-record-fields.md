---
title: "Mutable record fields"
slug: "mutable-record-fields"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Like most OCaml values, records are immutable by default. However, since OCaml also handles imperative programming, it provides a way to make individual fields *mutable*. Mutable fields can be modified in-place by assignment, rather than having to resort to usual functional techniques, such as functional update.

While introducing side-effects, mutable fields can result in an improved performance when used correctly.

## Declaring a record with mutable fields
In the following, `weight` is declared as a mutable field.

    type person = {
      name: string;
      mutable weight: int
    };;

**Remark**: As far as design is concerned here, one would consider the fact that a `person`'s name isn't likely to change, but their weight is.

## Initializing a record with mutable fields
Initializing a record with mutable fields isn't different from a regular record initialization.

    let john = { name = "John"; weight = 115 };;


## Setting the value to a mutable field
To assign a new value to a mutable record field, use the `<-` operator.

    john.weight <- 120;;

**Note**: The previous expression has a `unit` type.

