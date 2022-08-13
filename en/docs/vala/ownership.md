---
title: "Ownership"
slug: "ownership"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Note that the compiler will not prevent you from using variable for which its value ownership been transfeered.

## Transfer Ownership
<!-- language: lang-vala -->

    var foo = new uint8[12];
    var bar = (owned) foo;
    assert (foo == null);

The `bar` variable will own the value previously owned by `foo`.

## Implicit Copy
<!-- language: lang-vala -->

    var foo = new uint8[12];
    var bar = foo;
    assert (foo != bar);

In this example, the both `foo` and `bar` possess a strong reference, but since `uint8[]` only support single ownership, a copy is made.

