---
title: "FREE statement"
slug: "free-statement"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

The `FREE` statement frees allocated memory for one or more identifiers, either by POINTER or from a BASED working storage identifier.  Use after FREE is illegal.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/hOauL.png

## FREE an allocation
    01 field-1 PIC X(80) BASED.

    ALLOCATE field-1

    *> use field-1

    FREE field-1

    *> further use of field-1 will cause memory corruption

