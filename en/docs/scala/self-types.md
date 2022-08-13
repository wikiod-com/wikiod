---
title: "Self types"
slug: "self-types"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntax
  - trait Type { selfId => /*other members can refer to `selfId` in case `this` means something*/ }
  - trait Type { selfId: OtherType => /* other members can use `selfId` and it will be of type `OtherType` */
  - trait Type { selfId: OtherType1 with OtherType2 => /* `selfId` is of type `OtherType1` and `OtherType2` */

Often used with the cake pattern.

## Simple self type example
Self types can be used in traits and classes to define constraints on the concrete classes it is mixed to. It is also possible to use a different identifier for the `this` using this syntax (useful when outer object has to be referenced from an inner object).

Assume you want to store some objects. For that, you create interfaces for the storage and to add values to a container:

     trait Container[+T] {
       def add(o: T): Unit
     }

     trait PermanentStorage[T] {
       /* Constraint on self type: it should be Container
        * we can refer to that type as `identifier`, usually `this` or `self`
        * or the type's name is used. */
       identifier: Container[T] =>

       def save(o: T): Unit = {
         identifier.add(o)
         //Do something to persist too.
       }
     }

This way those are not in the same object hierarchy, but `PermanentStorage` cannot be implemented without also implementing `Container`.


