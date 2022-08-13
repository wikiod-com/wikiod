---
title: "Type-level Programming"
slug: "type-level-programming"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Introduction to type-level programming
If we consider a heterogenous list, wherein the elements of the list have varied but known types, it might be desirable to be able to perform operations on the elements of the list collectively without discarding the elements' type information. The following example implements a mapping operation over a simple heterogenous list.

Because the element type varies, the class of operations we can perform is restricted to some form of type projection, so we define a trait `Projection` having abstract `type Apply[A]` computing the result *type* of the projection, and `def apply[A](a: A): Apply[A]` computing the result *value* of the projection. 
    
    trait Projection {
      type Apply[A] // <: Any
      def apply[A](a: A): Apply[A]
    }

In implementing `type Apply[A]` we are programming at the type level (as opposed to the value level).

Our heterogenous list type defines a `map` operation parametrized by the desired projection as well as the projection's type. The result of the map operation is abstract, will vary by implementing class and projection, and must naturally still be an `HList`:

    sealed trait HList {
      type Map[P <: Projection] <: HList
      def map[P <: Projection](p: P): Map[P]
    }

In the case of `HNil`, the empty heterogenous list, the result of any projection will always be itself. Here we declare `trait HNil` as a convenience so that we may write `HNil` as a type in lieu of `HNil.type`:

    sealed trait HNil extends HList
    case object HNil extends HNil {
      type Map[P <: Projection] = HNil
      def map[P <: Projection](p: P): Map[P] = HNil
    }

`HCons` is the non-empty heterogenous list. Here we assert that when applying a map operation, the resulting head type is that which results from the application of the projection to the head value (`P#Apply[H]`), and that the resulting tail type is that which results from mapping the projection over the tail (`T#Map[P]`), which is known to be an `HList`:

    case class HCons[H, T <: HList](head: H, tail: T) extends HList {
      type Map[P <: Projection] = HCons[P#Apply[H], T#Map[P]]
      def map[P <: Projection](p: P): Map[P] = HCons(p.apply(head), tail.map(p))
    }

The most obvious such projection is to perform some form of wrapping operation - the following example yields an instance of `HCons[Option[String], HCons[Option[Int], HNil]]`:

    HCons("1", HCons(2, HNil)).map(new Projection {
      type Apply[A] = Option[A]
      def apply[A](a: A): Apply[A] = Some(a)
    })


