---
title: "scalaz"
slug: "scalaz"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

[Scalaz](https://github.com/scalaz/scalaz) is a Scala library for functional programming.

It provides purely functional data structures to complement those from the Scala standard library. It defines a set of foundational type classes (e.g. `Functor`,`Monad`) and corresponding instances for a large number of data structures.

## ApplyUsage
    import scalaz._
    import Scalaz._
    
    scala> Apply[Option].apply2(some(1), some(2))((a, b) => a + b)
    res0: Option[Int] = Some(3)

    scala> val intToString: Int => String = _.toString

    scala> Apply[Option].ap(1.some)(some(intToString))
    res1: Option[String] = Some(1)

    scala> Apply[Option].ap(none)(some(intToString))
    res2: Option[String] = None

    scala> val double: Int => Int = _ * 2

    scala> Apply[List].ap(List(1, 2, 3))(List(double))
    res3: List[Int] = List(2, 4, 6)
    
    scala> :kind Apply
    scalaz.Apply's kind is X[F[A]]

## FunctorUsage
    import scalaz._
    import Scalaz._
    scala> val len: String => Int = _.length
    len: String => Int = $$Lambda$1164/969820333@7e758f40
    
    scala> Functor[Option].map(Some("foo"))(len)
    res0: Option[Int] = Some(3)
    
    scala> Functor[Option].map(None)(len)
    res1: Option[Int] = None
    
    scala> Functor[List].map(List("qwer", "adsfg"))(len)
    res2: List[Int] = List(4, 5)

    scala> :kind Functor
    scalaz.Functor's kind is X[F[A]]

## ArrowUsage
    import scalaz._
    import Scalaz._
    scala> val plus1 = (_: Int) + 1
    plus1: Int => Int = $$Lambda$1167/1113119649@6a6bfd97
    
    scala> val plus2 = (_: Int) + 2
    plus2: Int => Int = $$Lambda$1168/924329548@6bbe050f
    
    scala> val rev = (_: String).reverse
    rev: String => String = $$Lambda$1227/1278001332@72685b74
    
    scala> plus1.first apply (1, "abc")
    res0: (Int, String) = (2,abc)
    
    scala> plus1.second apply ("abc", 2)
    res1: (String, Int) = (abc,3)
    
    scala> rev.second apply (1, "abc")
    res2: (Int, String) = (1,cba)
    
    scala> plus1 *** rev apply(7, "abc")
    res3: (Int, String) = (8,cba)
    
    scala> plus1 &&& plus2 apply 7
    res4: (Int, Int) = (8,9)
    
    scala> plus1.product apply (1, 2)
    res5: (Int, Int) = (2,3)

    scala> :kind Arrow
    scalaz.Arrow's kind is X[F[A1,A2]]

