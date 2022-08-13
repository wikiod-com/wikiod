---
title: "Parallel Collections"
slug: "parallel-collections"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Parallel collections facilitate parallel programming by hiding low-level parallelization details. This makes taking advantage of multi-core architectures easy. Examples of parallel collections include `ParArray`, `ParVector`, `mutable.ParHashMap`, `immutable.ParHashMap`, and `ParRange`. A full list can be found [in the documentation][1].

  [1]: http://docs.scala-lang.org/overviews/parallel-collections/concrete-parallel-collections

## Creating and Using Parallel Collections
To create a parallel collection from a sequential collection, call the `par` method. To create a sequential collection from a parallel collection, call the `seq` method. This example shows how you turn a regular `Vector` into a `ParVector`, and then back again:

    scala> val vect = (1 to 5).toVector
    vect: Vector[Int] = Vector(1, 2, 3, 4, 5)

    scala> val parVect = vect.par
    parVect: scala.collection.parallel.immutable.ParVector[Int] = ParVector(1, 2, 3, 4, 5)

    scala> parVect.seq
    res0: scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 4, 5)

The `par` method can be chained, allowing you to convert a sequential collection to a parallel collection and immediately perform an action on it:

    scala> vect.map(_ * 2)
    res1: scala.collection.immutable.Vector[Int] = Vector(2, 4, 6, 8, 10)
    
    scala> vect.par.map(_ * 2)
    res2: scala.collection.parallel.immutable.ParVector[Int] = ParVector(2, 4, 6, 8, 10)

In these examples, the work is actually parceled out to multiple processing units, and then re-joined after the work is complete - without requiring developer intervention.


## Pitfalls
**Do not use parallel collections when the collection elements must be received in a specific order.**

Parallel collections perform operations concurrently. That means that all of the work is divided into parts and distributed to different processors. Each processor is unaware of the work being done by others. If the *order of the collection* matters then work processed in parallel is nondeterministic. (Running the same code twice can yield different results.)

---

**Non-associative Operations**

If an operation is non-associative (if the order of execution matters), then the result on a parallelized collection will be nondeterministic.

    scala> val list = (1 to 1000).toList
    list: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10...
    
    scala> list.reduce(_ - _)
    res0: Int = -500498
    
    scala> list.reduce(_ - _)
    res1: Int = -500498
    
    scala> list.reduce(_ - _)
    res2: Int = -500498
    
    scala> val listPar = list.par
    listPar: scala.collection.parallel.immutable.ParSeq[Int] = ParVector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10...
    
    scala> listPar.reduce(_ - _)
    res3: Int = -408314
    
    scala> listPar.reduce(_ - _)
    res4: Int = -422884
    
    scala> listPar.reduce(_ - _)
    res5: Int = -301748

---

**Side Effects**

Operations that have side effects, such as `foreach`, may not execute as desired on parallelized collections due to race conditions. Avoid this by using functions that have no side effects, such as `reduce` or `map`.

    scala> val wittyOneLiner = Array("Artificial", "Intelligence", "is", "no", "match", "for", "natural", "stupidity")

    scala> wittyOneLiner.foreach(word => print(word + " "))
    Artificial Intelligence is no match for natural stupidity 

    scala> wittyOneLiner.par.foreach(word => print(word + " "))
    match natural is for Artificial no stupidity Intelligence

    scala> print(wittyOneLiner.par.reduce{_ + " " + _})
    Artificial Intelligence is no match for natural stupidity

    scala> val list = (1 to 100).toList
    list: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15...

