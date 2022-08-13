---
title: "Tuples"
slug: "tuples"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

**Why are tuples limited to length 23?**

Tuples are rewritten as objects by the compiler. The compiler has access to `Tuple1` through `Tuple22`. This arbitrary limit was decided by language designers.

**Why do tuple lengths count from 0?**

A `Tuple0` is equivalent to a `Unit`. 

## Creating a new Tuple
A tuple is a heterogeneous collection of two to twenty-two values. A tuple can be defined using parentheses. For tuples of size `2` (also called a 'pair') there's an arrow syntax.

```scala
scala> val x = (1, "hello") 
x: (Int, String) = (1,hello)
scala> val y = 2 -> "world" 
y: (Int, String) = (2,world)
scala> val z = 3 → "foo"     //example of using U+2192 RIGHTWARD ARROW
z: (Int, String) = (3,foo)
```

`x` is a tuple of size two. To access the elements of a tuple use `._1`, through `._22`. For instance, we can use `x._1` to access the first element of the `x` tuple. `x._2` accesses the second element. More elegantly, you can [use tuple extractors][1].

The arrow syntax for creating tuples of size two is primarily used in Maps, which are collections of `(key -> value)` pairs:

```scala
scala> val m = Map[Int, String](2 -> "world")
m: scala.collection.immutable.Map[Int,String] = Map(2 -> world)

scala> m + x
res0: scala.collection.immutable.Map[Int,String] = Map(2 -> world, 1 -> hello)

scala> (m + x).toList
res1: List[(Int, String)] = List((2,world), (1,hello))
```

The syntax for the pair in the map is the arrow syntax, making it clear that 1 is the key and a is the value associated with that key.


  [1]: https://www.wikiod.com/scala/extractors#Tuple Extractors

## Tuples within Collections
Tuples are often used within collections but they must be handled in a specific way. For example, given the following list of tuples: 

    scala> val l = List(1 -> 2, 2 -> 3, 3 -> 4)
    l: List[(Int, Int)] = List((1,2), (2,3), (3,4))

It may seem natural to add the elements together using implicit tuple-unpacking:

    scala> l.map((e1: Int, e2: Int) => e1 + e2)

However this results in the following error:

    <console>:9: error: type mismatch;
     found   : (Int, Int) => Int
     required: ((Int, Int)) => ?
                  l.map((e1: Int, e2: Int) => e1 + e2)

Scala cannot implicitly unpack the tuples in this manner. We have two options to fix this map. The first is to use the positional accessors `_1` and `_2`:

    scala> l.map(e => e._1 + e._2)
    res1: List[Int] = List(3, 5, 7)

The other option is to use a `case` statement to unpack the tuples using pattern matching:

    scala> l.map{ case (e1: Int, e2: Int) => e1 + e2}
    res2: List[Int] = List(3, 5, 7)

These restrictions apply for any higher-order-function applied to a collection of tuples.


