---
title: "Java Interoperability"
slug: "java-interoperability"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Converting Scala Collections to Java Collections and vice versa
When you need to pass a collection into a Java method:
```scala
import scala.collection.JavaConverters._

val scalaList = List(1, 2, 3)
JavaLibrary.process(scalaList.asJava)
```
If the Java code returns a Java collection, you can turn it into a Scala collection in a similar manner:
```scala
import scala.collection.JavaConverters._

val javaCollection = JavaLibrary.getList
val scalaCollection = javaCollection.asScala
```
Note that these are decorators, so they merely wrap the underlying collections in a Scala or Java collection interface. Therefore, the calls `.asJava` and `.asScala` do not copy the collections.

## Arrays
Arrays are regular JVM arrays with a twist that they are treated as invariant and have special constructors and implicit conversions. Construct them without the `new` keyword.

     val a = Array("element")

Now `a` has type `Array[String]`.

     val acs: Array[CharSequence] = a
     //Error: type mismatch;  found   : Array[String]  required: Array[CharSequence]

Although `String` is convertible to `CharSequence`, `Array[String]` is not convertible to `Array[CharSequence]`.

You can use an `Array` like other collections, thanks to an implicit conversion to `TraversableLike` `ArrayOps`:

     val b: Array[Int] = a.map(_.length)

Most of the Scala collections (`TraversableOnce`) have a `toArray` method taking an implicit `ClassTag` to construct the result array:

     List(0).toArray
     //> res1: Array[Int] = Array(0)

This makes it easy to use any `TraversableOnce` in your Scala code and then pass it to Java code which expects an array.

## Scala and Java type conversions
 Scala offers implicit conversions between all the major collection types in the JavaConverters object.

The following type conversions are bidirectional.

| Scala Type | Java Type |
| ---------- | --------- |
| Iterator   | java.util.Iterator   |
| Iterator   | java.util.Enumeration   |
| Iterator   | java.util.Iterable   |
| Iterator   | java.util.Collection   |
| mutable.Buffer | java.util.List |
| mutable.Set | java.util.Set |
| mutable.Map | java.util.Map |
| mutable.ConcurrentMap | java.util.concurrent.ConcurrentMap |


Certain other Scala collections can also be converted to Java, but do not have a conversion back to the original Scala type:

| Scala Type | Java Type |
| ------ | ------ |
| Seq   | java.util.List   |
| mutable.Seq   | java.util.List   |
| Set   | java.util.Set   |
| Map   | java.util.Map   |

*Reference*: 

[Conversions Between Java and Scala Collections][1]


  [1]: http://docs.scala-lang.org/overviews/collections/conversions-between-java-and-scala-collections.html

## Functional Interfaces for Scala functions - scala-java8-compat
[A Java 8 compatibility kit for Scala.](https://github.com/scala/scala-java8-compat)

Most examples are copied from [Readme](https://github.com/scala/scala-java8-compat/blob/master/README.md)

**Converters between scala.FunctionN and java.util.function**

    import java.util.function._
    import scala.compat.java8.FunctionConverters._
    
    val foo: Int => Boolean = i => i > 7
    def testBig(ip: IntPredicate) = ip.test(9)
    println(testBig(foo.asJava))  // Prints true
    
    val bar = new UnaryOperator[String]{ def apply(s: String) = s.reverse }
    List("cod", "herring").map(bar.asScala)    // List("doc", "gnirrih")
    
    def testA[A](p: Predicate[A])(a: A) = p.test(a)
    println(testA(asJavaPredicate(foo))(4))  // Prints false

**Converters between scala.Option and java.util classes Optional, OptionalDouble, OptionalInt, and OptionalLong.**

    import scala.compat.java8.OptionConverters._
    
        class Test {
          val o = Option(2.7)
          val oj = o.asJava        // Optional[Double]
          val ojd = o.asPrimitive  // OptionalDouble
          val ojds = ojd.asScala   // Option(2.7) again
        }

**Converters from Scala collections to Java 8 Streams**



    import java.util.stream.IntStream
    
    import scala.compat.java8.StreamConverters._
    import scala.compat.java8.collectionImpl.{Accumulator, LongAccumulator}
    
    
      val m = collection.immutable.HashMap("fish" -> 2, "bird" -> 4)
      val parStream: IntStream = m.parValueStream
      val s: Int = parStream.sum
      // 6, potientially computed in parallel
      val t: List[String] = m.seqKeyStream.toScala[List]
      // List("fish", "bird")
      val a: Accumulator[(String, Int)] = m.accumulate // Accumulator[(String, Int)]
    
      val n = a.stepper.fold(0)(_ + _._1.length) +
        a.parStream.count // 8 + 2 = 10
    
      val b: LongAccumulator = java.util.Arrays.stream(Array(2L, 3L, 4L)).accumulate
      // LongAccumulator
      val l: List[Long] = b.to[List] // List(2L, 3L, 4L)




