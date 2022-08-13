---
title: "Working with data in immutable style"
slug: "working-with-data-in-immutable-style"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Value and variable names should be in lower camel case

> Constant names should be in upper camel case. That is, if the member
> is final, immutable and it belongs to a package object or an object,
> it may be considered a constant
> 
> Method, Value and variable names should be in lower camel case

Source: http://docs.scala-lang.org/style/naming-conventions.html

This compile:

    val (a,b) = (1,2)
    // a: Int = 1
    // b: Int = 2

but this doesn't:

    val (A,B) = (1,2)
    // error: not found: value A
    // error: not found: value B




## It is not just val vs. var
### `val` and `var`

    scala> val a = 123
    a: Int = 123
    
    scala> a = 456
    <console>:8: error: reassignment to val
           a = 456
    
    scala> var b = 123
    b: Int = 123
    
    scala> b = 321
    b: Int = 321

 - `val` references are unchangeable: like a `final` variable in `Java`, once it has been initialized you cannot change it
 - `var` references are reassignable as a simple variable declaration in Java

### Immutable and Mutable collections

      val mut = scala.collection.mutable.Map.empty[String, Int]
      mut += ("123" -> 123)
      mut += ("456" -> 456)
      mut += ("789" -> 789)
    
      val imm = scala.collection.immutable.Map.empty[String, Int]
      imm + ("123" -> 123)
      imm + ("456" -> 456)
      imm + ("789" -> 789)

      scala> mut
        Map(123 -> 123, 456 -> 456, 789 -> 789)
    
      scala> imm
        Map()

    scala> imm + ("123" -> 123) + ("456" -> 456) + ("789" -> 789)
        Map(123 -> 123, 456 -> 456, 789 -> 789)

The Scala standard library offers both immutable and mutable data structures, not the reference to it. Each time an immutable data structure get "modified", a new instance is produced instead of modifying the original collection in-place. Each instance of the collection may share significant structure with another instance.

[Mutable and Immutable Collection (Official Scala Documentation)][1]


  [1]: http://docs.scala-lang.org/overviews/collections/overview.html




## But I can't use immutability in this case!
Let's pick as an example a function that takes 2 `Map` and return a `Map` containing every element in `ma` and `mb`:

    def merge2Maps(ma: Map[String, Int], mb: Map[String, Int]): Map[String, Int]

A first attempt could be iterating through the elements of one of the maps using `for ((k, v) <- map)` and somehow return the merged map.

    def merge2Maps(ma: ..., mb: ...): Map[String, Int] = {
    
      for ((k, v) <- mb) {
        ???
      }

    }

This very first move immediately add a constrain: **a mutation outside that `for` is now _needed_**. This is more clear when de-sugaring the `for`:

    // this:
    for ((k, v) <- map) { ??? }

    // is equivalent to:
    map.foreach { case (k, v) => ??? }

### "Why we have to mutate?"

`foreach` relies on side-effects. Every time we want something to happen within a `foreach` we need to "side-effect something", in this case we could mutate a variable `var result` or 
we can use a mutable data structure.

### Creating and filling the `result` map

Let's assume the `ma` and `mb` are `scala.collection.immutable.Map`, we could create the `result` Map from `ma`:

    val result = mutable.Map() ++ ma

Then iterate through `mb` adding its elements and if the `key` of the current element on `ma` already exist, let's override it with the `mb` one.

    mb.foreach { case (k, v) => result += (k -> v) }

### Mutable implementation

So far so good, we "had to use mutable collections" and a correct implementation could be:

    def merge2Maps(ma: Map[String, Int], mb: Map[String, Int]): Map[String, Int] = {
      val result = scala.collection.mutable.Map() ++ ma
      mb.foreach { case (k, v) => result += (k -> v) }
      result.toMap // to get back an immutable Map
    }

As expected:

    scala> merge2Maps(Map("a" -> 11, "b" -> 12), Map("b" -> 22, "c" -> 23))
      Map(a -> 11, b -> 22, c -> 23)

### Folding to the rescue

How can we get rid of `foreach` in this scenario? If all we what to do is basically iterate over the collection elements and apply a function while accumulating the result on option could be using `.foldLeft`:

    def merge2Maps(ma: Map[String, Int], mb: Map[String, Int]): Map[String, Int] = {
      mb.foldLeft(ma) { case (result, (k, v)) => result + (k -> v) }
      // or more concisely mb.foldLeft(ma) { _ + _ }
    }

In this case our "result" is the accumulated value starting from `ma`, the `zero` of the `.foldLeft`.

### Intermediate result

Obviously this immutable solution is producing and destroying many `Map` instances while folding, but it is worth mentioning that those instances are not a full clone of the `Map` accumulated but instead are sharing significant structure (data) with the existing instance.

### Easier reasonability

It is easier to reason about the semantic if it is more declarative as the `.foldLeft` approach. Using immutable data structures could help making our implementation easier to reason on.

