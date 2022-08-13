---
title: "Partial Functions"
slug: "partial-functions"
draft: false
images: []
weight: 9914
type: docs
toc: true
---

## Basic syntax
Scala has a special type of function called a [partial function][1], which extends [normal functions][2] -- meaning that a `PartialFunction` instance can be used wherever `Function1` is expected. Partial functions can be defined anonymously using `case` syntax also used in [pattern matching][3]:

    val pf: PartialFunction[Boolean, Int] = {
      case true => 7
    }

    pf.isDefinedAt(true) // returns true
    pf(true) // returns 7

    pf.isDefinedAt(false) // returns false
    pf(false) // throws scala.MatchError: false (of class java.lang.Boolean)

As seen in the example, a partial function need not be defined over the whole domain of its first parameter. A standard `Function1` instance is assumed to be *total*, meaning that it is defined for every possible argument.


  [1]: http://www.scala-lang.org/api/current/index.html#scala.PartialFunction
  [2]: http://www.scala-lang.org/api/current/index.html#scala.Function1
  [3]: https://www.wikiod.com/scala/pattern-matching

## Usage as a total function
Partial functions are very common in idiomatic Scala. They are often used for their convenient `case`-based syntax to define total functions over [traits][1]:

    sealed trait SuperType // `sealed` modifier allows inheritance within current build-unit only
    case object A extends SuperType
    case object B extends SuperType
    case object C extends SuperType

    val input: Seq[SuperType] = Seq(A, B, C)

    input.map {
      case A => 5
      case _ => 10
    } // Seq(5, 10, 10)

This saves the additional syntax of a `match` statement in a regular anonymous function. Compare:

    input.map { item => 
      item match {
        case A => 5
        case _ => 10
      }
    } // Seq(5, 10, 10)

It is also frequently used to perform a parameter decomposition using pattern matching, when a tuple or a case class is passed to a function:

    val input = Seq("A" -> 1, "B" -> 2, "C" -> 3)

    input.map { case (a, i) =>
       a + i.toString
    } // Seq("A1", "B2", "C3")


  [1]: https://www.wikiod.com/scala/traits

## Usage with `collect`
While partial function are often used as convenient syntax for total functions, by including a final wildcard match (`case _`), in some methods, their partiality is key. One very common example in idiomatic Scala is the [`collect`](http://www.scala-lang.org/api/current/index.html#scala.collection.TraversableLike@collect[B](pf:PartialFunction[A,B]):Traversable[B]) method, defined in the Scala collections library. Here, partial functions allow the common functions of examining the elements of a collection to map and/or filter them to occur in one compact syntax.

**Example 1**

Assuming that we have a square root function defined as partial function:

    val sqRoot:PartialFunction[Double,Double] = { case n if n > 0 => math.sqrt(n) }

We can invoke it with the `collect` combinator:

    List(-1.1,2.2,3.3,0).collect(sqRoot)

effectively performing the same operation as:

    List(-1.1,2.2,3.3,0).filter(sqRoot.isDefinedAt).map(sqRoot)

**Example 2**
    
    sealed trait SuperType // `sealed` modifier allows inheritance within current build-unit only
    case class A(value: Int) extends SuperType
    case class B(text: String) extends SuperType
    case object C extends SuperType
    
    val input: Seq[SuperType] = Seq(A(5), B("hello"), C, A(25), B(""))
    
    input.collect {
      case A(value) if value < 10   => value.toString
      case B(text) if text.nonEmpty => text
    } // Seq("5", "hello")

There are several things to note in the example above:

- The left-hand side of each pattern match effectively selects elements to process and include in the output. Any value that doesn't have a matching `case` is simply omitted.
- The right-hand side defines the case-specific processing to apply.
- Pattern matching binds variable for use in guard statements (the `if` clauses) and the right-hand side.



## Composition
Partial functions are often used to define a total function in parts:

    sealed trait SuperType
    case object A extends SuperType
    case object B extends SuperType
    case object C extends SuperType
    
    val pfA: PartialFunction[SuperType, Int] = {
      case A => 5
    }
    
    val pfB: PartialFunction[SuperType, Int] = {
      case B => 10
    }
    
    val input: Seq[SuperType] = Seq(A, B, C)
    
    input.map(pfA orElse pfB orElse {
      case _ => 15
    }) // Seq(5, 10, 15)

In this usage, the partial functions are attempted in order of concatenation with the [`orElse`][1] method. Typically, a final partial function is provided that matches all remaining cases. Collectively, the combination of these functions acts as a total function.

This pattern is typically used to separate concerns where a function may effectively act a dispatcher for disparate code paths. This is common, for example, in the [receive method of an Akka Actor][2].

  [1]: http://www.scala-lang.org/api/current/index.html#scala.PartialFunction@orElse[A1%3C:A,B1%3E:B](that:PartialFunction[A1,B1]):PartialFunction[A1,B1]
  [2]: http://doc.akka.io/docs/akka/snapshot/scala/actors.html#Defining_an_Actor_class

## Usage to extract tuples in a map function
These three map functions are equivalent, so use the variation that your team finds most readable.

    val numberNames = Map(1 -> "One", 2 -> "Two", 3 -> "Three")
    
    // 1. No extraction
    numberNames.map(it => s"${it._1} is written ${it._2}" )
    
    // 2. Extraction within a normal function
    numberNames.map(it => {
        val (number, name) = it
        s"$number is written $name"
    })

    // 3. Extraction via a partial function (note the brackets in the parentheses)
    numberNames.map({ case (number, name) => s"$number is written $name" })

The partial function **must match all input**: any case which doesn't match will throw an exception at runtime.

