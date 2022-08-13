---
title: "For Expressions"
slug: "for-expressions"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

## Syntax
- for {clauses} body
- for {clauses} yield body
- for (clauses) body
- for (clauses) yield body


## Parameters
| Parameter | Details |
| --------- | ------- |
| for | Required keyword to use a for loop/comprehension |
| clauses | The iteration and filters over which the for works. |
| yield | Use this if you want to create or 'yield' a collection. Using `yield` will cause the return type of the `for` to be a collection instead of `Unit`.|
| body | The body of the for expression, executed on each iteration. |

## Monadic for comprehensions
If you have several objects of [monadic][1] types, we can achieve combinations of the values using a 'for comprehension':

```
for {
   x <- Option(1)
   y <- Option("b")
   z <- List(3, 4)
} {
    // Now we can use the x, y, z variables
    println(x, y, z)
    x  // the last expression is *not* the output of the block in this case!
}

// This prints
// (1, "b", 3)
// (1, "b", 4)
```

The return type of this block is `Unit`.

If the objects are of the *same* monadic type `M` (e.g. `Option`) then using `yield` will return an object of type `M` instead of `Unit`.

```
val a = for {
   x <- Option(1)
   y <- Option("b")
} yield {
    // Now we can use the x, y variables
    println(x, y)
    // whatever is at the end of the block is the output
    (7 * x, y)
}

// This prints:
// (1, "b")
// The val `a` is set:
// a: Option[(Int, String)] = Some((7,b))

```

Note that the `yield` keyword *cannot* be used in the original example, where there is a mix of monadic types (`Option` and `List`). Trying to do so will yield a compile-time type mismatch error.

  [1]: https://www.wikiod.com/scala/monads

## Nested For Loop
This shows how you can iterate over multiple variables:

    for {
      x <- 1 to 2
      y <- 'a' to 'd'
    } println("(" + x + "," + y + ")")

(Note that `to` here is an infix operator method that returns an [inclusive range][1]. See the definition [here][2].)

This creates the output:

    (1,a)
    (1,b)
    (1,c)
    (1,d)
    (2,a)
    (2,b)
    (2,c)
    (2,d)

Note that this is an equivalent expression, using parentheses instead of brackets:

    for (
      x <- 1 to 2
      y <- 'a' to 'd'
    ) println("(" + x + "," + y + ")")

In order to get all of the combinations into a single vector, we can `yield` the result and set it to a `val`:

    val a = for {
      x <- 1 to 2
      y <- 'a' to 'd'
    } yield "(%s,%s)".format(x, y)
    // a: scala.collection.immutable.IndexedSeq[String] = Vector((1,a), (1,b), (1,c), (1,d), (2,a), (2,b), (2,c), (2,d))

  [1]: http://www.scala-lang.org/api/2.10.3/#scala.collection.immutable.NumericRange$$Inclusive
  [2]: http://www.scala-lang.org/api/2.10.3/#scala.Char

## Iterate Through Collections Using a For Loop
This demonstrates how to print each element of a Map

    val map = Map(1 -> "a", 2 -> "b")
    for (number <- map) println(number) // prints (1,a), (2,b)
    for ((key, value) <- map) println(value) // prints a, b

This demonstrates how to print each element of a list

    val list = List(1,2,3)
    for(number <- list) println(number) // prints 1, 2, 3

## Desugaring For Comprehensions
`for` comprehensions in Scala are just [syntactic sugar][1]. These comprehensions are implemented using the `withFilter`, `foreach`, `flatMap` and `map` methods of their subject types. For this reason, only types that have these methods defined can be utilized in a `for` comprehension.

A `for` comprehension of the following form, with patterns `pN`, generators `gN` and conditions `cN`:

    for(p0 <- x0 if g0; p1 <- g1 if c1) { ??? }

... will de-sugar to nested calls using `withFilter` and `foreach`:

    g0.withFilter({ case p0 => c0  case _ => false }).foreach({
      case p0 => g1.withFilter({ case p1 => c1  case _ => false }).foreach({
        case p1 => ???
      })
    })

Whereas a `for`/`yield` expression of the following form:

    for(p0 <- g0 if c0; p1 <- g1 if c1) yield ???

... will de-sugar to nested calls using `withFilter` and either `flatMap` or `map`:

    g0.withFilter({ case p0 => c0  case _ => false }).flatMap({
      case p0 => g1.withFilter({ case p1 => c1  case _ => false }).map({
        case p1 => ???
      })
    })

(Note that `map` is used in the innermost comprehension, and `flatMap` is used in every outer comprehension.)

A `for` comprehension can be applied to any type implementing the methods required by the de-sugared representation. There are no restrictions on the return types of these methods, so long as they are composable.

   


  [1]: https://en.wikipedia.org/wiki/Syntactic_sugar

## Basic For Loop
    for (x <- 1 to 10)
      println("Iteration number " + x)

This demonstrates iterating a variable, `x`, from `1` to `10` and doing something with that value. The return type of this `for` comprehension is `Unit`.

## Basic For Comprehension
This demonstrates a filter on a for-loop, and the use of `yield` to create a 'sequence comprehension':

    for ( x <- 1 to 10 if x % 2 == 0)
      yield x

The output for this is:

    scala.collection.immutable.IndexedSeq[Int] = Vector(2, 4, 6, 8, 10)

A for comprehension is useful when you need to create a new collection based on the iteration and it's filters.

