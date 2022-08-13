---
title: "Futures"
slug: "futures"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Consuming a Failed Future
Sometimes the computation in a Future can create an exception, which will cause the Future to fail.  In the "Creating a Future" example, what if the calling code passed `55` and `0` to the `divide` method?  It'd throw an `ArithmeticException` after trying to divide by zero, of course.  How would that be handled in consuming code?  There are actually a handful of ways to deal with failures.

Handle the exception with `recover` and pattern matching.

    object Calculator {
        def calculateAndReport(a: Int, b: Int) = {
            val eventualQuotient = FutureDivider divide(a, b)
        
            eventualQuotient recover {
                case ex: ArithmeticException => println(s"It failed with: ${ex.getMessage}")
            }
        }
    }

Handle the exception with the `failed` projection, where the exception becomes the value of the Future:

    object Calculator {
        def calculateAndReport(a: Int, b: Int) = {
            val eventualQuotient = FutureDivider divide(a, b)
        
            // Note the use of the dot operator to get the failed projection and map it.
            eventualQuotient.failed.map {
                ex => println(s"It failed with: ${ex.getMessage}")
            }
        }
    }

## Sequencing and traversing Futures
In some cases it is necessary to calculate a variable amount of values on separate Futures. Assume to have a `List[Future[Int]]`, but instead a `List[Int]` needs to be processed. Then the question is how to turn this instance of `List[Future[Int]]` into a `Future[List[Int]]`. For this purpose there is the `sequence` method on the [`Future`][1] companion object.

    def listOfFuture: List[Future[Int]] = List(1,2,3).map(Future(_))
    def futureOfList: Future[List[Int]] = Future.sequence(listOfFuture)

In general `sequence` is a commonly known operator within the world of functional programming that transforms `F[G[T]]` into `G[F[T]]` with restrictions to `F` and `G`.

There is an alternate operator called `traverse`, which works similar but takes a function as an extra argument. With the identity function `x => x` as a parameter it behaves like the `sequence` operator.

    def listOfFuture: List[Future[Int]] = List(1,2,3).map(Future(_))
    def futureOfList: Future[List[Int]] = Future.traverse(listOfFuture)(x => x)

However, the extra argument allows to modify each future instance inside the given `listOfFuture`. Furthermore, the first argument doesn't need to be a list of `Future`. Therefore it is possible to transform the example as follows:

    def futureOfList: Future[List[Int]] = Future.traverse(List(1,2,3))(Future(_))

In this case the `List(1,2,3)` is directly passed as first argument and the identity function `x => x` is replaced with the function `Future(_)` to similarly wrap each `Int` value into a `Future`. An advantage of this is that the intermediary `List[Future[Int]]` can be omitted to improve performance.

  [1]: http://www.scala-lang.org/api/2.9.3/scala/concurrent/Future$.html

## Creating a Future
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    object FutureDivider {
        def divide(a: Int, b: Int): Future[Int] = Future {
            // Note that this is integer division.
            a / b
        }
    }

Quite simply, the `divide` method creates a Future that will resolve with the quotient of `a` over `b`.

## Consuming a Successful Future
The easiest way to consume a _successful_ Future-- or rather, get the value inside the Future-- is to use the `map` method.  Suppose some code calls the `divide` method of the `FutureDivider` object from the "Creating a Future" example.  What would the code need to look like to get the quotient of `a` over `b`?

    object Calculator {
        def calculateAndReport(a: Int, b: Int) = {
            val eventualQuotient = FutureDivider divide(a, b)
            
            eventualQuotient map {
                quotient => println(quotient)
            }
        }
    }

## Putting the Future Together
The previous examples demonstrated the individual features of a Future, handling success and failure cases.  Usually, however, both features are handled much more tersely.  Here's the example, written in a neater and more realistic way:

    object Calculator {
        def calculateAndReport(a: Int, b: Int) = {
            val eventualQuotient = FutureDivider divide(a, b)
            
            eventualQuotient map {
                quotient => println(s"Quotient: $quotient")
            } recover {
                case ex: ArithmeticException => println(s"It failed with: ${ex.getMessage}")
            }
        }
    }

## Combine Multiple Futures – For Comprehension
The *for comprehension* is a compact way to run a block of code that depends on the successful result of multiple futures.

With `f1, f2, f3` three `Future[String]`'s that will contain the strings `one, two, three` respectively,

    val fCombined = 
        for {
            s1 <- f1
            s2 <- f2
            s3 <- f3
        } yield (s"$s1 - $s2 - $s3")

`fCombined` will be a `Future[String]` containing the string `one - two - three` once all the futures have completed successfully.

Note that an implicit ExectionContext is assumed here.

Also, keep in mind that for comprehension is just a [syntactic sugar][1] for a flatMap method, so Future objects construction inside for body would eliminate concurrent execution of code-blocks enclosed by futures and lead to sequential code. You see it on example:

    val result1 = for {
      first <- Future {
        Thread.sleep(2000)
        System.currentTimeMillis()
      }
      second <- Future {
        Thread.sleep(1000)
        System.currentTimeMillis()
      }
    } yield first - second
    
    val fut1 = Future {
      Thread.sleep(2000)
      System.currentTimeMillis()
    }
    val fut2 = Future {
      Thread.sleep(1000)
      System.currentTimeMillis()
    }
    val result2 = for {
      first <- fut1
      second <- fut2
    } yield first - second

Value enclosed by `result1` object would be always negative while 
`result2` would be positive.

For more details about the *for comprehension* and `yield` in general, see http://docs.scala-lang.org/tutorials/FAQ/yield.html


  [1]: https://en.wikipedia.org/wiki/Syntactic_sugar

