---
title: "Streams"
slug: "streams"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

Streams are lazily-evaluated, meaning they can be used to implement generators, which will provide or 'generate' a new item of the specified type on-demand, rather than before the fact. This ensures only the computations necessary are done.

## Using a Stream to Generate a Random Sequence
`genRandom` creates a stream of random numbers that has a one in four chance of terminating each time it's called.

    def genRandom: Stream[String] = {
      val random = scala.util.Random.nextFloat()
      println(s"Random value is: $random")
      if (random < 0.25) {
        Stream.empty[String]
      } else {
        ("%.3f : A random number" format random) #:: genRandom
      }
    }
    
    lazy val randos = genRandom  // getRandom is lazily evaluated as randos is iterated through
    
    for {
      x <- randos
    } println(x) // The number of times this prints is effectively randomized.

Note the `#::` construct, which *lazily recurses*: because it is prepending the current random number to a stream, it does not evaluate the remainder of the stream until it is iterated through.

## Infinite self-referent stream
```scala
// Generate stream that references itself in its evaluation
lazy val primes: Stream[Int] =
  2 #:: Stream.from(3, 2)
    .filter { i => primes.takeWhile(p => p * p <= i).forall(i % _ != 0) }
    .takeWhile(_ > 0) // prevent overflowing

// Get list of 10 primes
assert(primes.take(10).toList == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))

// Previously calculated values were memoized, as shown by toString
assert(primes.toString == "Stream(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, ?)")
```

## Infinite Streams via Recursion
Streams can be built that reference themselves and thus become infinitely recursive.

    // factorial
    val fact: Stream[BigInt] = 1 #:: fact.zipWithIndex.map{case (p,x)=>p*(x+1)}
    fact.take(10)  // (1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
    fact(24)       // 620448401733239439360000

    // the Fibonacci series
    val fib: Stream[BigInt] = 0 #:: fib.scan(1:BigInt)(_+_)
    fib.take(10)  // (0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    fib(124)      // 36726740705505779255899443

    // random Ints between 10 and 99 (inclusive)
    def rndInt: Stream[Int] = (util.Random.nextInt(90)+10) #:: rndInt
    rndInt.take(10)  // (20, 95, 14, 44, 42, 78, 85, 24, 99, 85)

In this context the difference between [Var, Val, and Def][1] is interesting. As a `def` each element is recalculated every time it is referenced. As a `val` each element is retained and reused after it's been calculated. This can be demonstrated by creating a side-effect with each calculation.

    // def with extra output per calculation
    def fact: Stream[Int] = 1 #:: fact.zipWithIndex.map{case (p,x)=>print("!");p*(x+1)}
    fact(5)  // !!!!!!!!!!!!!!! 120
    fact(4)  // !!!!!!!!!! 24
    fact(7)  // !!!!!!!!!!!!!!!!!!!!!!!!!!!! 5040

    // now as val
    val fact: Stream[Int] = 1 #:: fact.zipWithIndex.map{case (p,x)=>print("!");p*(x+1)}
    fact(5)  // !!!!! 120
    fact(4)  // 24
    fact(7)  // !! 5040

This also explains why the random number `Stream` doesn't work as a `val`.

    val rndInt: Stream[Int] = (util.Random.nextInt(90)+10) #:: rndInt
    rndInt.take(5)  // (79, 79, 79, 79, 79)


  [1]: https://www.wikiod.com/scala/var-val-and-def

