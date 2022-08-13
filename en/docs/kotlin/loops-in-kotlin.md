---
title: "Loops in Kotlin"
slug: "loops-in-kotlin"
draft: false
images: []
weight: 7831
type: docs
toc: true
---

In Kotlin, loops are compiled down to optimized loops wherever possible. For example, if you iterate over a number range, the bytecode will be compiled down to a corresponding loop based on plain int values to avoid the overhead of object creation.

## Looping over iterables
You can loop over any iterable by using the standard for-loop:

    val list = listOf("Hello", "World", "!")
    for(str in list) {
        print(str)
    }

Lots of things in Kotlin are iterable, like number ranges:

    for(i in 0..9) {
        print(i)
    }

If you need an index while iterating:

    for((index, element) in iterable.withIndex()) {
        print("$element at index $index")
    }

There is also a functional approach to iterating included in the standard library, without apparent language constructs, using the forEach function:

    iterable.forEach {
        print(it.toString())
    }

`it` in this example implicitly holds the current element, see [Lambda Functions][1]

  [1]: https://www.wikiod.com/kotlin/functions#Lambda Functions

## Repeat an action x times
    repeat(10) { i ->
        println("This line will be printed 10 times")
        println("We are on the ${i + 1}. loop iteration")
    }

## Break and continue
Break and continue keywords work like they do in other languages.

    while(true) {
        if(condition1) {
            continue // Will immediately start the next iteration, without executing the rest of the loop body
        }
        if(condition2) {
            break // Will exit the loop completely
        }
    }

If you have nested loops, you can label the loop statements and qualify the break and continue statements to specify which loop you want to continue or break:

    outer@ for(i in 0..10) {
        inner@ for(j in 0..10) {
            break       // Will break the inner loop
            break@inner // Will break the inner loop
            break@outer // Will break the outer loop
        }
    }
This approach won't work for the functional `forEach` construct, though.

## Iterating over a Map in kotlin
    //iterates over a map, getting the key and value at once

    var map = hashMapOf(1 to "foo", 2 to "bar", 3 to "baz")
    
    for ((key, value) in map) {
        println("Map[$key] = $value")
    }

## Recursion
Looping via recursion is also possible in Kotlin as in most programming languages.

    fun factorial(n: Long): Long = if (n == 0) 1 else n * factorial(n - 1)

    println(factorial(10)) // 3628800

In the example above, the `factorial` function will be called repeatedly by itself until the given condition is met.


## While Loops
While and do-while loops work like they do in other languages:

    while(condition) {
        doSomething()
    }

    do {
        doSomething()
    } while (condition)

In the do-while loop, the condition block has access to values and variables declared in the loop body.

## Functional constructs for iteration
The [Kotlin Standard Library][1] also provides numerous useful functions to iteratively work upon collections.

For example, the [`map`][2] function can be used to transform a list of items.

    val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)
    val numberStrings = numbers.map { "Number $it" }

One of the many advantages of this style is it allows to chain operations in a similar fashion. Only a minor modification would be required if say, the list above were needed to be filtered for even numbers. The [`filter`][3] function can be used.

    val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)
    val numberStrings = numbers.filter { it % 2 == 0 }.map { "Number $it" }


  [1]: https://kotlinlang.org/api/latest/jvm/stdlib/index.html
  [2]: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/map.html
  [3]: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/filter.html

