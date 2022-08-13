---
title: "DSL Building"
slug: "dsl-building"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Focus on the syntax details to design internal [DSLs](https://en.wikipedia.org/wiki/Domain-specific_language) in Kotlin.

## Infix approach to build DSL
If you have:

    infix fun <T> T?.shouldBe(expected: T?) = assertEquals(expected, this)

you can write the following DSL-like code in your tests:

    @Test
    fun test() {
      100.plusOne() shouldBe 101
    }


## Overriding invoke method to build DSL
If you have:

    class MyExample(val i: Int) {
      operator fun <R> invoke(block: MyExample.() -> R) = block()
      fun Int.bigger() = this > i
    }

you can write the following DSL-like code in your production code:

    fun main2(args: Array<String>) {
        val ex = MyExample(233)
        ex {
            // bigger is defined in the context of `ex`
            // you can only call this method inside this context
            if (777.bigger()) kotlin.io.println("why")
        }
    }




## Using operators with lambdas
If you have:

    val r = Random(233)
    infix inline operator fun Int.rem(block: () -> Unit) {
      if (r.nextInt(100) < this) block()
    }

You can write the following DSL-like code:

    20 % { println("The possibility you see this message is 20%") }

## Using extensions with lambdas
If you have:

    operator fun <R> String.invoke(block: () -> R) = {
      try { block.invoke() }
      catch (e: AssertException) { System.err.println("$this\n${e.message}") }
    }


You can write the following DSL-like code:

    "it should return 2" {
       parse("1 + 1").buildAST().evaluate() shouldBe 2
    }

If you feel confused with `shouldBe` above, see the example `Infix approach to build DSL`.

