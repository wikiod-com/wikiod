---
title: "Testing with ScalaTest"
slug: "testing-with-scalatest"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Spec Test Cheatsheet
**Setup**

The tests below uses these values for the examples.

    val helloWorld = "Hello World"
    val helloWorldCount = 1
    val helloWorldList = List("Hello World", "Bonjour Le Monde")
    def sayHello = throw new IllegalStateException("Hello World Exception")

**Type check**

To verify the type for a given `val`:

    helloWorld shouldBe a [String]

Note that the brackets here are used to get type `String`.

**Equal check**

To test equality:

    helloWorld shouldEqual "Hello World"
    helloWorld should === ("Hello World")
    helloWorldCount shouldEqual 1
    helloWorldCount shouldBe 1
    helloWorldList shouldEqual List("Hello World", "Bonjour Le Monde")
    helloWorldList === List("Hello World", "Bonjour Le Monde")

**Not Equal check**

To test inequality:

    helloWorld should not equal "Hello"
    helloWorld !== "Hello"
    helloWorldCount should not be 5
    helloWorldList should not equal List("Hello World")
    helloWorldList !== List("Hello World")
    helloWorldList should not be empty

**Length check**

To verify length and/or size:

    helloWorld should have length 11
    helloWorldList should have size 2

**Exceptions check**

To verify the type and message of an exception:

    val exception = the [java.lang.IllegalStateException] thrownBy {
      sayHello
    }
    exception.getClass shouldEqual classOf[java.lang.IllegalStateException]
    exception.getMessage should include ("Hello World")





## Hello World Spec Test
Create a testing class in the `src/test/scala` directory, in a file named `HelloWorldSpec.scala`. Put this inside the file:

    import org.scalatest.{FlatSpec, Matchers}
    
    class HelloWorldSpec extends FlatSpec with Matchers {
    
      "Hello World" should "not be an empty String" in {
          val helloWorld = "Hello World"
          helloWorld should not be ("")
      }
    }

- This example is making use of [`FlatSpec`][1] and [`Matchers`][2], which are part of the [ScalaTest library.][1]
- `FlatSpec` allows tests to be written in the **[Behavior-Driven Development (BDD)][3]** style. In this style, a sentence is used to describe the expected behavior of a given unit of code. The test confirms that the code adheres to that behavior. [See the documentation for additional information][4].


  [1]: http://doc.scalatest.org/3.0.0/index.html#org.scalatest.FlatSpec
  [2]: http://doc.scalatest.org/3.0.0/index.html#org.scalatest.Matchers
  [3]: https://en.wikipedia.org/wiki/Behavior-driven_development
  [4]: http://doc.scalatest.org/1.8/org/scalatest/FlatSpec.html

## Include the ScalaTest Library with SBT
Using SBT to [manage the library dependency][1], add this to `build.sbt`:

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

More details can be found [at the ScalaTest site][2].


  [1]: https://www.wikiod.com/sbt/dependencies
  [2]: http://www.scalatest.org/user_guide/using_scalatest_with_sbt

