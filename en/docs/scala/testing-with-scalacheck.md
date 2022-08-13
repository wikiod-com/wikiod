---
title: "Testing with ScalaCheck"
slug: "testing-with-scalacheck"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

ScalaCheck is a library written in Scala and used for automated property-based testing of Scala or Java programs. ScalaCheck was originally inspired by the Haskell library QuickCheck, but has also ventured into its own.

ScalaCheck has no external dependencies other than the Scala runtime, and works great with sbt, the Scala build tool. It is also fully integrated in the test frameworks ScalaTest and specs2.

## Scalacheck with scalatest and error messages
Example of usage scalacheck with scalatest.
Below we have four tests:

 - "show pass example" - it passes
 - "show simple example without custom error message " - just failed message without details, `&&` boolean operator is used
 - "show example with error messages on argument" - error message on argument (`"argument" |:`) Props.all method is used instead of `&&`
 - "show example with error messages on command" - error message on command (`"command" |:`) Props.all method is used instead of `&&`


<!-- language: lang-scala -->


    import org.scalatest.prop.Checkers
    import org.scalatest.{Matchers, WordSpecLike}
    
    import org.scalacheck.Gen._
    import org.scalacheck.Prop._
    import org.scalacheck.Prop
    
    object Splitter {
      def splitLineByColon(message: String): (String, String) = {
        val (command, argument) = message.indexOf(":") match {
          case -1 =>
            (message, "")
          case x: Int =>
            (message.substring(0, x), message.substring(x + 1))
        }
        (command.trim, argument.trim)
      }
    
      def splitLineByColonWithBugOnCommand(message: String): (String, String) = {
        val (command, argument) = splitLineByColon(message)
        (command.trim + 2, argument.trim)
      }
    
      def splitLineByColonWithBugOnArgument(message: String): (String, String) = {
        val (command, argument) = splitLineByColon(message)
        (command.trim, argument.trim + 2)
      }
    }
    
    class ScalaCheckSpec extends WordSpecLike with Matchers with Checkers {
    
      private val COMMAND_LENGTH = 4
    
      "ScalaCheckSpec " should {

<!-- language: lang-scala -->
        "show pass example" in {
          check {
            Prop.forAll(listOfN(COMMAND_LENGTH, alphaChar), alphaStr) {
              (chars, expArgument) =>
                val expCommand = new String(chars.toArray)
                val line = s"$expCommand:$expArgument"
                val (c, p) = Splitter.splitLineByColon(line)
                Prop.all("command" |: c =? expCommand, "argument" |: expArgument =? p)
            }
    
          }
        }

<!-- language: lang-scala -->        
    "show simple example without custom error message " in {
      check {
        Prop.forAll(listOfN(COMMAND_LENGTH, alphaChar), alphaStr) {
          (chars, expArgument) =>
            val expCommand = new String(chars.toArray)
            val line = s"$expCommand:$expArgument"
            val (c, p) = Splitter.splitLineByColonWithBugOnArgument(line)
            c === expCommand && expArgument === p
        }

      }
    }

<!-- language: lang-scala -->        
    "show example with error messages on argument" in {
      check {
        Prop.forAll(listOfN(COMMAND_LENGTH, alphaChar), alphaStr) {
          (chars, expArgument) =>
            val expCommand = new String(chars.toArray)
            val line = s"$expCommand:$expArgument"
            val (c, p) = Splitter.splitLineByColonWithBugOnArgument(line)
            Prop.all("command" |: c =? expCommand, "argument" |: expArgument =? p)
        }

      }
    }
        


<!-- language: lang-scala -->    
    "show example with error messages on command" in {
      check {
        Prop.forAll(listOfN(COMMAND_LENGTH, alphaChar), alphaStr) {
          (chars, expArgument) =>
            val expCommand = new String(chars.toArray)
            val line = s"$expCommand:$expArgument"
            val (c, p) = Splitter.splitLineByColonWithBugOnCommand(line)
            Prop.all("command" |: c =? expCommand, "argument" |: expArgument =? p)
        }

      }
    }

The output (fragments):

    [info] - should show example // passed
    [info] - should show simple example without custom error message  *** FAILED ***
    [info]    (ScalaCheckSpec.scala:73)
    [info]     Falsified after 0 successful property evaluations.
    [info]     Location: (ScalaCheckSpec.scala:73)
    [info]     Occurred when passed generated values (
    [info]       arg0 = List(), // 3 shrinks
    [info]       arg1 = ""
    [info]     )
    [info] - should show example with error messages on argument *** FAILED ***
    [info]    (ScalaCheckSpec.scala:86)
    [info]     Falsified after 0 successful property evaluations.
    [info]     Location: (ScalaCheckSpec.scala:86)
    [info]     Occurred when passed generated values (
    [info]       arg0 = List(), // 3 shrinks
    [info]       arg1 = ""
    [info]     )
    [info]     Labels of failing property:
    [info]       Expected "" but got "2"
    [info]       argument
    [info] - should show example with error messages on command *** FAILED ***
    [info]    (ScalaCheckSpec.scala:99)
    [info]     Falsified after 0 successful property evaluations.
    [info]     Location: (ScalaCheckSpec.scala:99)
    [info]     Occurred when passed generated values (
    [info]       arg0 = List(), // 3 shrinks
    [info]       arg1 = ""
    [info]     )
    [info]     Labels of failing property:
    [info]       Expected "2" but got ""
    [info]       command



