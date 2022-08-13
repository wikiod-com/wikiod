---
title: "Getting started with Scala Language"
slug: "getting-started-with-scala-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World by extending App
    object HelloWorld extends App {
      println("Hello, world!")
    }

[Live demo](http://ideone.com/UgmX9y)

By extending the [`App`](http://www.scala-lang.org/api/2.11.8/scala/App.html) [trait][1], you can avoid defining an explicit `main` method. The entire body of the `HelloWorld` object is treated as "the main method".

<!-- if version [lt 2.11.0] -->
> ## Delayed Initialization
> Per [the official documentation](http://www.scala-lang.org/api/2.11.8/index.html#scala.App), `App` makes use of a feature called *Delayed Initialization*. This means that the object fields are initialized *after* the main method is called. 
<!-- end version if -->
<!-- if version [gte 2.11.0] -->
> ## Delayed Initialization
> Per [the official documentation](http://www.scala-lang.org/api/2.11.8/index.html#scala.App), `App` makes use of a feature called *[Delayed Initialization][2]*. This means that the object fields are initialized *after* the main method is called. 
>
> `DelayedInit` is now **deprecated** for general use, but is *still supported* for `App` as a special case. Support will continue until a replacement feature is decided upon and implemented.
<!-- end version if -->

To access command-line arguments when extending `App`, use `this.args`:

    object HelloWorld extends App {
      println("Hello World!")
      for {
        arg <- this.args
      } println(s"Arg=$arg")
    }

When using `App`, the body of the object will be executed as the `main` method, there is no need to override `main`.


  [1]: http://docs.scala-lang.org/tutorials/tour/traits.html
  [2]: http://www.scala-lang.org/api/current/#scala.DelayedInit

## Hello World by Defining a 'main' Method
Place this code in a file named `HelloWorld.scala`:

    object Hello {
      def main(args: Array[String]): Unit = {
        println("Hello World!")
      }
    }

[Live demo](http://ideone.com/7HYlEB)

To compile it to bytecode that is executable by the JVM:

    $ scalac HelloWorld.scala

To run it:

    $ scala Hello

When the Scala runtime loads the program, it looks for an object named `Hello` with a `main` method. The `main` method is the program entry point and is executed.

Note that, unlike Java, Scala has no requirement of naming objects or classes after the file they're in. Instead, the parameter `Hello` passed in the command `scala Hello` refers to the object to look for that contains the `main` method to be executed. It is perfectly possible to have multiple objects with main methods in the same `.scala` file.

The `args` array will contain the command-line arguments given to the program, if any. For instance, we can modify the program like this:

    object HelloWorld {
      def main(args: Array[String]): Unit = {
        println("Hello World!")
        for {
          arg <- args
        } println(s"Arg=$arg")
      }
    }

Compile it:

    $ scalac HelloWorld.scala

And then execute it:

    $ scala HelloWorld 1 2 3
    Hello World!
    Arg=1
    Arg=2
    Arg=3




## Hello World as a script
Scala can be used as a scripting language. To demonstrate, create `HelloWorld.scala` with the following content:

    println("Hello")

Execute it with the command-line interpreter (the `$` is the command line prompt):

    $ scala HelloWorld.scala
    Hello

If you omit `.scala` (such as if you simply typed `scala HelloWorld`) the runner will look for a compiled `.class` file with bytecode instead of compiling and then executing the script. 

> **Note:** If scala is used as a scripting language no package can be defined.

In operating systems utilizing `bash` or similar shell terminals, Scala scripts can be executed using a 'shell preamble'. Create a file named `HelloWorld.sh` and place the following as its content:

    #!/bin/sh
    exec scala "$0" "$@"
    !#
    println("Hello")

The parts between `#!` and `!#` is the 'shell preamble', and is interpreted as a bash script. The rest is Scala.

Once you have saved the above file, you must grant it 'executable' permissions. In the shell you can do this:

    $ chmod a+x HelloWorld.sh

(Note that this gives permission to everyone: [read about chmod][1] to learn how to set it for more specific sets of users.)

Now you can execute the script like this:

    $ ./HelloWorld.sh


  [1]: https://en.wikipedia.org/wiki/Chmod

## Scala Quicksheet

| Description | Code |
| ------ | ------ |
| [Assign immutable int value][1]  | `val x = 3`  |
| [Assign mutable int value][1]  | `var x = 3`  |
| [Assign immutable value with explicit type][1]  | `val x: Int = 27`  |
| [Assign lazily evaluated value][2] | `lazy val y = print("Sleeping in.")` |
| [Bind a function to a name][1] | `val f = (x: Int) => x * x` |
| [Bind a function to a name with explicit type][1] | `val f: Int => Int = (x: Int) => x * x` |
| [Define a method][1] | `def f(x: Int) = x * x` |
| [Define a method with explicit typing][1] | `def f(x: Int): Int = x * x` |
| [Define a class][3] | `class Hopper(someParam: Int) { ... }` |
| [Define an object][3] | `object Hopper(someParam: Int) { ... }` |
| [Define a trait][4] | `trait Grace { ... }` |
| [Get first element of sequence][5] | `Seq(1,2,3).head` |
| [If switch][6] | `val result = if(x > 0) "Positive!"`
| [Get all elements of sequence except first][5] | `Seq(1,2,3).tail` |
| [Loop through a list][7] | `for { x <- Seq(1,2,3) } print(x)` |
| [Nested Looping][8] | <code>for { <br/>&nbsp;&nbsp;x <- Seq(1,2,3)<br/>&nbsp;&nbsp;y <- Seq(4,5,6)<br/>} print(x + ":" + y)</code> |
| [For each list element execute function][9] | `List(1,2,3).foreach { println }`|
| Print to standard out | `print("Ada Lovelace")`  |
| [Sort a list alphanumerically][10] | `List('b','c','a').sorted` |


  [1]: https://www.wikiod.com/scala/var-val-and-def#Var, Val, and Def
  [2]: https://www.wikiod.com/scala/var-val-and-def#Lazy val
  [3]: https://www.wikiod.com/scala/classes-and-objects#Singleton & Companion Objects
  [4]: https://www.wikiod.com/scala/traits
  [5]: https://www.wikiod.com/scala/collections
  [6]: https://www.wikiod.com/scala/if-expressions#Basic If Expressions
  [7]: https://www.wikiod.com/scala/for-expressions
  [8]: https://www.wikiod.com/scala/for-expressions#Nested For Loop
  [9]: https://www.wikiod.com/scala/collections#Foreach
  [10]: https://www.wikiod.com/scala/collections#Sort A List

## Using the Scala REPL
When you execute `scala` in a terminal without additional parameters it opens up a [REPL][1] (Read-Eval-Print Loop) interpreter:

    nford:~ $ scala
    Welcome to Scala 2.11.8 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_66).
    Type in expressions for evaluation. Or try :help.
    
    scala> 

The REPL allows you to execute Scala in a worksheet fashion: the execution context is preserved and you can manually try out commands without having to build a whole program. For instance, by typing `val poem = "As halcyons we shall be"` would look like this:

    scala> val poem = "As halcyons we shall be"
    poem: String = As halcyons we shall be

Now we can print our `val`:

    scala> print(poem)
    As halcyons we shall be

Note that `val` is immutable and cannot be overwritten:

    scala> poem = "Brooding on the open sea"
    <console>:12: error: reassignment to val
           poem = "Brooding on the open sea"

But in the REPL you *can* redefine a `val` (which would cause an error in a normal Scala program, if it was done in the same scope):

    scala> val poem = "Brooding on the open sea"
    poem: String = Brooding on the open sea

For the remainder of your REPL session this newly defined variable will shadow the previously defined variable. REPLs are useful for quickly seeing how objects or other code works. All of Scala's features are available: you can define functions, classes, methods, etc. 


  [1]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop

