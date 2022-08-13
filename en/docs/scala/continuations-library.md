---
title: "Continuations Library"
slug: "continuations-library"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Continuation passing style is a form of control flow that involves passing to functions the rest of the computation as a "continuation" argument. The function in question later invokes that continuation to continue program execution. One way to think of a continuation is as a closure. The Scala continuations library brings delimited continuations in the form of the primitives `shift`/`reset` to the language.

continuations library: https://github.com/scala/scala-continuations

## Syntax
- reset { ... } // Continuations extend up to the end of the enclosing reset block
- shift { ... } // Create a continuation stating from after the call, passing it to the closure
- A @cpsParam[B, C] // A computation that requires a function A => B to create a value of C
- @cps[A] // Alias for @cpsParam[A, A]
- @suspendable // Alias for @cpsParam[Unit, Unit]

`shift` and `reset` are primitive control flow structures, like `Int.+` is a primitive operation and `Long` is a primitive type. They are more primitive than either in that delimited continuations can actually be used to construct almost all control flow structures. They are not very useful "out-of-the-box", but they truly shine when they are used in libraries to create rich APIs.

Continuations and monads are also closely linked. Continuations can be made into the [continuation monad][], and monads are continuations because their `flatMap` operation takes a continuation as parameter.

[continuation monad]: http://blog.tmorris.net/posts/continuation-monad-in-scala/

## Callbacks are Continutations
    // Takes a callback and executes it with the read value
    def readFile(path: String)(callback: Try[String] => Unit): Unit = ???

    readFile(path) { _.flatMap { file1 =>
      readFile(path2) { _.foreach { file2 =>
        processFiles(file1, file2)
      }}
    }}

The function argument to `readFile` is a continuation, in that `readFile` invokes it to continue program execution after it has done its job.

In order to rein in what can easily become callback hell, we use the continuations library.

    reset { // Reset is a delimiter for continuations.
      for { // Since the callback hell is relegated to continuation library machinery.
            // a for-comprehension can be used
        file1 <- shift(readFile(path1)) // shift has type (((A => B) => C) => A)
        // We use it as (((Try[String] => Unit) => Unit) => Try[String])
        // It takes all the code that occurs after it is called, up to the end of reset, and
        // makes it into a closure of type (A => B).
        // The reason this works is that shift is actually faking its return type.
        // It only pretends to return A.
        // It actually passes that closure into its function parameter (readFile(path1) here),
        // And that function calls what it thinks is a normal callback with an A.
        // And through compiler magic shift "injects" that A into its own callsite.
        // So if readFile calls its callback with parameter Success("OK"),
        // the shift is replaced with that value and the code is executed until the end of reset,
        // and the return value of that is what the callback in readFile returns.
        // If readFile called its callback twice, then the shift would run this code twice too.
        // Since readFile returns Unit though, the type of the entire reset expression is Unit
        //
        // Think of shift as shifting all the code after it into a closure,
        // and reset as resetting all those shifts and ending the closures.
        file2 <- shift(readFile(path2))
      } processFiles(file1, file2)
    }

    // After compilation, shift and reset are transformed back into closures
    // The for comprehension first desugars to:
    reset {
      shift(readFile(path1)).flatMap { file1 => shift(readFile(path2)).foreach { file2 => processFiles(file1, file2) } }
    }
    // And then the callbacks are restored via CPS transformation
    readFile(path1) { _.flatMap { file1 => // We see how shift moves the code after it into a closure
      readFile(path2) { _.foreach { file2 =>
        processFiles(file1, file2)
      }}
    }}  // And we see how reset closes all those closures
    // And it looks just like the old version!

## Creating Functions That Take Continuations
If `shift` is called outside of a delimiting `reset` block, it can be used to create functions that themselves create continuations inside a `reset` block. It is important to note that `shift`'s type is not just `(((A => B) => C) => A)`, it is actually `(((A => B) => C) => (A @cpsParam[B, C]))`. That annotation marks where CPS transformations are needed. Functions that call `shift` without `reset` have their return type "infected" with that annotation.

Inside a `reset` block, a value of `A @cpsParam[B, C]` seems to have a value of `A`, though really it's just pretending. The continuation that is needed to complete the computation has type `A => B`, so the code following a method that returns this type must return `B`. `C` is the "real" return type, and after CPS transformation the function call has the type `C`.

Now, the example, taken from the [Scaladoc][] of the library

    val sessions = new HashMap[UUID, Int=>Unit]
    def ask(prompt: String): Int @suspendable = // alias for @cpsParam[Unit, Unit]. @cps[Unit] is also an alias. (@cps[A] = @cpsParam[A,A])
      shift {
        k: (Int => Unit) => {
          println(prompt)
          val id = uuidGen
          sessions += id -> k
        }
      }

    def go(): Unit = reset {
      println("Welcome!")
      val first = ask("Please give me a number") // Uses CPS just like shift
      val second = ask("Please enter another number")
      printf("The sum of your numbers is: %d\n", first + second)
    }

Here, `ask` will store the continuation into a map, and later some other code can retrieve that "session" and pass in the result of the query to the user. In this way, `go` can actually be using an asynchronous library while its code looks like normal imperative code.

[Scaladoc]: http://www.scala-lang.org/files/archive/api/2.11.8/scala-continuations-library/#scala.util.continuations.package


