---
title: "Higher Order Function"
slug: "higher-order-function"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

Scala goes to great lengths to treat methods and functions as syntactically identical. But under the hood, they are distinct concepts.

A method is executable code, and has no value representation.

A function is an actual object instance of type [`Function1`][1] (or a similar type of another arity). Its code is contained in its `apply` method. Effectively, it simply acts as a value that can be passed around.

Incidentally, the ability to treat functions as values *is* exactly what is meant by a language having support for higher-order functions. Function instances are Scala's approach to implementing this feature.

An actual higher-order function is a function that either takes a function value as an argument or returns a function value. But in Scala, as all operations are methods, it's more general to think of methods that receive or return function parameters. So [`map`][2], as defined on `Seq` might be thought of as a "higher-order function" due to its parameter being a function, but it is not literally a function; it is a method.

  [1]: http://www.scala-lang.org/api/current/scala/Function1.html
  [2]: http://www.scala-lang.org/api/current/index.html#scala.collection.Seq@map[B](f:A=%3EB):Seq[B]

## Using methods as function values
The Scala compiler will automatically convert methods into function values for the purpose of passing them into higher-order functions.

    object MyObject {
      def mapMethod(input: Int): String = {
        int.toString
      }
    }
    
    Seq(1, 2, 3).map(MyObject.mapMethod) // Seq("1", "2", "3")

In the example above, `MyObject.mapMethod` is not a function call, but instead is  passed to `map` as a value. Indeed, `map` *requires* a function value passed to it, as can be seen in it's signature. The signature for the `map` of a `List[A]` (a list of objects of type `A`) is:

    def map[B](f: (A) ⇒ B): List[B]

The `f: (A) => B` part indicates that the parameter to this method call is some function that takes an object of type `A` and returns an object of type `B`. `A` and `B` are arbitrarily defined. Returning to the first example, we can see that `mapMethod` takes an `Int` (which corresponds to `A`) and returns a `String` (which corresponds to `B`). Thus `mapMethod` is a valid function value to pass to `map`. We could rewrite the same code like this:

    Seq(1, 2, 3).map(x:Int => int.toString)

This inlines the function value, which may add clarity for simple functions.

## High Order Functions(Function as Parameter)
   A higher-order function, as opposed to a first-order function, can have one of three forms:
  - One or more of its parameters is a function, and it returns some value.
  - It returns a function, but none of its parameters is a function.
  - Both of the above: One or more of its parameters is a function, and it returns a function.
    
        object HOF {
            def main(args: Array[String]) {
            val list = List(("Srini","E"),("Subash","R"),("Ranjith","RK"),("Vicky","s"),("Sudhar","s"))
            //HOF
             val fullNameList= list.map(n => getFullName(n._1, n._2))

             }

            def getFullName(firstName: String, lastName: String): String = firstName + "." + lastName
            }

Here the map function takes a `getFullName(n._1,n._2)` function as a parameter. This is called **HOF (Higher order function).**






## Arguments lazy evaluation
Scala supports lazy evaluation for function arguments using notation: `def func(arg: => String)`. Such function argument might take regular `String` object or a higher order function with `String` return type. In second case, function argument would be evaluated on value access.

Please see the example:

    def calculateData: String = {
      print("Calculating expensive data! ")
      "some expensive data"
    }
    
    def dumbMediator(preconditions: Boolean, data: String): Option[String] = {
      print("Applying mediator")
      preconditions match {
        case true => Some(data)
        case false => None
      }
    }
    
    def smartMediator(preconditions: Boolean, data: => String): Option[String] = {
      print("Applying mediator")
      preconditions match {
        case true => Some(data)
        case false => None
      }
    }

    smartMediator(preconditions = false, calculateData)
    
    dumbMediator(preconditions = false, calculateData)

`smartMediator` call would return None value and print message `"Applying mediator"`. 

`dumbMediator` call would return None value and print message `"Calculating expensive data! Applying mediator"`.

Lazy evaluation might be extremely useful when you want to optimize an overhead of expensive arguments calculation.

