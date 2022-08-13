---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9900
type: docs
toc: true
---

Scala has first-class functions.

# Difference between functions and methods:

A function is not a method in Scala: functions are a value, and may be assigned as such. Methods (created using `def`), on the other hand, must belong to a class, trait or object.

- Functions are compiled to a class extending a trait (such as `Function1`) at compile-time, and are instantiated to a value at runtime. Methods, on the other hand, are members of their class, trait or object, and do not exist outside of that.
- A method may be converted to a function, but a function cannot be converted to a method.
- Methods can have type parameterization, whereas functions do not.
- Methods can have parameter default values, whereas functions can not.

## Anonymous Functions
Anonymous functions are functions that are defined but not assigned a name. 

The following is an anonymous function that takes in two integers and returns the sum.

    (x: Int, y: Int) => x + y

The resultant expression can be assigned to a `val`:

    val sum = (x: Int, y: Int) => x + y

Anonymous functions are primarily used as arguments to other functions. For instance, the `map` function on a collection expects another function as its argument:

    // Returns Seq("FOO", "BAR", "QUX")
    Seq("Foo", "Bar", "Qux").map((x: String) => x.toUpperCase)

The types of the arguments of the anonymous function can be omitted: the types are [inferred automatically][1]:

    Seq("Foo", "Bar", "Qux").map((x) => x.toUpperCase)

If there is just one argument, the parentheses around that argument can be omitted:

    Seq("Foo", "Bar", "Qux").map(x => x.toUpperCase)

Underscores shorthand
===========

There is an even shorter syntax that doesn't require names for the arguments. The above snippet can be written:

    Seq("Foo", "Bar", "Qux").map(_.toUpperCase)

`_` represents the anonymous function arguments positionally. With an anonymous function that has multiple parameters, each occurrence of `_` will refer to a different argument. For instance, the two following expressions are equivalent:

    // Returns "FooBarQux" in both cases
    Seq("Foo", "Bar", "Qux").reduce((s1, s2) => s1 + s2)
    Seq("Foo", "Bar", "Qux").reduce(_ + _)

When using this shorthand, any argument represented by the positional `_` can only be referenced a single time and in the same order.

Anonymous Functions with No Parameters
===========

To create a value for an anonymous function that does not take parameters, leave the parameter list blank:

    val sayHello = () => println("hello")

  [1]: https://www.wikiod.com/scala/type-inference

## Composition
Function composition allows for two functions to operate and be viewed as a single function. Expressed in mathematical terms, given a function `f(x)` and a function `g(x)`, the function `h(x) = f(g(x))`.

When a function is compiled, it is compiled to a type related to [`Function1`][1]. Scala provides two methods in the `Function1` implementation related to composition: `andThen` and `compose`. The `compose` method fits with the above mathematical definition like so:

    val f: B => C = ...
    val g: A => B = ...
    
    val h: A => C = f compose g

The `andThen` (think `h(x) = g(f(x))`) has a more 'DSL-like' feeling:

    val f: A => B = ...
    val g: B => C = ...
    
    val h: A => C = f andThen g

A new anonymous function is allocated with that is closed over `f` and `g`. This function is bound to the new function `h` in both cases. 

    def andThen(g: B => C): A => C = new (A => C){
      def apply(x: A) = g(self(x))
    }

If either `f` or `g` works via a side-effect, then calling `h` will cause all side-effects of `f` and `g` to happen in the order. The same is true of any mutable state changes.

  [1]: http://www.scala-lang.org/api/rc2/scala/Function1.html

## Relationship to PartialFunctions
    trait PartialFunction[-A, +B] extends (A => B)

Every single-argument `PartialFunction` is also a `Function1`. This is counter-intuitive in a formal mathematical sense, but better fits object oriented design. For this reason `Function1` does not have to provide a constant `true` `isDefinedAt` method.

To define a partial function (which is also a function), use the following syntax:

    { case i: Int => i + 1 } // or equivalently { case i: Int ⇒ i + 1 }

For further details, take a look at [PartialFunctions](https://www.wikiod.com/scala/partial-functions).




