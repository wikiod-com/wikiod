---
title: "Var, Val, and Def"
slug: "var-val-and-def"
draft: false
images: []
weight: 9893
type: docs
toc: true
---

As `val` are semantically static, they are initialized "in-place" wherever they appear in the code. This can produce surprising and undesirable behavior when used in abstract classes and traits.

For example, let's say we would like to make a trait called `PlusOne` that defines an increment operation on a wrapped `Int`. Since `Int`s are immutable, the value plus one is known at initialization and will never be changed afterwards, so semantically it's a `val`. However, defining it this way will produce an unexpected result.

    trait PlusOne {
        val i:Int

        val incr = i + 1
    }

    class IntWrapper(val i: Int) extends PlusOne

No matter what value `i` you construct `IntWrapper` with, calling `.incr` on the returned object will always return 1. This is because the val `incr` is initialized *in the trait*, before the extending class, and at that time `i` only has the default value of `0`. (In other conditions, it might be populated with `Nil`, `null`, or a similar default.)

The general rule, then, is to avoid using `val` on any value that depends on an abstract field. Instead, use `lazy val`, which does not evaluate until it is needed, or `def`, which evaluates every time it is called. Note however that if the `lazy val` is forced to evaluate by a `val` before initialization completes, the same error will occur. 

A fiddle (written in Scala-Js, but the same behavior applies) can be found [here.][1]


  [1]: http://www.scala-js-fiddle.com/gist/6013f97cf6052df5aa8961a4cb4ef2bc

## Var, Val, and Def
# var

A `var` is a reference variable, similar to variables in languages like Java. Different objects can be freely assigned to a `var`, so long as the given object has the same type that the `var` was declared with:

    scala> var x = 1
    x: Int = 1
    
    scala> x = 2
    x: Int = 2

    scala> x = "foo bar"
    <console>:12: error: type mismatch;
     found   : String("foo bar")
     required: Int
           x = "foo bar"
           ^
Note in the example above the type of the `var` was inferred by the compiler given the first value assignment.

# val

A `val` is a constant reference. Thus, a new object cannot be assigned to a `val` that has already been assigned.
    
    scala> val y = 1
    y: Int = 1
    
    scala> y = 2
    <console>:12: error: reassignment to val
           y = 2
             ^
However, the object that a `val` points to is *not* constant. That object may be modified:

    scala> val arr = new Array[Int](2)
    arr: Array[Int] = Array(0, 0)
    
    scala> arr(0) = 1
    
    scala> arr
    res1: Array[Int] = Array(1, 0)

# def

A `def` defines a method. A method cannot be re-assigned to.
    
    scala> def z = 1
    z: Int
    
    scala> z = 2
    <console>:12: error: value z_= is not a member of object $iw
           z = 2
           ^

In the above examples, `val y` and `def z` return the same value. However, a `def` is evaluated *when it is called*, whereas a `val` or `var` is evaluated *when it is assigned*. This can result in differing behavior when the definition has side effects:

    scala> val a = {println("Hi"); 1}
    Hi
    a: Int = 1

    scala> def b = {println("Hi"); 1}
    b: Int

    scala> a + 1
    res2: Int = 2
    
    scala> b + 1
    Hi
    res3: Int = 2

# Functions

Because functions are values, they can be assigned to `val`/`var`/`def`s. Everything else works in the same manner as above:

    scala> val x = (x: Int) => s"value=$x"
    x: Int => String = <function1>
    
    scala> var y = (x: Int) => s"value=$x"
    y: Int => String = <function1>
    
    scala> def z = (x: Int) => s"value=$x"
    z: Int => String
    
    scala> x(1)
    res0: String = value=1
    
    scala> y(2)
    res1: String = value=2
    
    scala> z(3)
    res2: String = value=3

## Lazy val
`lazy val` is a language feature where the initialization of a `val` is delayed until it is accessed for the first time. After that point, it acts just like a regular `val`.

To use it add the `lazy` keyword before `val`. For example, using the REPL:

    scala> lazy val foo = {
         |   println("Initializing")
         |   "my foo value"
         | }
    foo: String = <lazy>
    
    scala> val bar = {
         |   println("Initializing bar")
         |   "my bar value"
         | }
    Initializing bar
    bar: String = my bar value
    
    scala> foo
    Initializing
    res3: String = my foo value
    
    scala> bar
    res4: String = my bar value
    
    scala> foo
    res5: String = my foo value

This example demonstrates the execution order. When the `lazy val` is declared, all that is saved to the `foo` value is a lazy function call that hasn't been evaluated yet. When the regular `val` is set, we see the `println` call execute and the value is assigned to `bar`. When we evalute `foo` the first time we see `println` execute - but not when it's evaluated the second time. Similarly, when `bar` is evaluated we don't see `println` execute - only when it is declared.

# When To Use 'lazy'

1. **Initialization is computationally expensive and usage of `val` is rare.**

       lazy val tiresomeValue = {(1 to 1000000).filter(x => x % 113 == 0).sum}
       if (scala.util.Random.nextInt > 1000) {
         println(tiresomeValue)
       }

   `tiresomeValue` takes a long time to calculate, and it's not always used. Making it a `lazy val` saves unnecessary computation.

2. **Resolving cyclic dependencies**

   Let's look at an example with two objects that need to be declared at the same time during instantiation:

       object comicBook {
         def main(args:Array[String]): Unit = {
           gotham.hero.talk()
           gotham.villain.talk()
         }
       }
    
       class Superhero(val name: String) {
         lazy val toLockUp = gotham.villain
         def talk(): Unit = {
           println(s"I won't let you win ${toLockUp.name}!")
         }
       }

       class Supervillain(val name: String) {
         lazy val toKill = gotham.hero
         def talk(): Unit = {
           println(s"Let me loosen up Gotham a little bit ${toKill.name}!")
         }
       }

       object gotham {
         val hero: Superhero = new Superhero("Batman")
         val villain: Supervillain = new Supervillain("Joker")
       }

   Without the keyword `lazy`, the respective objects can not be members of an object. Execution of such a program would result in a `java.lang.NullPointerException`. By using `lazy`, the reference can be assigned before it is initialized, without fear of having an uninitialized value.

## Overloading Def
You can overload a `def` if the signature is different:

    def printValue(x: Int) {
      println(s"My value is an integer equal to $x")
    }
    
    def printValue(x: String) {
      println(s"My value is a string equal to '$x'")
    }
    
    printValue(1)  // prints "My value is an integer equal to 1"
    printValue("1") // prints "My value is a string equal to '1'"

This functions the same whether inside classes, traits, objects or not.

## Named Parameters
When invoking a `def`, parameters may be assigned explicitly by name. Doing so means they needn't be correctly ordered. For example, define `printUs()` as:

    // print out the three arguments in order.
    def printUs(one: String, two: String, three: String) = 
       println(s"$one, $two, $three")

Now it can be called in these ways (amongst others):

    printUs("one", "two", "three") 
    printUs(one="one", two="two", three="three")
    printUs("one", two="two", three="three")
    printUs(three="three", one="one", two="two") 

This results in `one, two, three` being printed in all cases.

If not all arguments are named, the first arguments are matched by order. No positional (non-named) argument may follow a named one:

    printUs("one", two="two", three="three") // prints 'one, two, three'
    printUs(two="two", three="three", "one") // fails to compile: 'positional after named argument'


