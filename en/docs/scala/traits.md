---
title: "Traits"
slug: "traits"
draft: false
images: []
weight: 9914
type: docs
toc: true
---

## Syntax
- trait ATrait { ... }
- class AClass (...) extends ATrait { ... }
- class AClass extends BClass with ATrait
- class AClass extends ATrait with BTrait
- class AClass extends ATrait with BTrait with CTrait
- class ATrait extends BTrait

## Solving the Diamond Problem
The [diamond problem][1], or multiple inheritance, is handled by Scala using Traits, which are similar to Java interfaces. Traits are more flexible than interfaces and can include implemented methods. This makes traits similar to [mixins][2] in other languages.

Scala does not support inheritance from multiple classes, but a user can extend multiple traits in a single class:

    trait traitA {
      def name = println("This is the 'grandparent' trait.")
    }
    
    trait traitB extends traitA {
      override def name = {
        println("B is a child of A.")
        super.name
      }
    
    }
    
    trait traitC extends traitA {
      override def name = {
        println("C is a child of A.")
        super.name
      }
    }
    
    object grandChild extends traitB with traitC
    
    grandChild.name

Here `grandChild` is inheriting from both `traitB` and `traitC`, which in turn both inherit from `traitA`. The output (below) also shows the order of precedence when resolving which method implementations are called first:

    C is a child of A. 
    B is a child of A. 
    This is the 'grandparent' trait.

Note that, when `super` is used to invoke methods in `class` or `trait`, [linearization][3] rule come into play to decide call hierarchy. Linearization order for `grandChild` will be:

> grandChild -> traitC -> traitB -> traitA -> AnyRef -> Any


----------

Below is another example:

    trait Printer {
      def print(msg : String) = println (msg)
    }
    
    trait DelimitWithHyphen extends Printer {
      override def print(msg : String) {
        println("-------------")
        super.print(msg)
      }
    }
    
    trait DelimitWithStar extends Printer  {
      override def print(msg : String) {
        println("*************")
        super.print(msg)
      }
    }
    
    class CustomPrinter extends Printer with DelimitWithHyphen with DelimitWithStar
    
    object TestPrinter{
      def main(args: Array[String]) {
        new CustomPrinter().print("Hello World!")
      }
    }

This program prints:

    *************
    -------------
    Hello World!

Linearization for `CustomPrinter` will be:

> CustomPrinter -> DelimitWithStar -> DelimitWithHyphen -> Printer ->
> AnyRef -> Any


  [1]: https://en.wikipedia.org/wiki/Multiple_inheritance
  [2]: https://en.wikipedia.org/wiki/Mixin
  [3]: https://www.wikiod.com/scala/traits#Linearization

## Stackable Modification with Traits
You can use traits to modify methods of a class, using traits in stackable fashion.

The following example shows how traits can be stacked. The ordering of the traits are important. Using different order of traits, different behavior is achieved.

    class Ball {
      def roll(ball : String) = println("Rolling : " + ball)
    }
    
    trait Red extends Ball {
      override def roll(ball : String) = super.roll("Red-" + ball)
    }
    
    trait Green extends Ball {
      override def roll(ball : String) = super.roll("Green-" + ball)
    }
    
    trait Shiny extends Ball {
      override def roll(ball : String) = super.roll("Shiny-" + ball)
    }
    
    object Balls {
      def main(args: Array[String]) {
        val ball1 = new Ball with Shiny with Red
        ball1.roll("Ball-1") // Rolling : Shiny-Red-Ball-1
    
        val ball2 = new Ball with Green with Shiny
        ball2.roll("Ball-2") // Rolling : Green-Shiny-Ball-2
      }
    }

Note that `super` is used to invoke `roll()` method in both the traits. Only in this way we can achieve stackable modification. In cases of stackable modification, method invocation order is determined by *[linearization rule][1]*.


  [1]: https://www.wikiod.com/scala/traits#Linearization

## Linearization
In case of [stackable modification][1], Scala arranges classes and traits in a linear order to determine method call hierarchy, which is known as *linearization*. The linearization rule is used *only* for those methods that involve method invocation via `super()`. Let's consider this by an example:

    class Shape {
      def paint (shape: String): Unit = {
        println(shape)
      }
    }
    
    trait Color extends Shape {
      abstract override def paint (shape : String) {
        super.paint(shape + "Color ")
      }
    }
    
    trait Blue extends Color {
      abstract override def paint (shape : String) {
        super.paint(shape + "with Blue ")
      }
    }
    
    trait Border extends Shape {
      abstract override def paint (shape : String) {
        super.paint(shape + "Border ")
      }
    }
    
    trait Dotted extends Border {
      abstract override def paint (shape : String) {
        super.paint(shape + "with Dotted ")
      }
    }
    
    class MyShape extends Shape with Dotted with Blue {
      override def paint (shape : String) {
        super.paint(shape)
      }
    }

Linearization happens from *back to front*. In this case, 

 1. First `Shape` will be linearized, which looks like:

    `Shape -> AnyRef -> Any`

 2. Then `Dotted` is linearized:

    `Dotted -> Border -> Shape -> AnyRef -> Any`

 3. Next in line is `Blue`. Normally `Blue`'s linearization will be:

    `Blue -> Color -> Shape -> AnyRef -> Any`

    because, in `MyShape`'s linearization until now (*Step 2*), `Shape -> AnyRef -> Any` has already appeared. Hence, it is ignored. Thus, the `Blue` linearization will be:

    `Blue -> Color -> Dotted -> Border -> Shape -> AnyRef -> Any`

 4. Finally, `Circle` will be added and final linearization order will be:
    > Circle -> Blue -> Color -> Dotted -> Border -> Shape -> AnyRef -> Any

This linearization order decides invocation order of methods when `super` is used in any class or trait. The first method implementation from the right is invoked, in the linearization order. If `new MyShape().paint("Circle ")` is executed, it will print:

    Circle with Blue Color with Dotted Border 

More information on linearization can be found [here][2]. 


  [1]: https://www.wikiod.com/scala/traits#Stackable Modification with Traits
  [2]: http://www.artima.com/pins1ed/traits.html#12.6

## Trait Basics
This is the most basic version of a trait in Scala.

    trait Identifiable {
      def getIdentifier: String
      def printIndentification(): Unit = println(getIdentifier)
    }
    
    case class Puppy(id: String, name: String) extends Identifiable {
      def getIdentifier: String = s"$name has id $id"
    }

Since no super class is declared for trait `Identifiable`, by default it extends from `AnyRef` class. Because no definition for `getIdentifier` is provided in `Identifiable`, the `Puppy` class must implement it. However, `Puppy` inherits the implementation of `printIdentification` from `Identifiable`.

In the REPL:

    val p = new Puppy("K9", "Rex")
    p.getIdentifier  // res0: String = Rex has id K9
    p.printIndentification()  // Rex has id K9


