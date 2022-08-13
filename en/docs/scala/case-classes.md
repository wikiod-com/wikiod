---
title: "Case Classes"
slug: "case-classes"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Syntax
- case class Foo() // Case classes with no parameters must have an empty list
- case class Foo(a1: A1, ..., aN: AN) // Create a case class with fields a1 ... aN
- case object Bar // Create a singleton case class


## Generated Code Artifacts
The `case` modifier causes the Scala compiler to automatically generate common boilerplate code for the class. Implementing this code manually is tedious and a source of errors. The following case class definition:

    case class Person(name: String, age: Int)

... will have the following code automatically generated:

    class Person(val name: String, val age: Int)
      extends Product with Serializable
    {
      def copy(name: String = this.name, age: Int = this.age): Person =
        new Person(name, age)

      def productArity: Int = 2

      def productElement(i: Int): Any = i match {
        case 0 => name
        case 1 => age
        case _ => throw new IndexOutOfBoundsException(i.toString)
      }

      def productIterator: Iterator[Any] =
        scala.runtime.ScalaRunTime.typedProductIterator(this)

      def productPrefix: String = "Person"

      def canEqual(obj: Any): Boolean = obj.isInstanceOf[Person]

      override def hashCode(): Int = scala.runtime.ScalaRunTime._hashCode(this)

      override def equals(obj: Any): Boolean = this.eq(obj) || obj match {
        case that: Person => this.name == that.name && this.age == that.age
        case _ => false
      }

      override def toString: String =
        scala.runtime.ScalaRunTime._toString(this)
    }

The `case` modifier also generates a companion object:

    object Person extends AbstractFunction2[String, Int, Person] with Serializable {
      def apply(name: String, age: Int): Person = new Person(name, age)

      def unapply(p: Person): Option[(String, Int)] =
        if(p == null) None else Some((p.name, p.age))
    }

When applied to an `object`, the `case` modifier has similar (albeit less dramatic) effects. Here the primary gains are a `toString` implementation and a `hashCode` value that is consistent across processes. Note that case objects (correctly) use reference equality:

    object Foo extends Product with Serializable {
      def productArity: Int = 0

      def productIterator: Iterator[Any] =
        scala.runtime.ScalaRunTime.typedProductIterator(this)

      def productElement(i: Int): Any =
        throw new IndexOutOfBoundsException(i.toString)

      def productPrefix: String = "Foo"

      def canEqual(obj: Any): Boolean = obj.isInstanceOf[this.type]

      override def hashCode(): Int = 70822 // "Foo".hashCode()

      override def toString: String = "Foo"
    }

It is still possible to manually implement methods that would otherwise be provided by the `case` modifier in both the class itself and its companion object.



## Case Class Basics
In comparison to regular classes – case classes notation provides several benefits:
- All constructor arguments are `public` and can be accessed on initialized objects (normally this is not the case, as demonstrated here):

      case class Dog1(age: Int)
      val x = Dog1(18)
      println(x.age) // 18 (success!)

      class Dog2(age: Int)
      val x = new Dog2(18)
      println(x.age) // Error: "value age is not a member of Dog2"

- It provides an implementation for the following methods: `toString`, `equals`, `hashCode` (based on properties), `copy`, `apply` and `unapply`:

      case class Dog(age: Int)
      val d1 = Dog(10)
      val d2 = d1.copy(age = 15)

- It provides a convenient mechanism for pattern matching:

      sealed trait Animal // `sealed` modifier allows inheritance within current build-unit only
      case class Dog(age: Int) extends Animal
      case class Cat(owner: String) extends Animal
      val x: Animal = Dog(18)
      x match {
          case Dog(x) => println(s"It's a $x years old dog.")
          case Cat(x) => println(s"This cat belongs to $x.")
      }


## Case Class Equality
One feature provided for free by case classes is an auto-generated `equals` method that checks the value equality of all individual member fields instead of just checking the reference equality of the objects.

With ordinary classes:

    class Foo(val i: Int)
    val a = new Foo(3)
    val b = new Foo(3)
    println(a == b)// "false" because they are different objects

With case classes:

    case class Foo(i: Int)
    val a = Foo(3)
    val b = Foo(3)
    println(a == b)// "true" because their members have the same value

## Case Classes and Immutabilty
The Scala compiler prefixes every argument in the parameter list by default with `val`. This means that, by default, case classes are immutable. Each parameter is given an accessor method, but there are no mutator methods. For example:


    case class Foo(i: Int)
    
    val fooInstance = Foo(1)
    val j = fooInstance.i       // get
    fooInstance.i = 2           // compile-time exception (mutation: reassignment to val)

Declaring a parameter in a case class as `var` overrides the default behavior and makes the case class mutable:

    case class Bar(var i: Int)
    
    val barInstance = Bar(1)
    val j = barInstance.i       // get
    barInstance.i = 2           // set

Another instance when a case class is 'mutable' is when the value in the case class is mutable:

    import scala.collection._
    
    case class Bar(m: mutable.Map[Int, Int])
    
    val barInstance = Bar(mutable.Map(1 -> 2))
    barInstance.m.update(1, 3)                  // mutate m
    barInstance                                 // Bar(Map(1 -> 3)

Note that the 'mutation' that is occurring here is in the map that `m` points to, not to `m` itself. Thus, if some other object had `m` as a member, it would see the change as well. Note how in the following example changing `instanceA` also changes `instanceB`:

    import scala.collection.mutable
    
    case class Bar(m: mutable.Map[Int, Int])
    
    val m = mutable.Map(1 ->2)
    val barInstanceA = Bar(m)
    val barInstanceB = Bar(m)
    barInstanceA.m.update(1,3)
    barInstanceA  // Bar = Bar(Map(1 -> 3))
    barInstanceB  // Bar = Bar(Map(1 -> 3))
    m  // scala.collection.mutable.Map[Int,Int] = Map(1 -> 3)

## Create a Copy of an Object with Certain Changes
Case classes provide a `copy` method that creates a new object that shares the same fields as the old one, with certain changes.

We can use this feature to create a new object from a previous one that has some of the same characteristics. This simple case class to demonstrates this feature:


    case class Person(firstName: String, lastName: String, grade: String, subject: String)
    val putu = Person("Putu", "Kevin", "A1", "Math")
    val mark = putu.copy(firstName = "Ketut", lastName = "Mark")
    // mark: People = People(Ketut,Mark,A1,Math)

In this example we can see that the two objects share similar characteristics (`grade = A1`, `subject = Math`), except where they have been specified in the copy (`firstName` and `lastName`).

## Single Element Case Classes for Type Safety
In order to achieve type safety sometimes we want to avoid the use of primitive types on our domain. For instance, imagine a `Person` with a `name`. Typically, we would encode the `name` as a `String`. However, it would not be hard to mix a `String` representing a `Person`'s `name` with a `String` representing an error message:

    def logError(message: ErrorMessage): Unit = ???
    case class Person(name: String)
    val maybeName: Either[String, String] = ??? // Left is error, Right is name
    maybeName.foreach(logError) // But that won't stop me from logging the name as an error!

To avoid such pitfalls you can encode the data like this:

    case class PersonName(value: String)
    case class ErrorMessage(value: String)
    case class Person(name: PersonName)

and now our code will not compile if we mix `PersonName` with `ErrorMessage`, or even an ordinary `String`.

    val maybeName: Either[ErrorMessage, PersonName] = ???
    maybeName.foreach(reportError) // ERROR: tried to pass PersonName; ErrorMessage expected
    maybeName.swap.foreach(reportError) // OK

But this incurs a small runtime overhead as we now have to box/unbox `String`s to/from their `PersonName` containers. In order to avoid this, one can make `PersonName` and `ErrorMessage` value classes:

    case class PersonName(val value: String) extends AnyVal
    case class ErrorMessage(val value: String) extends AnyVal

