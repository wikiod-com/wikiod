---
title: "Type Classes"
slug: "type-classes"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

To avoid serialization problems, particularly in distributed environments (e.g. [Apache Spark](https://www.wikiod.com/docs/apache-spark)), it is a best practice to implement the `Serializable` trait for type class instances.

## Simple Type Class
A type class is simply a `trait` with one or more type parameters:

```
trait Show[A] {
  def show(a: A): String
}
```

Instead of extending a type class, an implicit instance of the type class is provided for each supported type. Placing these implementations in the companion object of the type class allows implicit resolution to work without any special imports:

```
object Show {
  implicit val intShow: Show[Int] = new Show {
    def show(x: Int): String = x.toString
  }

  implicit val dateShow: Show[java.util.Date] = new Show {
    def show(x: java.util.Date): String = x.getTime.toString
  }

  // ..etc
}
```
If you want to guarantee that a generic parameter passed to a function has an instance of a type class, use implicit parameters:

```
def log[A](a: A)(implicit showInstance: Show[A]): Unit = {
  println(showInstance.show(a))
}
```

You can also use a [context bound][1]:

```
def log[A: Show](a: A): Unit = {
  println(implicitly[Show[A]].show(a))
}
```

Call the above `log` method like any other method. It will fail to compile if an implicit `Show[A]` implementation can't be found for the `A` you pass to `log`

```
log(10) // prints: "10"
log(new java.util.Date(1469491668401L) // prints: "1469491668401"
log(List(1,2,3)) // fails to compile with
                 // could not find implicit value for evidence parameter of type Show[List[Int]]
```

This example implements the `Show` type class. This is a common type class used to convert arbitrary instances of arbitrary types into `String`s. Even though every object has a `toString` method, it's not always clear whether or not `toString` is defined in a useful way. With use of the `Show` type class, you can guarantee that anything passed to `log` has a well-defined conversion to `String`.


  [1]: http://docs.scala-lang.org/tutorials/FAQ/context-and-view-bounds.html

## Extending a Type Class
This example discusses extending the below type class.
```
trait Show[A] {
  def show: String
}
```
To make a class _you_ control (and is written in Scala) extend the type class, add an implicit to its companion object. Let us show how we can get the `Person` class from [this example](https://www.wikiod.com/scala/classes-and-objects#Constructors) to extend `Show`:
```
class Person(val fullName: String) {    
  def this(firstName: String, lastName: String) = this(s"$firstName $lastName")
}
```
We can make this class extend `Show` by adding an implicit to `Person`'s companion object:
```
object Person {
  implicit val personShow: Show[Person] = new Show {
    def show(p: Person): String = s"Person(${p.fullname})"
  }
}
```
A companion object _must be in the same file_ as the class, so you need both `class Person` and `object Person` in the same file.

To make a class you do not control, or is not written in Scala, extend the type class, add an implicit to the companion object of the type class, as shown in the [Simple Type Class](https://www.wikiod.com/scala/type-classes#Simple Type Class) example.

If you control neither the class nor the type class, create an implicit as above anywhere, and `import` it. Using the `log` method on the [Simple Type Class example](https://www.wikiod.com/scala/type-classes#Simple Type Class):
```
object MyShow {
  implicit val personShow: Show[Person] = new Show {
    def show(p: Person): String = s"Person(${p.fullname})"
  }
}

def logPeople(persons: Person*): Unit = {
  import MyShow.personShow
  persons foreach { p => log(p) }
}
```


## Add type class functions to types
Scala's implementation of type classes is rather verbose. One way to reduce the verbosity is to introduce so-called "Operation Classes". These classes will automatically wrap a variable/value when they are imported to extend functionality.

To illustrate this, let us first create a simple type class:

    // The mathematical definition of "Addable" is "Semigroup"
    trait Addable[A] {
      def add(x: A, y: A): A
    }

Next we will implement the trait (instantiate the type class):

    object Instances {

      // Instance for Int
      // Also called evidence object, meaning that this object saw that Int learned how to be added
      implicit object addableInt extends Addable[Int] {
        def add(x: Int, y: Int): Int = x + y
      }

      // Instance for String
      implicit object addableString extends Addable[String] {
        def add(x: String, y: String): String = x + y
      }

    }

At the moment it would be very cumbersome to use our Addable instances:

    import Instances._
    val three = addableInt.add(1,2)

We would rather just write write `1.add(2)`. Therefore we'll create an "Operation Class" (also called an "Ops Class") that will always wrap over a type that implements `Addable`.


    object Ops {
      implicit class AddableOps[A](self: A)(implicit A: Addable[A]) {
        def add(other: A): A = A.add(self, other)
      }
    }

Now we can use our new function `add` as if it was part of `Int` and `String`:

    object Main {

      import Instances._ // import evidence objects into this scope
      import Ops._       // import the wrappers

      def main(args: Array[String]): Unit = {

        println(1.add(5))
        println("mag".add("net"))
        // println(1.add(3.141)) // Fails because we didn't create an instance for Double

      }
    }

"Ops" classes can be created automatically by macros in [simulacrum](https://github.com/mpilquist/simulacrum) library:

    import simulacrum._

    @typeclass trait Addable[A] {
      @op("|+|") def add(x: A, y: A): A
    }


