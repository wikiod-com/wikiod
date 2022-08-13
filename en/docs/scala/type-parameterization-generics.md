---
title: "Type Parameterization (Generics)"
slug: "type-parameterization-generics"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Parameterized Methods
The return type of a method can depend on the *type* of the parameter. In this example, `x` is the parameter, `A` is the *type* of `x`, which is known as the *type parameter*.

    def f[A](x: A): A = x

    f(1)         // 1
    f("two")     // "two"
    f[Float](3)  // 3.0F

Scala will use [type inference][1] to determine the return type, which constrains what methods may be called on the parameter. Thus, care must be taken: the following is a compile-time error because `*` is not defined for every type `A`:

    def g[A](x: A): A = 2 * x  // Won't compile


  [1]: https://www.wikiod.com/scala/type-inference

## Generic collection
## Defining the list of Ints ##

```
trait IntList { ... }

class Cons(val head: Int, val tail: IntList) extends IntList { ... }

class Nil extends IntList { ... }
```

but what if we need to define the list of Boolean, Double etc?

## Defining generic list

```
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: [T], val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head: Nothing = throw NoSuchElementException("Nil.head")

  def tail: Nothing = throw NoSuchElementException("Nil.tail")
}
```

## The Option type
A nice example of a parameterized type is the [Option type](https://github.com/scala/scala/blob/2.12.x/src/library/scala/Option.scala). It is essentially just the following definition (with several more methods defined on the type):

    sealed abstract class Option[+A] {
      def isEmpty: Boolean
      def get: A

      final def fold[B](ifEmpty: => B)(f: A => B): B =
        if (isEmpty) ifEmpty else f(this.get)

      // lots of methods...
    }

    case class Some[A](value: A) extends Option[A] {
      def isEmpty = false
      def get = value
    }

    case object None extends Option[Nothing] {
      def isEmpty = true
      def get = throw new NoSuchElementException("None.get")
    }

We can also see that this has a parameterized method, `fold`, which returns something of type `B`.

