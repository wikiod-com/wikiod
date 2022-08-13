---
title: "Type Variance"
slug: "type-variance"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Covariance
The `+` symbol marks a type parameter as *covariant* - here we say that "`Producer` is covariant on `A`":

    trait Producer[+A] {
      def produce: A
    }

A covariant type parameter can be thought of as an "output" type. Marking `A` as covariant asserts that `Producer[X] <: Producer[Y]` provided that `X <: Y`. For example, a `Producer[Cat]` is a valid `Producer[Animal]`, as all produced cats are also valid animals.

A covariant type parameter cannot appear in contravariant (input) position. The following example will not compile as we are asserting that `Co[Cat] <: Co[Animal]`, but `Co[Cat]` has `def handle(a: Cat): Unit` which cannot handle any `Animal` as required by `Co[Animal]`!

    trait Co[+A] {
      def produce: A
      def handle(a: A): Unit
    }

One approach to dealing with this restriction is to use type parameters bounded by the covariant type parameter. In the following example, we know that `B` is a supertype of `A`. Therefore given `Option[X] <: Option[Y]` for `X <: Y`, we know that `Option[X]`'s `def getOrElse[B >: X](b: => B): B` can accept any supertype of `X` - which includes the supertypes of `Y` as required by `Option[Y]`:

    trait Option[+A] {
      def getOrElse[B >: A](b: => B): B
    }

  

## Contravariance
The `-` symbol marks a type parameter as *contravariant* - here we say that "`Handler` is contravariant on `A`":

    trait Handler[-A] {
      def handle(a: A): Unit
    }

A contravariant type parameter can be thought of as an "input" type. Marking `A` as contravariant asserts that `Handler[X] <: Handler[Y]` provided that `X >: Y`. For example a `Handler[Animal]` is a valid `Handler[Cat]`, as a `Handler[Animal]` must also handle cats.

A contravariant type parameter cannot appear in covariant (output) position. The following example will not compile as we are asserting that a `Contra[Animal] <: Contra[Cat]`, however a `Contra[Animal]` has `def produce: Animal` which is not guaranteed to produce cats as required by `Contra[Cat]`!

    trait Contra[-A] {
       def handle(a: A): Unit
       def produce: A
    }

Beware however: for the purposes of overloading resolution, contravariance also counterintuitively inverts the specificity of a type on the contravariant type parameter - `Handler[Animal]` is considered to be "more specific" than `Handler[Cat]`.  

As it is not possible to overload methods on type parameters, this behavior generally only becomes problematic when resolving implicit arguments. In the following example `ofCat` will never be used, as the return type of `ofAnimal` is more specific:

    implicit def ofAnimal: Handler[Animal] = ???
    implicit def ofCat: Handler[Cat] = ???

    implicitly[Handler[Cat]].handle(new Cat)

This behavior is currently slated to [change in dotty][1], and is why (as an example) `scala.math.Ordering` is invariant on its type parameter `T`. One workaround is to make your typeclass invariant, and type-parametrize the implicit definition in the event that you want it to apply to subclasses of a given type:

    trait Person
    object Person {
      implicit def ordering[A <: Person]: Ordering[A] = ???
    }

[1]: https://github.com/lampepfl/dotty/commit/89540268e6c49fb92b9ca61249e46bb59981bf5a

## Invariance
By default all type parameters are invariant - given `trait A[B]`, we say that "`A` is invariant on `B`". This means that given two parametrizations `A[Cat]` and `A[Animal]`, we assert no sub/superclass relationship between these two types - it does not hold that `A[Cat] <: A[Animal]` nor that `A[Cat] >: A[Animal]` regardless of the relationship between `Cat` and `Animal`.

Variance annotations provide us with a means of declaring such a relationship, and imposes rules on the usage of type parameters so that the relationship remains valid.

## Covariance of a collection


## Covariance on an invariant trait
There is also a way to have a single method accept a covariant argument, instead of having the whole trait covariant. This may be necessary because you would like to use `T` in a contravariant position, but still have it covariant.

    trait LocalVariance[T]{
      /// ??? throws a NotImplementedError
      def produce: T = ???
      // the implicit evidence provided by the compiler confirms that S is a
      // subtype of T.
      def handle[S](s: S)(implicit evidence: S <:< T) = {
        // and we can use the evidence to convert s into t.
        val t: T = evidence(s)
        ???
      }
    }

    trait A {}
    trait B extends A {}

    object Test {
      val lv = new LocalVariance[A] {}

      // now we can pass a B instead of an A.
      lv.handle(new B {})
    }


