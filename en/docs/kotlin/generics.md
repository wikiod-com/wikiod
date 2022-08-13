---
title: "Generics"
slug: "generics"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

A List can hold numbers, words or really anything.
That's why we call the List *generic*.

Generics are basically used to define which types a class can hold and which type an object currently holds.

## Syntax
 - class *ClassName*<***TypeName***>
 - class *ClassName*<*>
 - *ClassName*<in ***UpperBound***>
 - *ClassName*<out ***LowerBound***>
 - class *Name*<***TypeName***:***UpperBound***>

## Parameters
| Parameter      | Details                        |
| -------------- | ------------------------------ |
| **TypeName**   | Type Name of generic parameter |
| **UpperBound** | Covariant Type                 |
| **LowerBound** | Contravariant Type             |
| ClassName      | Name of the class              |

# Implied Upper Bound is Nullable

In Kotlin Generics, the upper bound of type parameter `T` would be `Any?`.  Therefore for this class:

<!-- language-all: kotlin -->

    class Consumer<T>

The type parameter `T` is really `T: Any?`.  To make a non-nullable upper bound, explicitly specific `T: Any`.  For example:

    class Consumer<T: Any>

## Declaration-site variance
[Declaration-site variance][2] can be thought of as declaration of use-site variance once and for all the use-sites.

<!-- language-all: kotlin -->

      class Consumer<in T> { fun consume(t: T) { ... } }

      fun charSequencesConsumer() : Consumer<CharSequence>() = ...

      val stringConsumer : Consumer<String> = charSequenceConsumer() // OK since in-projection
      val anyConsumer : Consumer<Any> = charSequenceConsumer() // Error, Any cannot be passed
      
      val outConsumer : Consumer<out CharSequence> = ... // Error, T is `in`-parameter

  Widespread examples of declaration-site variance are `List<out T>`, which is immutable so that `T` only appears as the return value type, and `Comparator<in T>`, which only receives `T` as argument.


  [1]: https://kotlinlang.org/docs/reference/generics.html#use-site-variance-type-projections
  [2]: https://kotlinlang.org/docs/reference/generics.html#declaration-site-variance

## Use-site variance
[Use-site variance][1] is similar to Java wildcards:

Out-projection:

<!-- language-all: kotlin -->

      val takeList : MutableList<out SomeType> = ... // Java: List<? extends SomeType>

      val takenValue : SomeType = takeList[0] // OK, since upper bound is SomeType

      takeList.add(takenValue) // Error, lower bound for generic is not specified

In-projection:

      val putList : MutableList<in SomeType> = ... // Java: List<? super SomeType>
      
      val valueToPut : SomeType = ...
      putList.add(valueToPut) // OK, since lower bound is SomeType

      putList[0] // This expression has type Any, since no upper bound is specified

Star-projection

      val starList : MutableList<*> = ... // Java: List<?>

      starList[0] // This expression has type Any, since no upper bound is specified
      starList.add(someValue) // Error, lower bound for generic is not specified

**See also:**
* [Variant Generics](https://kotlinlang.org/docs/reference/java-to-kotlin-interop.html#variant-generics) interoperability when calling Kotlin from Java.

  [1]: https://kotlinlang.org/docs/reference/generics.html#use-site-variance-type-projections

