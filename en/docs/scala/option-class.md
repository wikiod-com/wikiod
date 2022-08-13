---
title: "Option Class"
slug: "option-class"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

## Syntax
-   class Some\[+T](value: T) extends Option\[T]
-   object None extends Option\[Nothing]
-   Option\[T](value: T)
    
    Constructor to create either a `Some(value)` or `None` as appropriate for the value provided.

## Using Option Instead of Null
In Java (and other languages), using `null` is a common way of indicating that there is no value attached to a reference variable. In Scala, using `Option` is preferred over using `null`. `Option` wraps values that *might* be `null`.

`None` is a subclass of `Option` wrapping a null reference. `Some` is a subclass of `Option` wrapping a non-null reference.

Wrapping a reference is easy:

```
val nothing = Option(null) // None
val something = Option("Aren't options cool?") // Some("Aren't options cool?")
```

This is typical code when calling a Java library that might return a null reference:

```
val resource = Option(JavaLib.getResource())
// if null, then resource = None
// else resource = Some(resource)
```

If `getResource()` returns a `null` value, `resource` will be a `None` object. Otherwise it will be a `Some(resource)` object. The preferred way to handle an `Option` is using higher order functions available within the `Option` type. For example if you want to check if your value is not `None` (similar to checking if `value == null`), you would use the `isDefined` function:

```
val resource: Option[Resource] = Option(JavaLib.getResource())
if (resource.isDefined) {  // resource is `Some(_)` type
  val r: Resource = resource.get
  r.connect()
}
```

Similarly, to check for a `null` reference you can do this:
```
val resource: Option[Resource] = Option(JavaLib.getResource())
if (resource.isEmpty) { // resource is `None` type.
  System.out.println("Resource is empty! Cannot connect.")
}
```

It is preferred that you treat conditional execution on the wrapped value of an `Option` (without using the 'exceptional' `Option.get` method) by treating the `Option` as a monad and using `foreach`:

```
val resource: Option[Resource] = Option(JavaLib.getResource())
resource foreach (r => r.connect())
// if r is defined, then r.connect() is run
// if r is empty, then it does nothing
```

If a `Resource` instance is required (versus an `Option[Resource]` instance), you can still use `Option` to protect against null values. Here the `getOrElse` method provides a default value:

```
lazy val defaultResource = new Resource()
val resource: Resource = Option(JavaLib.getResource()).getOrElse(defaultResource)

```

Java code won't readily handle Scala's `Option`, so when passing values to Java code it is good form to unwrap an `Option`, passing `null` or a sensible default where appropriate:

```
val resource: Option[Resource] = ???
JavaLib.sendResource(resource.orNull)
JavaLib.sendResource(resource.getOrElse(defaultResource)) // 
```

## Basics
An `Option` is a data structure that contains either a single value, or no value at all. An `Option` can be thought of as collections of zero or one elements.

Option is an abstract class with two children: `Some` and `None`.

`Some` contains a single value, and `None` contains no value.

`Option` is useful in expressions that would otherwise use `null` to represent the lack of a concrete value. This protects against a `NullPointerException`, and allows the composition of many expressions that might not return a value using combinators such as `Map`, `FlatMap`, etc.

## Example with Map ##

```
val countries = Map(
  "USA" -> "Washington",
  "UK" -> "London",
  "Germany" -> "Berlin",
  "Netherlands" -> "Amsterdam",
  "Japan" -> "Tokyo"
)
    
println(countries.get("USA")) // Some(Washington)
println(countries.get("France")) // None
println(countries.get("USA").get) // Washington
println(countries.get("France").get) // Error: NoSuchElementException
println(countries.get("USA").getOrElse("Nope")) // Washington
println(countries.get("France").getOrElse("Nope")) // Nope
```

`Option[A]` is **sealed** and thus cannot be extended. Therefore it's semantics are stable and can be relied on.

## Options as Collections
`Option`s have some useful higher-order functions that can be easily understood by viewing options as _collections with zero or one items_ - where `None` behaves like the empty collection, and `Some(x)` behaves like a collection with a single item, `x`. 

    val option: Option[String] = ???

    option.map(_.trim) // None if option is None, Some(s.trim) if Some(s)
    option.foreach(println) // prints the string if it exists, does nothing otherwise
    option.forall(_.length > 4) // true if None or if Some(s) and s.length > 4
    option.exists(_.length > 4) // true if Some(s) and s.length > 4
    option.toList // returns an actual list

## Options in for comprehensions
`Option`s have a `flatMap` method. This means they can be used in a for comprehension. In this way we can lift regular functions to work on `Option`s without having to redefine them.

    val firstOption: Option[Int] = Option(1)
    val secondOption: Option[Int] = Option(2)
    
    val myResult = for {
      firstValue <- firstOption
      secondValue <- secondOption
    } yield firstValue + secondValue
    // myResult: Option[Int] = Some(3)

When one of the values is a `None` the ending result of the calculation will be `None` as well.

    val firstOption: Option[Int] = Option(1)
    val secondOption: Option[Int] = None
    
    val myResult = for {
      firstValue <- firstOption
      secondValue <- secondOption
    } yield firstValue + secondValue
    // myResult: Option[Int] = None

Note: this pattern extends more generally for concepts called `Monad`s. (More information should be available on pages relating to for comprehensions and `Monad`s)

In general it is not possible to mix different monads in a for comprehension. But since `Option` can be easily converted to an `Iterable`, we can easily mix `Option`s and `Iterable`s by calling the `.toIterable` method.

    val option: Option[Int] = Option(1)
    val iterable: Iterable[Int] = Iterable(2, 3, 4, 5)

    // does NOT compile since we cannot mix Monads in a for comprehension
    // val myResult = for {
    //   optionValue <- option
    //   iterableValue <- iterable
    //} yield optionValue + iterableValue

    // It does compile when adding a .toIterable on the option
    val myResult = for {
      optionValue <- option.toIterable
      iterableValue <- iterable
    } yield optionValue + iterableValue
    // myResult: Iterable[Int] = List(2, 3, 4, 5)

A small note: if we had defined our for comprehension the other way around the for comprehension would compile since our option would be converted implicitly. For that reason it is useful to always add this `.toIterable` (or corresponding function depending on which collection you are using) for consistency.

