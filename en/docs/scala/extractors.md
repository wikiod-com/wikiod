---
title: "Extractors"
slug: "extractors"
draft: false
images: []
weight: 9854
type: docs
toc: true
---

## Syntax
 - val extractor(extractedValue1, _ /* ignored second extracted value */) = valueToBeExtracted
 - valueToBeExtracted match { case extractor(extractedValue1, _) => ???}
 - val (tuple1, tuple2, tuple3) = tupleWith3Elements
 - object Foo { def unapply(foo: Foo): Option[String] = Some(foo.x); }

## Case Class Extractors
A [case class][1] is a class with a lot of standard boilerplate code automatically included. One benefit of this is that Scala makes it easy to use extractors with case classes.

```
case class Person(name: String, age: Int)  // Define the case class
val p = Person("Paola", 42)  // Instantiate a value with the case class type

val Person(n, a) = p  // Extract values n and a
// n: String = Paola
// a: Int = 42
```

At this juncture, both `n` and `a` are `val`s in the program and can be accessed as such: they are said to have been 'extracted' from p. Continuing:

```
val p2 = Person("Angela", 1337)

val List(Person(n1, a1), Person(_, a2)) = List(p, p2)
// n1: String = Paola
// a1: Int = 42
// a2: Int = 1337
```

Here we see two important things:
- Extraction can happen at 'deep' levels: properties of nested objects can be extracted.
- Not all elements *need* to be extracted. The wildcard `_` character indicates that that particular value can be anything, and is ignored. No `val` is created.

In particular, this can make matching over collections easy:

    val ls = List(p1, p2, p3)  // List of Person objects
    ls.map(person => person match {
      case Person(n, a) => println("%s is %d years old".format(n, a))
    })

Here, we have code that uses the extractor to explicitly check that `person` is a `Person` object and immediately pull out the variables that we care about: `n` and `a`.


  [1]: https://www.wikiod.com/scala/case-classes

## Tuple Extractors
`x` and `y` are extracted from the tuple:
```scala
val (x, y) = (1337, 42)
// x: Int = 1337
// y: Int = 42
```

To ignore a value use `_`:

```scala
val (_, y: Int) = (1337, 42)
// y: Int = 42
```

To unpack an extractor:

```scala
val myTuple = (1337, 42)
myTuple._1  // res0: Int = 1337
myTuple._2  // res1: Int = 42
```

Note that tuples have a maximum length of 22, and thus `._1` through `._22` will work (assuming the tuple is at least that size).

Tuple extractors may be used to provide symbolic arguments for literal functions:

```scala
val persons = List("A." -> "Lovelace", "G." -> "Hopper")
val names = List("Lovelace, A.", "Hopper, G.")

assert {
  names ==
    (persons map { name =>
      s"${name._2}, ${name._1}"
    })
}

assert {
  names ==
    (persons map { case (given, surname) =>
      s"$surname, $given"
    })
}
```

## Regex Extractors
A regular expression with grouped parts can be used as an extractor:

    scala> val address = """(.+):(\d+)""".r
    address: scala.util.matching.Regex = (.+):(\d+)
    
    scala> val address(host, port) = "some.domain.org:8080"
    host: String = some.domain.org
    port: String = 8080

Note that when it is not matched, a `MatchError` will be thrown at runtime:

    scala> val address(host, port) = "something not a host and port"
    scala.MatchError: something not a host and port (of class java.lang.String)

## Extractor Infix notation
If a case class has exactly two values, its extractor can be used in infix notation.


    case class Pair(a: String, b: String)
    val p: Pair = Pair("hello", "world")
    val x Pair y = p
    //x: String = hello
    //y: String = world


Any extractor that returns a 2-tuple can work this way.


    object Foo {
        def unapply(s: String): Option[(Int, Int)] = Some((s.length, 5))
    }
    val a Foo b = "hello world!"
    //a: Int = 12
    //b: Int = 5



## Unapply - Custom Extractors
A custom extraction can be written by implementing the `unapply` method and returning a value of type `Option`:

    class Foo(val x: String)

    object Foo {
      def unapply(foo: Foo): Option[String] = Some(foo.x)
    }
    
    new Foo("42") match {
      case Foo(x) => x
    }
    // "42"

The return type of `unapply` may be something other than `Option`, provided the type returned provides `get` and `isEmpty` methods. In this example, `Bar` is defined with those methods, and `unapply` returns an instance of `Bar`:

    class Bar(val x: String) {
      def get = x
      def isEmpty = false
    }

    object Bar {
      def unapply(bar: Bar): Bar = bar
    }
    
    new Bar("1337") match {
      case Bar(x) => x
    }
    // "1337"

The return type of `unapply` can also be a `Boolean`, which is a special case that does not carry the `get` and `isEmpty` requirements above. However, note in this example that `DivisibleByTwo` is an object, not a class, and does not take a parameter (and therefore that parameter cannot be bound):

    object DivisibleByTwo {
      def unapply(num: Int): Boolean = num % 2 == 0
    }

    4 match {                        
      case DivisibleByTwo() => "yes" 
      case _ => "no"
    }
    // yes

    3 match {
      case DivisibleByTwo() => "yes"
      case _ => "no"
    }
    // no

Remember that `unapply` goes in the companion object of a class, not in the class. The example above will be clear if you understand this distinction.

## Transformative extractors
Extractor behavior can be used to derive arbitrary values from their input. This can be useful in scenarios where you want to be able to act on the results of a transformation in the event that the transformation is successful.

Consider as an example the various [user name formats usable in a Windows environment][1]:

    object UserPrincipalName {
      def unapply(str: String): Option[(String, String)] = str.split('@') match {
        case Array(u, d) if u.length > 0 && d.length > 0 => Some((u, d))
        case _ => None
      }        
    }

    object DownLevelLogonName {
      def unapply(str: String): Option[(String, String)] = str.split('\\') match {
        case Array(d, u) if u.length > 0 && d.length > 0 => Some((d, u))
        case _ => None
      }
    }

    def getDomain(str: String): Option[String] = str match {
      case UserPrincipalName(_, domain) => Some(domain)
      case DownLevelLogonName(domain, _) => Some(domain)
      case _ => None
    }

In fact it is possible to create an extractor exhibiting both behaviors by broadening the types it can match:

    object UserPrincipalName {
      def unapply(obj: Any): Option[(String, String)] = obj match {
        case upn: UserPrincipalName => Some((upn.username, upn.domain))
        case str: String => str.split('@') match {
          case Array(u, d) if u.length > 0 && d.length > 0 => Some((u, d))
          case _ => None
        }
        case _ => None
      }        
    }

In general, extractors are simply a convenient reformulation of the `Option` pattern, as applied to methods with names like `tryParse`:

    UserPrincipalName.unapply("user@domain") match {
      case Some((u, d)) => ???
      case None => ???
    }

[1]: https://msdn.microsoft.com/en-us/library/windows/desktop/aa380525(v=vs.85).aspx

