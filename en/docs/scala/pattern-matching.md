---
title: "Pattern Matching"
slug: "pattern-matching"
draft: false
images: []
weight: 9481
type: docs
toc: true
---

## Syntax
- selector match partialFunction
- selector match {list of case alternatives) // This is most common form of the above


## Parameters
| Parameter | Details |
| --------- | ------- |
| selector  | The expression whose value is being pattern-matched. |
| alternatives | a list of `case`-delimited alternatives. |

## Simple Pattern Match
This example shows how to match an input against several values:

    def f(x: Int): String = x match {
      case 1 => "One"
      case 2 => "Two"
      case _ => "Unknown!"
    }
    
    f(2)  // "Two"
    f(3)  // "Unknown!"

[Live demo](http://ideone.com/cWOQYT)


Note:  `_` is the *fall through* or *default* case, but it is not required.

    def g(x: Int): String = x match {
      case 1 => "One"
      case 2 => "Two"
    }

    g(1)  // "One"
    g(3)  // throws a MatchError

To avoid throwing an exception, it is a best functional-programming practice here to handle the default case (`case _ => <do something>`). Note that matching over *[a case class][1]* can help the compiler produce a warning if a case is missing.  The same goes for user-defined types which extend a sealed trait. If the match is total then a default case may not be needed 

  [1]: https://www.wikiod.com/scala/case-classes

It is also possible to match against values that are not defined inline. These must be *stable identifiers*, which are obtained by either using a capitalized name or enclosing backticks.

With `One`and `two` defined somewhere else, or passed as function parameters:

    val One: Int = 1
    val two: Int = 2

They can be matched against in the following way:

    def g(x: Int): String = x match {
      case One => "One"
      case `two` => "Two"
    }

Unlike other programming languages as Java for example there is no fall through. If a case block matches an input, it gets executed and the matching is finished. Therefore the least specific case should be the last case block.

    def f(x: Int): String = x match {
      case _ => "Default"
      case 1 => "One"
    }
    
    f(5) // "Default"
    f(1) // "Default"

## Pattern Matching on a Seq
To check for a precise number of elements in the collection

    def f(ints: Seq[Int]): String = ints match {
      case Seq() =>
          "The Seq is empty !"
      case Seq(first) =>
          s"The seq has exactly one element : $first"
      case Seq(first, second) =>
          s"The seq has exactly two elements : $first, $second"
      case  s @ Seq(_, _, _) => 
          s"s is a Seq of length three and looks like ${s}"  // Note individual elements are not bound to their own names.
      case s: Seq[Int] if s.length == 4 =>
          s"s is a Seq of Ints of exactly length 4"  // Again, individual elements are not bound to their own names.
      case _ =>
          "No match was found!"
    }

[Live demo](http://ideone.com/uTGdSa)

To extract the first(s) element(s) and keeping the rest as a collection:

    def f(ints: Seq[Int]): String = ints match {
      case Seq(first, second, tail @ _*) =>
          s"The seq has at least two elements : $first, $second. The rest of the Seq is $tail"
      case Seq(first, tail @ _*) =>
          s"The seq has at least one element : $first. The rest of the Seq is $tail"
      // alternative syntax
      // here of course this one will never match since it checks
      // for the same thing as the one above
      case first +: tail =>
          s"The seq has at least one element : $first. The rest of the Seq is $tail"
      case _ =>
          "The seq didn't match any of the above, so it must be empty"
    }
In general, any form that can be used to construct a sequence can be used to pattern match against an existing sequence.

Note that while using `Nil` and `::` will work when pattern matching a Sequence, it does convert it to a `List`, and can have unexpected results. Constrain yourself to `Seq( ...)` and `+:` to avoid this.

Note that while using `::` will not work for `WrappedArray`, `Vector` etc, see:

    scala> def f(ints:Seq[Int]) = ints match {
         | case h :: t => h
         | case _ => "No match"
         | }
    f: (ints: Seq[Int])Any
    
    scala> f(Array(1,2))
    res0: Any = No match

And with `+:`

    scala> def g(ints:Seq[Int]) = ints match {
         | case h+:t => h
         | case _ => "No match"
         | }
    g: (ints: Seq[Int])Any
    
    scala> g(Array(1,2).toSeq)
    res4: Any = 1



## Guards (if expressions)
Case statements can be combined with if expressions to provide extra logic when pattern matching.

    def checkSign(x: Int): String = {
        x match {
          case a if a < 0 => s"$a is a negative number"
          case b if b > 0 => s"$b is a positive number"
          case c => s"$c neither positive nor negative"
        }
    }

It is important to ensure your guards do not create a non-exhaustive match (the compiler often will not catch this):

    def f(x: Option[Int]) = x match {
        case Some(i) if i % 2 == 0 => doSomething(i)
        case None    => doSomethingIfNone
    }

This throws a `MatchError` on odd numbers. You must either account for all cases, or use a wildcard match case:

    def f(x: Option[Int]) = x match {
        case Some(i) if i % 2 == 0 => doSomething(i)
        case _ => doSomethingIfNoneOrOdd
    }

## Pattern Matching with Regex
    val emailRegex: Regex = "(.+)@(.+)\\.(.+)".r

    "name@example.com" match {
      case emailRegex(userName, domain, topDomain) => println(s"Hi $userName from $domain")
      case _ => println(s"This is not a valid email.")
    }

In this example, the regex attempts to match the email address provided. If it does, the `userName` and `domain` is extracted and printed. `topDomain` is also extracted, but nothing is done with it in this example.
Calling `.r` on a String `str` is equivalent to `new Regex(str)`. The `r` function is available via an [implicit conversion][1].


  [1]: https://www.wikiod.com/scala/implicits#Implicit Conversion

## Pattern Matching With Stable Identifier
In standard pattern matching, the identifier used will shadow any identifier in the enclosing scope. Sometimes it is necessary to match on the enclosing scope's variable.

The following example function takes a character and a list of tuples and returns a new list of tuples. If the character existed as the first element in one of the tuples, the second element is incremented. If it does not yet exist in the list, a new tuple is created.

    def tabulate(char: Char, tab: List[(Char, Int)]): List[(Char, Int)] = tab match {
      case Nil => List((char, 1))
      case (`char`, count) :: tail => (char, count + 1) :: tail
      case head :: tail => head :: tabulate(char, tail)
    }

The above demonstrates pattern matching where the method's input, `char`, is kept 'stable' in the pattern match: that is, if you call `tabulate('x', ...)`, the first case statement would be interpreted as:

    case('x', count) => ...

Scala will interpret any variable demarcated with a tick mark as a stable identifier: it will also interpret any variable that starts with a capital letter in the same way.

## Matching on an Option
If you are matching on an [Option][1] type:

```
def f(x: Option[Int]) = x match {
    case Some(i) => doSomething(i)
    case None    => doSomethingIfNone
}
```
This is functionally equivalent to using `fold`, or `map`/`getOrElse`:

    def g(x: Option[Int]) = x.fold(doSomethingIfNone)(doSomething)
    def h(x: Option[Int]) = x.map(doSomething).getOrElse(doSomethingIfNone)


  [1]: https://www.wikiod.com/scala/option-class

## Pattern Matching Sealed Traits
When pattern matching an object whose type is a sealed trait, Scala will check at compile-time that all cases are 'exhaustively matched':


    sealed trait Shape
    case class Square(height: Int, width: Int) extends Shape
    case class Circle(radius: Int) extends Shape
    case object Point extends Shape
    
    
    def matchShape(shape: Shape): String = shape match {
        case Square(height, width) => "It's a square"
        case Circle(radius)        => "It's a circle"
        //no case for Point because it would cause a compiler warning.
    }

If a new `case class` for `Shape` is later added, all `match` statements on `Shape` will start to throw a compiler warning. This makes thorough refactoring easier: the compiler will alert the developer to all code that needs to be updated.


## Pattern binder (@)
The `@` sign binds a variable to a name during a pattern match. The bound variable can either be the entire matched object or part of the matched object:

    sealed trait Shape
    case class Rectangle(height: Int, width: Int) extends Shape
    case class Circle(radius: Int) extends Shape
    case object Point extends Shape
  
    (Circle(5): Shape) match {
      case Rectangle(h, w) => s"rectangle, $h x $w."
      case Circle(r) if r > 9 => s"large circle"
      case c @ Circle(_) => s"small circle: ${c.radius}"  // Whole matched object is bound to c
      case Point => "point"
    }
    
`> res0: String = small circle: 5`

The bound identifier can be used in conditional filters. Thus:
 
    case Circle(r) if r > 9 => s"large circle"

can be written as:

    case c @ Circle(_) if c.radius > 9 => s"large circle"

The name can be bound to only a part of the matched pattern:

    Seq(Some(1), Some(2), None) match {
      // Only the first element of the matched sequence is bound to the name 'c'
      case Seq(c @ Some(1), _*) => head
      case _ => None
    }


`> res0: Option[Int] = Some(1)`



## Pattern Matching with Case Classes
Every case class defines an extractor that can be used to capture the members of the case class when pattern matching:


    case class Student(name: String, email: String)
    
    def matchStudent1(student: Student): String = student match {
        case Student(name, email) => s"$name has the following email: $email" // extract name and email
    }

All the normal rules of pattern-matching apply - you can use guards and constant expressions to control matching:

    def matchStudent2(student: Student): String = student match {
        case Student("Paul", _) => "Matched Paul" // Only match students named Paul, ignore email
        case Student(name, _) if name == "Paul" => "Matched Paul" // Use a guard to match students named Paul, ignore email
        case s if s.name == "Paul" => "Matched Paul" // Don't use extractor; use a guard to match students named Paul, ignore email
        case Student("Joe", email) => s"Joe has email $email" // Match students named Joe, capture their email
        case Student(name, email) if name == "Joe" => s"Joe has email $email" // use a guard to match students named Joe, capture their email
        case Student(name, email) => s"$name has email $email." // Match all students, capture name and email 
    }


## Pattern Matching Types
Pattern matching can also be used to check the type of an instance, rather than using [`isInstanceOf[B]`][1]:

    val anyRef: AnyRef = ""
                                                      
    anyRef match {
      case _: Number       => "It is a number"
      case _: String       => "It is a string"
      case _: CharSequence => "It is a char sequence"
    }
    //> res0: String = It is a string

The order of the cases is important:

    anyRef match {
      case _: Number       => "It is a number"
      case _: CharSequence => "It is a char sequence"
      case _: String       => "It is a string"
    }
    //> res1: String = It is a char sequence

In this manner it is similar to a classical 'switch' statement, without the fall-through functionality. However, you can also pattern match and 'extract' values from the type in question. For instance:

    case class Foo(s: String)
    case class Bar(s: String)
    case class Woo(s: String, i: Int)
    
    def matcher(g: Any):String = {
      g match {
        case Bar(s) => s + " is classy!" 
        case Foo(_) => "Someone is wicked smart!"
        case Woo(s, _) => s + " is adventerous!"
        case _ => "What are we talking about?"
      }
    }
    
    print(matcher(Foo("Diana")))  // prints 'Diana is classy!'
    print(matcher(Bar("Hadas")))  // prints 'Someone is wicked smart!'
    print(matcher(Woo("Beth", 27)))   // prints 'Beth is adventerous!'
    print(matcher(Option("Katie")))  // prints 'What are we talking about?'

Note that in the `Foo` and `Woo` case we use the underscore (`_`) to 'match an unbound variable'. That is to say that the value (in this case `Hadas` and `27`, respectively) is not bound to a name and thus is not available in the handler for that case. This is useful shorthand in order to match 'any' value without worrying about what that value is.

  [1]: http://docs.scala-lang.org/tutorials/scala-for-java-programmers.html#traits

## Pattern Matching compiled as tableswitch or lookupswitch
The `@switch` annotation tells the compiler that the `match` statement can be replaced with a single `tableswitch` instruction at the bytecode level. This is a minor optimization that can remove unnecessary comparisons and variable loads during runtime.

The `@switch` annotation works only for matches against literal constants and `final val` identifiers. If the pattern match cannot be compiled as a `tableswitch`/`lookupswitch`, the compiler will raise a warning.

    import annotation.switch

    def suffix(i: Int) = (i: @switch) match {
      case 1 => "st"
      case 2 => "nd"
      case 3 => "rd"
      case _ => "th"
    }

 The results are the same as a normal pattern match:

    scala> suffix(2)
    res1: String = "2nd"
    
    scala> suffix(4)
    res2: String = "4th"

---

From the [Scala Documentation][3] (2.8+) – `@switch`:
> An annotation to be applied to a match expression. If present, the
> compiler will verify that the match has been compiled to a tableswitch
> or lookupswitch, and issue an error if it instead compiles into a
> series of conditional expressions.

From the Java Specification:
 * [tableswitch][1]: "Access jump table by index and jump"
 * [lookupswitch][2]: "Access jump table by key match and jump"

  [1]: https://docs.oracle.com/javase/specs/jvms/se6/html/Instructions2.doc14.html
  [2]: https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html#lookupswitch
  [3]: http://www.scala-lang.org/api/2.12.1/scala/annotation/switch.html


## Matching Multiple Patterns At Once
The `|` can be used to have a single case statement match against multiple inputs to yield the same result:

    def f(str: String): String = str match {
      case "foo" | "bar" => "Matched!"
      case _ => "No match."
    }
    
    f("foo")  // res0: String = Matched!
    f("bar")  // res1: String = Matched!
    f("fubar")  // res2: String = No match.

Note that while matching **values** this way works well, the following matching of **types** will cause problems:

    sealed class FooBar
    case class Foo(s: String) extends FooBar
    case class Bar(s: String) extends FooBar
    
    val d = Foo("Diana")
    val h = Bar("Hadas")
    
    // This matcher WILL NOT work.
    def matcher(g: FooBar):String = {
      g match {
        case Foo(s) | Bar(s) => print(s)  // Won't work: s cannot be resolved
        case Foo(_) | Bar(_) => _         // Won't work: _ is an unbound placeholder
        case _ => "Could not match"
      }
    }

If in the latter case (with `_`) you don't need the value of the unbound variable and just want to do something else, you're fine:

    def matcher(g: FooBar):String = {
      g match {
        case Foo(_) | Bar(_) => "Is either Foo or Bar."  // Works fine
        case _ => "Could not match"
      }
    }

Otherwise, you are left with splitting your cases:

    def matcher(g: FooBar):String = {
      g match {
        case Foo(s) => s 
        case Bar(s) => s
        case _ => "Could not match"
      }
    }

## Pattern Matching on tuples
Given the following `List` of tuples:

    val pastries = List(("Chocolate Cupcake", 2.50), 
                        ("Vanilla Cupcake", 2.25),
                        ("Plain Muffin", 3.25))

Pattern matching can be used to handle each element differently:

```scala
pastries foreach { pastry =>
  pastry match {
    case ("Plain Muffin", price) => println(s"Buying muffin for $price")
    case p if p._1 contains "Cupcake" => println(s"Buying cupcake for ${p._2}")
    case _ => println("We don't sell that pastry")
  }
}
```

The first case shows how to match against a specific string and get the corresponding price. The second case shows a use of if and [tuple extraction][1] to match against elements of the tuple.


  [1]: https://www.wikiod.com/scala/extractors#Tuple Extractors

