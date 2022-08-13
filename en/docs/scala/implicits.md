---
title: "Implicits"
slug: "implicits"
draft: false
images: []
weight: 9757
type: docs
toc: true
---

## Syntax
 - implicit val x: T = ???

Implicit classes allow custom methods to be added to existing types, without having to modify their code, thereby enriching types without needing control of the code. 

Using implicit types to enrich an existing class is often referred to as an 'enrich my library' pattern.

**Restrictions on Implicit Classes**

1.  Implicit classes may only exist within another class, object, or trait.
2.  Implicit classes may only have one non-implicit primary constructor parameter.
3.  There may not be another object, class, trait, or class member definition within the same scope that has the same name as the implicit class.

## Implicit Classes
Implicit classes make it possible to add new methods to previously defined classes.

The `String` class has no method `withoutVowels`. This can be added like so:

    object StringUtil {
      implicit class StringEnhancer(str: String) {
        def withoutVowels: String = str.replaceAll("[aeiou]", "")
      }
    }

The implicit class has a single constructor parameter (`str`) with the type that you would like to extend (`String`) and contains the method you would like to "add" to the type (`withoutVowels`). The newly defined methods can now be used directly on the enhanced type (when the enhanced type is in implicit scope):

    import StringUtil.StringEnhancer // Brings StringEnhancer into implicit scope
    
    println("Hello world".withoutVowels) // Hll wrld

Under the hood, implicit classes define an [implicit conversion][1] from the enhanced type to the implicit class, like this:

    implicit def toStringEnhancer(str: String): StringEnhancer = new StringEnhancer(str)

Implicit classes are often defined as [Value classes][2] to avoid creating runtime objects and thus removing the runtime overhead:

    implicit class StringEnhancer(val str: String) extends AnyVal {
        /* conversions code here */
    }


With the above improved definition, a new instance of `StringEnhancer` doesn't need to be created every time the `withoutVowels` method gets invoked.


  [1]: https://www.wikiod.com/scala/implicits#Implicit Conversion
  [2]: http://docs.scala-lang.org/overviews/core/value-classes.html

## Implicit Parameters
Implicit parameters can be useful if a parameter of a type should be defined once in the scope and then applied to all functions that use a value of that type.

A normal function call looks something like this:

    // import the duration methods
    import scala.concurrent.duration._

    // a normal method:
    def doLongRunningTask(timeout: FiniteDuration): Long = timeout.toMillis

    val timeout = 1.second
    // timeout: scala.concurrent.duration.FiniteDuration = 1 second

    // to call it
    doLongRunningTask(timeout) // 1000

Now lets say we have some methods that all have a timeout duration, and we want to call all those methods using the same timeout. We can define timeout as an implicit variable.

    // import the duration methods
    import scala.concurrent.duration._

    // dummy methods that use the implicit parameter
    def doLongRunningTaskA()(implicit timeout: FiniteDuration): Long = timeout.toMillis
    def doLongRunningTaskB()(implicit timeout: FiniteDuration): Long = timeout.toMillis

    // we define the value timeout as implicit
    implicit val timeout: FiniteDuration = 1.second
    
    // we can now call the functions without passing the timeout parameter
    doLongRunningTaskA() // 1000
    doLongRunningTaskB() // 1000

The way this works is that the scalac compiler looks for a value in the scope which is **marked as implicit** and **whose type matches** the one of the implicit parameter. If it finds one, it will apply it as the implicit parameter.

> Note that this won't work if you define two or even more implicits of
> the same type in the scope.

To customize the error message, use the `implicitNotFound` annotation on the type:

    @annotation.implicitNotFound(msg = "Select the proper implicit value for type M[${A}]!")
    case class M[A](v: A) {}
    
    def usage[O](implicit x: M[O]): O = x.v
  
    //Does not work because no implicit value is present for type `M[Int]`
    //usage[Int]   //Select the proper implicit value for type M[Int]!
    implicit val first: M[Int] = M(1)
    usage[Int]     //Works when `second` is not in scope
    implicit val second: M[Int] = M(2)
    //Does not work because more than one implicit values are present for the type `M[Int]`
    //usage[Int]   //Select the proper implicit value for type M[Int]!


A timeout is a usual use case for this, or for example in [Akka][1] the ActorSystem is (most of the times) always the same, so it's usually passed implicitly. Another use case would be library design, most commonly with FP libraries that rely on typeclasses (like [scalaz][2], [cats][3] or [rapture][4]).

> It's generally considered bad practice to use implicit parameters with basic types like *Int*, *Long*, *String* etc. since it will create confusion and make the code less readable.


  [1]: http://akka.io/
  [2]: https://github.com/scalaz/scalaz
  [3]: https://github.com/typelevel/cats
  [4]: https://github.com/propensive/rapture

## Implicit Conversion
An implicit conversion allows the compiler to automatically convert an object of one type to another type. This allows the code to treat an object as an object of another type.
    
    case class Foo(i: Int)
    
    // without the implicit
    Foo(40) + 2    // compilation-error (type mismatch)

    // defines how to turn a Foo into an Int
    implicit def fooToInt(foo: Foo): Int = foo.i

    // now the Foo is converted to Int automatically when needed
    Foo(40) + 2    // 42

The conversion is one-way: in this case you cannot convert `42` back to `Foo(42)`. To do so, a second implicit conversion must be defined:

    implicit def intToFoo(i: Int): Foo = Foo(i)

Note that this is the mechanism by which a float value can be added to an integer value, for instance. 

> Implicit conversions should be used sparingly because they obfuscate what is happening. It is a best practice to use an explicit conversion via a method call unless there's a tangible readability gain from using an implicit conversion.

There is no significant performance impact of implicit conversions.

Scala automatically imports a variety of implicit conversions in [`scala.Predef`][1], including all conversions from Java to Scala and back. These are included by default in any file compilation.


  [1]: http://www.scala-lang.org/api/2.12.x/scala/Predef$.html

## Resolving Implicit Parameters Using 'implicitly'
Assuming an implicit parameter list with more than one implicit parameter:

    case class Example(p1:String, p2:String)(implicit ctx1:SomeCtx1, ctx2:SomeCtx2)

Now, assuming that one of the implicit instances is not available (`SomeCtx1`) while all other implicit instances needed are in-scope, to create an instance of the class an instance of `SomeCtx1` must be provided.

This can be done while preserving each other in-scope implicit instance using the `implicitly` keyword:

    Example("something","somethingElse")(new SomeCtx1(), implicitly[SomeCtx2])

## Implicits in the REPL
To view all the `implicits` in-scope during a REPL session:

    scala> :implicits

To also include implicit conversions defined in `Predef.scala`:

    scala> :implicits -v

If one has an expression and wishes to view the effect of all rewrite rules that apply to it (including implicits):

    scala> reflect.runtime.universe.reify(expr) // No quotes. reify is a macro operating directly on code.

(Example:

    scala> import reflect.runtime.universe._
    scala> reify(Array("Alice", "Bob", "Eve").mkString(", "))
    resX: Expr[String] = Expr[String](Predef.refArrayOps(Array.apply("Alice", "Bob", "Eve")(Predef.implicitly)).mkString(", "))
)

