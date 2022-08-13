---
title: "String Interpolation"
slug: "string-interpolation"
draft: false
images: []
weight: 9905
type: docs
toc: true
---

This feature exists in Scala 2.10.0 and above.

## Using expression in string literals
You can use curly braces to interpolate expressions into string literals:

```
def f(x: String) = x + x
val a = "A"

s"${a}"    // "A"
s"${f(a)}" // "AA"
```

---

Without the braces, scala would only interpolate the *identifier* after the `$` (in this case `f`). Since there is no implicit conversion from `f` to a `String` this is an exception in this example:

```
s"$f(a)"  // compile-time error (missing argument list for method f)
```

## Hello String Interpolation
The **s** interpolator allows the usage of variables within a string.

    val name = "Brian"
    println(s"Hello $name")

prints "Hello Brian" to the console when ran.

## Formatted String Interpolation Using the f Interpolator
    val num = 42d

Print two decimal places for `num` using *f*

    println(f"$num%2.2f")
    42.00

Print `num` using scientific notation using *e*

    println(f"$num%e")
    4.200000e+01

Print `num` in hexadecimal with *a* 

    println(f"$num%a")
    0x1.5p5

Other format strings can be found at https://docs.oracle.com/javase/6/docs/api/java/util/Formatter.html#detail





## String interpolators as extractors
It is also possible to use Scala's string interpolation feature to create elaborate extractors (pattern matchers), as perhaps most famously employed in the [quasiquotes API][1] of Scala macros.

Given that `n"p0${i0}p1"` desugars to `new StringContext("p0", "p1").n(i0)`, it is perhaps unsurprising that extractor functionality is enabled by providing an implicit conversion from `StringContext` to a class with property `n` of a type defining an `unapply` or `unapplySeq` method. 

As an example, consider the following extractor which extracts path segments by constructing a regular expression from the `StringContext` parts. We can then delegate most of the heavy lifting to the `unapplySeq` method provided by the resulting [scala.util.matching.Regex][2]:

    implicit class PathExtractor(sc: StringContext) {
      object path {
        def unapplySeq(str: String): Option[Seq[String]] =
          sc.parts.map(Regex.quote).mkString("^", "([^/]+)", "$").r.unapplySeq(str)      
      }
    }

    "/documentation/scala/1629/string-interpolation" match {
      case path"/documentation/${topic}/${id}/${_}" => println(s"$topic, $id")
      case _ => ???
    }

Note that the `path` object could also define an `apply` method in order to behave as a regular interpolator as well.

[1]: http://docs.scala-lang.org/overviews/quasiquotes/unlifting
[2]: http://www.scala-lang.org/api/current/#scala.util.matching.Regex

## Custom string interpolators
It is possible to define custom string interpolators in addition to the built-in ones.

    my"foo${bar}baz"

Is expanded by the compiler to:
    
    new scala.StringContext("foo", "baz").my(bar)

`scala.StringContext` has no `my` method, therefore it can be provided by implicit conversion. A custom interpolator with the same behavior as the builtin `s` interpolator would then be implemented as follows:

    implicit class MyInterpolator(sc: StringContext) {
      def my(subs: Any*): String = {
        val pit = sc.parts.iterator
        val sit = subs.iterator
        // Note parts.length == subs.length + 1
        val sb = new java.lang.StringBuilder(pit.next())
        while(sit.hasNext) {
          sb.append(sit.next().toString)
          sb.append(pit.next())          
        }
        sb.toString
      }
    }

And the interpolation `my"foo${bar}baz"` would desugar to:

    new MyInterpolation(new StringContext("foo", "baz")).my(bar)

Note that there is no restriction on the arguments or return type of the interpolation function. This leads us down a dark path where interpolation syntax can be used creatively to construct arbitrary objects, as illustrated in the following example:

    case class Let(name: Char, value: Int)

    implicit class LetInterpolator(sc: StringContext) {
      def let(value: Int): Let = Let(sc.parts(0).charAt(0), value)
    }

    let"a=${4}" // Let(a, 4)
    let"b=${"foo"}" // error: type mismatch
    let"c=" // error: not enough arguments for method let: (value: Int)Let

## Raw String Interpolation
You can use the **raw** interpolator if you want a String to be printed as is and without any escaping of literals.
<pre><code>
println(raw"Hello World In English And French\nEnglish:\tHello World\nFrench:\t\tBonjour Le Monde")
</code></pre>

With the use of the **raw** interpolator, you should see the following printed in the console:
<pre><code>
Hello World In English And French\nEnglish:\tHello World\nFrench:\t\tBonjour Le Monde
</code></pre>

Without the **raw** interpolator, `\n` and `\t` would have been escaped.
<pre><code>
println("Hello World In English And French\nEnglish:\tHello World\nFrench:\t\tBonjour Le Monde")
</code></pre>
Prints:
<pre><code>
Hello World In English And French
English:       Hello World
French:        Bonjour Le Monde
</code></pre>

