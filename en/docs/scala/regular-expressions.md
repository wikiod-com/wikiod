---
title: "Regular Expressions"
slug: "regular-expressions"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
 - re.findAllIn(s: CharSequence): MatchIterator
 - re.findAllMatchIn(s: CharSequence): Iterator[Match]
 - re.findFirstIn(s: CharSequence): Option[String]
 - re.findFirstMatchIn(s: CharSequence): Option[Match]
 - re.findPrefixMatchIn(s: CharSequence): Option[Match]
 - re.findPrefixOf(s: CharSequence): Option[String]
 - re.replaceAllIn(s: CharSequence, replacer: Match => String): String
 - re.replaceAllIn(s: CharSequence, replacement: String): String
 - re.replaceFirstIn(s: CharSequence, replacement: String): String
 - re.replaceSomeIn(s: CharSequence, replacer: Match => Option[String]): String
 - re.split(s: CharSequence): Array[String]

## Declaring regular expressions
The `r` method implicitly provided via [scala.collection.immutable.StringOps][3] produces an instance of [scala.util.matching.Regex][1] from the subject string. Scala's triple-quoted string syntax is useful here, as you do not have to escape backslashes as you would in Java:

    val r0: Regex = """(\d{4})-(\d${2})-(\d{2})""".r     // :)
    val r1: Regex = "(\\d{4})-(\\d{2})-(\\d{2})".r // :(

[scala.util.matching.Regex][1] implements an idiomatic regular expression API for Scala as a wrapper over [java.util.regex.Pattern][2], and the supported syntax is the same. That being said, Scala's support for multi-line string literals makes the `x` flag substantially more useful, enabling comments and ignoring pattern whitespace:

    val dateRegex = """(?x:
       (\d{4}) # year
      -(\d{2}) # month
      -(\d{2}) # day
    )""".r

There is an overloaded version of `r`, `def r(names: String*): Regex` which allows you to assign group names to your pattern captures. This is somewhat brittle as the names are disassociated from the captures, and should only be used if the regular expression will be used in multiple locations:

    """(\d{4})-(\d{2})-(\d{2})""".r("y", "m", "d").findFirstMatchIn(str) match {
      case Some(matched) =>
        val y = matched.group("y").toInt
        val m = matched.group("m").toInt
        val d = matched.group("d").toInt
        java.time.LocalDate.of(y, m, d)
      case None => ???
    }

[1]: http://www.scala-lang.org/api/current/#scala.util.matching.Regex
[2]: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html
[3]: http://www.scala-lang.org/api/current/#scala.collection.immutable.StringOps

## Repeating matching of a pattern in a string
    val re = """\((.*?)\)""".r

    val str = "(The)(example)(of)(repeating)(pattern)(in)(a)(single)(string)(I)(had)(some)(trouble)(with)(once)"

    re.findAllMatchIn(str).map(_.group(1)).toList
    res2: List[String] = List(The, example, of, repeating, pattern, in, a, single, string, I, had, some, trouble, with, once)

