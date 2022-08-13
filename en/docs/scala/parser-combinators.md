---
title: "Parser Combinators"
slug: "parser-combinators"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

**ParseResult Cases**

A `ParseResult` comes in three flavors:

- Success, with a marker as to the start of the match and the next character to be matched.
- Failure, with a marker as to the start of where the match was attempted. In this case the parser backtracks to that position, where it will be when parsing continues.
- Error, which stops the parsing. No backtracking or further parsing occurs.

## Basic Example
    import scala.util.parsing.combinator._
    
    class SimpleParser extends RegexParsers {
      // Define a grammar rule, turn it into a regex, and apply it the input.
      def word: Parser[String] = """[A-Z][a-z]+""".r ^^ { _.toString }
    }
    
    object SimpleParser extends SimpleParser {
      val parseAlice = parse(word, "Alice went to Alamo Square.")
      val parseBarb = parse(word, "barb went Upside Down.")
    }
    
    //Successfully finds a match
    println(SimpleParser.parseAlice)
    //Fails to find a match
    println(SimpleParser.parseBarb)

The output will be as follows:

    [1.6] parsed: Alice
    res0: Unit = ()
    
    [1.1] failure: string matching regex `[A-Z][a-z]+' expected but `b' found
    
    barb went Upside Down.
    ^

`[1.6]` in the `Alice` example indicates that the start of the match is at position `1`, and the fist character remaining to match starts at position `6`. 

