---
title: "If Expressions"
slug: "if-expressions"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Basic If Expressions
In Scala (in contrast to Java and most other languages), `if` is an **expression** instead of a *statement*. Regardless, the syntax is identical:

```
if(x < 1984) {
   println("Good times")
} else if(x == 1984) {
   println("The Orwellian Future begins")
} else {
   println("Poor guy...")
}
```

The implication of `if` being an expression is that you can assign the result of the evalation of the expression to a variable:

```
val result = if(x > 0) "Greater than 0" else "Less than or equals 0"
\\ result: String = Greater than 0
```

Above we see that the `if` expression is evaluated and `result` is set to that resulting value.

The return type of an `if` expression is the **supertype** of all logic branches. This means that for this example the return type is a ``String``. Since not all `if` expressions return a value (such as an `if` statement that has no `else` branch logic), it is possible that the return type is `Any`:

    val result = if(x > 0) "Greater than 0"
    // result: Any = Greater than 0

If no value can be returned (such as if only side effects like `println` are used inside the logical branches), then the return type will be `Unit`:

    val result = if(x > 0) println("Greater than 0")
    // result: Unit = ()

`if` expressions in Scala are similar to how the [ternary operator in Java][1] functions. Because of this similarity, there is no such operator in Scala: it would be redundant.

> Curly braces can be omitted in an `if` expression if the content is a single line.


  [1]: https://www.wikiod.com/java/operators#The Conditional Operator (? :)

