---
title: "Operators in Scala"
slug: "operators-in-scala"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Built-in Operators
Scala has the following built-in operators (methods/language elements with predefined precedence rules):

| Type| Symbol | Example |
| ------ | ------ | ------ |
| Arithmetic operators | `+` `-` `*` `/` `%` | `a + b` |
| Relational operators | `==` `!=` `>` `<` `>=` `<=` | `a > b` |
| Logical operators | `&&` `&` <code>&#124;&#124;</code> <code>&#124;</code> `!` | `a && b` |
| Bit-wise operators | `&` <code>&#124;</code> `^` `~` `<<` `>>` `>>>` | `a & b`, `~a`, `a >>> b` |
| Assignment operators | `=` `+=` `-=` `*=` `/=` `%=` `<<=` `>>=` `&=` `^=` \|= | `a += b` 

Scala operators have the same meaning as in [Java](https://www.wikiod.com/java/operators)

**Note**: methods ending with `:` bind to the right (and right associative), so the call with `list.::(value)` can be written as `value :: list` with operator syntax. (`1 :: 2 :: 3 :: Nil` is the same as `1 :: (2 :: (3 :: Nil))`)

## Operator Overloading
In Scala you can define your own operators:

```
class Team {
   def +(member: Person) = ...
}
```

With the above defines you can use it like:

```
ITTeam + Jack
```

or 

```
ITTeam.+(Jack)
```

To define unary operators you can prefix it with `unary_`. E.g. `unary_!`

```
class MyBigInt {
   def unary_! = ...
}

var a: MyBigInt = new MyBigInt
var b = !a
```


## Operator Precedence
| Category | Operator | Associativity |
| --- | --- | --- |
| Postfix | `()` `[]` |  Left to right |
| Unary | `!` `~` | Right to left |
| Multiplicative | `*` `/` `%` | Left to right |
| Additive | `+` `-` | Left to right |
| Shift | `>>` `>>>` `<<` | Left to right |
| Relational | `>` `>=` `<` `<=` | Left to right |
| Equality | `==` `!=` | Left to right |
| Bitwise and | `&` | Left to right |
| Bitwise xor | `^` | Left to right |
| Bitwise or | <code>&#124;</code> | Left to right |
| Logical and | `&&` | Left to right |
| Logical or | <code>&#124;&#124;</code> | Left to right |
| Assignment | `=` `+=` `-=` `*=` `/=` `%=` `>>=` `<<=` `&=` `^=` \|= | Right to left |
| Comma | `,` | Left to right |

[Programming in Scala][1] gives the following outline based on the 1st character in the operator. E.g. `>` is the 1st character in the operator `>>>`:

| Operator |
| --- |
| (all other special characters) |
| `*` `/` `%` |
| `+` `-` |
| `:` |
| `=` `!` |
| `<` `>` |
| `&` |
| `^` |
| <code>&#124;</code> |
| (all letters) |
| (all assignment operators) |

The one exception to this rule concerns *assignment operators*, e.g. `+=`, `*=`, etc. If an operator ends with an equal character (=) and is not one of the comparison operators `<=`, `>=`, `==` or `!=`, then the precedence of the operator is the same as simple assignment. In other words, lower than that of any other operator.


  [1]: http://www.artima.com/pins1ed/basic-types-and-operations.html

