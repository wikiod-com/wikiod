---
title: "Regular Expressions"
slug: "regular-expressions"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
- var regExp = RegExp(r'^(.*)$', multiLine: true, caseSensitive: false);

## Parameters
| Parameter | Details |
| --------- | ------- |
| `String source` | The regular expression as a `String` |
| `{bool multiline}` | Whether this is a multiline regular expression. (matches `^` and `$` at the beginning and end of each line individually not the whole String) |
| `{bool caseSensitive}` | If the expression is case sensitive |

Dart regular expressions have the same syntax and semantics as JavaScript regular expressions. See http://ecma-international.org/ecma-262/5.1/#sec-15.10 for the specification of JavaScript regular expressions.

This means that any JavaScript resource you find about Regular Expressions online applies to dart.

## Create and use a Regular Expression
```
var regExp = new RegExp(r"(\w+)");
var str = "Parse my string";
Iterable<Match> matches = regExp.allMatches(str);
```

It's a good idea to use "raw strings" (prefix with `r`)
when writing regular expressions so you can use unescaped backslashes in your expression.


