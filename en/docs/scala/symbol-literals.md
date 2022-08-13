---
title: "Symbol Literals"
slug: "symbol-literals"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Scala comes with a concept of **symbols** - strings that are *interned*, that is: two symbols with the same name (the same character sequence), in contrary to strings, will refer to the same object during execution. 

Symbols are a feature of many languages: Lisp, Ruby and Erlang and more, however in Scala they are of relatively small use. Good feature to have nevertheless.

**Use:**

Any literal beginning with a single quote `'`, followed by one or more digits, letters, or under‐scores `_` is a symbol literal. The first character is an exception as it can’t be a digit.

Good definitions:

    'ATM
    'IPv4
    'IPv6
    'map_to_operations
    'data_format_2006

    // Using the `Symbol.apply` method

    Symbol("hakuna matata")
    Symbol("To be or not to be that is a question")


Bad definitions:

    '8'th_division
    '94_pattern
    'bad-format


## Replacing strings in case clauses
Let's say we have multiple data sources which include *database, file, prompt* and *argumentList*. Depending on chosen source we change our approach:

    def loadData(dataSource: Symbol): Try[String] = dataSource match {
      case 'database => loadDatabase() // Loading data from database
      case 'file =>  loadFile() // Loading data from file
      case 'prompt => askUser() // Asking user for data
      case 'argumentList => argumentListExtract() // Accessing argument list for data
      case _ => Failure(new Exception("Unsupported data source"))
    }

We could have very well used `String` in place of `Symbol`. We didn't, because none of strings's features are useful in this context.

This makes the code simpler and less error prone.



