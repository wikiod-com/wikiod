---
title: "Type aliases"
slug: "type-aliases"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

With type aliases, we can give a alias to other type. It's ideal for giving a name to function types like `(String) -> Boolean` or generic type like `Pair<Person, Person>`.

Type aliases support generics. An alias can replace a type with generics and an alias can be generics.

## Syntax
 - **typealias** *alias-name* **=** *existing-type*

Type aliases is a feature of the compiler. Nothing is added in the generated code for the JVM. All aliases will be replaced by the real type.

## Function type
    typealias StringValidator = (String) -> Boolean
    typealias Reductor<T, U, V> = (T, U) -> V

## Generic type
    typealias Parents = Pair<Person, Person>
    typealias Accounts = List<Account>

