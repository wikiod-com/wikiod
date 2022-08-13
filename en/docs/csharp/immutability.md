---
title: "Immutability"
slug: "immutability"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## System.String class


## Strings and immutability
Immutable types are types that when changed create a new version of the object in memory, rather than changing the existing object in memory. The simplest example of this is the built-in `string` type.

Taking the following code, that appends " world" onto the word "Hello"

    string myString = "hello";
    myString += " world";

What is happening in memory in this case is that a new object is created when you append to the `string` in the second line. If you do this as part of a  large loop, there is the potential for this to cause performance issues in your application.

The mutable equivalent for a `string` is a `StringBuilder`

Taking the following code

    StringBuilder myStringBuilder = new StringBuilder("hello");
    myStringBuilder.append(" world");

When you run this, you are modifying the `StringBuilder` object itself in memory.

