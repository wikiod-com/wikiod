---
title: "Single Abstract Method Types (SAM Types)"
slug: "single-abstract-method-types-sam-types"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

Single Abstract Methods are types, introduced in [Java 8][1], that have exactly one abstract member.


  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html

## Lambda Syntax
**NOTE: This is only available in Scala 2.12+ (and in recent 2.11.x versions with the `-Xexperimental -Xfuture` compiler flags)**

A SAM type can be implemented using a lambda:

<!-- if version [gte 2.11.8] -->
    trait Runnable {
      def run(): Unit
    }

    val t: Runnable = () => println("foo")
<!-- end version if -->

The type can optionally have other non-abstract members:

<!-- if version [gte 2.11.8] -->
    trait Runnable {
      def run(): Unit
      def concrete: Int = 42
    }

    val t: Runnable = () => println("foo")
<!-- end version if -->


