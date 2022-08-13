---
title: "Class Delegation"
slug: "class-delegation"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

A Kotlin class may implement an interface by delegating its methods and properties to another object that implements that interface. This provides a way to compose behavior using association rather than inheritance.

## Delegate a method to another class
    interface Foo {
        fun example()
    }

    class Bar {
        fun example() {
            println("Hello, world!")
        }
    }

    class Baz(b : Bar) : Foo by b

    Baz(Bar()).example()

The example prints `Hello, world!`

