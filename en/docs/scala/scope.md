---
title: "Scope"
slug: "scope"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Scope on Scala defines where a value (`def`, `val`, `var` or `class`) can be accessed from.

## Syntax
 - declaration
 - private declaration
 - private[this] declaration
 - private[fromWhere] declaration
 - protected declaration
 - protected[fromWhere] declaration

## Public (default) scope
By default, the scope is `public`, the value can be accessed from anywhere.

    package com.example {
      class FooClass {
        val x = "foo"
      }
    }

    package an.other.package {
      class BarClass {
        val foo = new com.example.FooClass
        foo.x // <- Accessing a public value from another package
      }
    }

## A private scope
When the scope is private, it can only be accessed from the current class or other instances of the current class.

    package com.example {
      class FooClass {
        private val x = "foo"
        def aFoo(otherFoo: FooClass) {
          otherFoo.x // <- Accessing from another instance of the same class
        }
      }
      class BarClass {
        val f = new FooClass
        f.x // <- This will not compile
      }
    }

## A private package-specific scope
You can specify a package where the private value can be accessed.

    package com.example {
      class FooClass {
        private val x = "foo"
        private[example] val y = "bar"
      }
      class BarClass {
        val f = new FooClass
        f.x // <- Will not compile
        f.y // <- Will compile
      }
    }

## Object private scope
The most restrictive scope is _"object-private"_ scope, which only allows that value to be accessed from the same instance of the object.

    class FooClass {
      private[this] val x = "foo"
      def aFoo(otherFoo: FooClass) = {
        otherFoo.x // <- This will not compile, accessing x outside the object instance
      }
    }


## Protected scope
The protected scope allows the value to be accessed from any subclasses of the current class.

    class FooClass {
      protected val x = "foo"
    }
    class BarClass extends FooClass {
      val y = x // It is a subclass instance, will compile
    }
    class ClassB {
      val f = new FooClass
      f.x // <- This will not compile
    }


## Package protected scope
The package protected scope allows the value to be accessed only from any subclass in a specific package.

    package com.example {
      class FooClass {
        protected[example] val x = "foo"
      }
      class ClassB extends FooClass {
        val y = x // It's in the protected scope, will compile
      }
    }
    package com {
      class BarClass extends com.example.FooClass {
        val y = x // <- Outside the protected scope, will not compile
      }
    }

