---
title: "Java interop"
slug: "java-interop"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
 - `.` let's you access instance methods
 - `.-` let's you access instance fields
 - `..` macro expanding to multiple nested invocations of `.`

As a hosted language, Clojure provides excellent interoperability support with Java. Clojure code can also be called directly from Java.

## Calling an instance method on a Java object
You can call an instance method using the `.` special form:

    (.trim " hello ")
    ;;=> "hello"

You can call instance methods with arguments like this:

    (.substring "hello" 0 2)
    ;;=> "he"

## Creating a new Java object
You can create instance of objects in one of two ways:

    (java.awt.Point. 0 1)
    ;;=> => #object[java.awt.Point 0x3776d535 "java.awt.Point[x=0,y=1]"]

Or

    (new java.awt.Point 0 1)
    ;;=> => #object[java.awt.Point 0x3776d535 "java.awt.Point[x=0,y=1]"]

## Referencing an instance field on a Java Object
You can call an instance field using the `.-` syntax:

    (def p (java.awt.Point. 0 1))
    (.-x p)
    ;;=> 0
    (.-y p)
    ;;=> 1


## Calling a static method
You can call static methods like this:

    (System/currentTimeMillis)
    ;;=> 1469493415265

Or pass in arguments, like this:

    (System/setProperty "foo" "42")
    ;;=> nil
    (System/getProperty "foo")
    ;;=> "42"

    

## Calling a Clojure function from Java
You can call a Clojure function from Java code by looking up the function and invoking it:

    IFn times = Clojure.var("clojure.core", "*");
    times.invoke(2, 2);

This looks up the `*` function from the `clojure.core` namespace and invokes it with the arguments 2 & 2.

