---
title: "Creating a Class"
slug: "creating-a-class"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Creating a class in Pharo is as simple as sending `subclass:` message to another class object. But most of the classes are created and modified in a system browser (currently Nautilus).

## Adding a class in a system browser
The most common way to add (and edit classes) in from the system browser In the Nautilus system browser have nothing selected or select a package (first column) or a class (second column). Depending on the selection, the code editor will display a slightly different class template:

| Selection | Template |
| ------ | ------ |
| None    | Empty class template   |
| Package | A class template with pre-filled package name (based on the selected package) |
| Class   | An actual definition of the selected class |

The following image demonstrates the Nautilus window with a selected package:
[![Nautilus window with a selected package][1]][1]

By editing the class template and accepting (saving) the changes you will create a class (or modify an existing one if a class with the same name exists)

  [1]: https://i.stack.imgur.com/s06DU.png

## Trivial class creation code
In Pharo everything is an object, and every object responds to messages. So if you evaluate the following code

    Object subclass: #MyClass
the object `Object` will create for you its subclass called _MyClass_. If you don't have any particular superclass in your mind it's advised to subclass from `Object` but this brings a tautological confusion into the previous example. Let's say that you want to create a class _PriorityStack_ with class _Stack_ as a superclass, then you need to evaluate:

    Stack subclass: #PriorityStack

## Anonymous class
You can create classes without names that are not installed in the system by sending `newAnonymousSubclass` to a class.

For example

    anonymousSet := Set newAnonymousSubclass

will assign an anonymous subclass of Set to `anonymousSet` variable. Then you can compile methods in this class and instantiate it, or swap it with a real class.

Useful for test resources of for proxying

