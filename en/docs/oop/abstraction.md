---
title: "Abstraction"
slug: "abstraction"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Access Modifiers
Access modifiers are used to control the access to an object or to a function/method. This is a main part of the concept of _Abstraction_.

Different programming languages use different access modifiers. Here are some examples:

* __Java__

    Java has 4 access modifiers.
    1. `private` - These attributes can be accessed only inside the class.
    2. `protected` - These attributes can be accessed by sub classes and classes from the same package.
    3. `package` - These attributes can be accessed by the classes within the same package only.
    4. `public` - These attributes can be accessed by everybody.


* __C++__

    C++ has 3 access modifiers.
    1. `private` - These attributes can be accessed only inside the class.
    2. `protected` - These attributes can be accessed by derived classes.
    3. `public` - These attributes can be accessed by everybody.

* __C#__

    C# has 5 access modifiers
    1. `private` - These attributes can be accessed only inside the class.
    2. `protected internal` - These attributes can be accessed by same assembly and derived classes.
    3. `protected` - These attributes can be accessed by derived classes.
    4. `public internal` - These attributes can be accessed by the classes within the same assembly.
    5. `public` - These attributes can be accessed by everybody.

## Abstraction - Introduction
Abstraction is one of the main concepts in __Object Oriented Programming (OOP)__. This is the process of hiding the implementation details for the outsiders while showing only essential details. In another words, Abstraction is a technique to arrange the complexity of a program.

There are two basic type of abstraction:

1. Control abstraction

    This is done using sub-routines and control flow. We can call another function/method/routine (sub-routine) from a function/method to do a specific task, where that sub-routine is abstract.

2. Data abstraction

    This is done through various data structures and their implementations. We can create our own data structures to store our data, while keeping the implementation abstract.

In _OOP_ we use mix of control and function abstraction.



