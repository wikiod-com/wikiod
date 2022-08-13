---
title: "Dependency Inversion Principle (DIP)"
slug: "dependency-inversion-principle-dip"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

The principle basically says, `Class` should depend on abstractions (e.g interface, abstract classes), not specific details (implementations). That means, You should let the caller create the dependencies instead of letting the class itself create the dependencies.

## Dependency Inversion Principle C#
[![enter image description here][1]][1]

[1]: https://i.stack.imgur.com/NWXzv.png

To understand  *Dependency Inversion Principle (DIP)* we need to clear concept about *Inversion Of Control(IOC)* and *Dependency Injection(DI)*. So here we discuss all about the terms with *Dependency Inversion Principle (DIP)*.

**Inversion Of Control(IOC)** :
The control or logic which is not the part of that entity is taken care by someone else.
Or, IOC is a programming technique where the unconcerned LOGIC is delegated to some other entity.

**Dependency Injection(DI)** :
Dependency injection is a technique which helps to inject dependent objects of a class that makes architecture more loosely coupled.

There are lots of IOC container for .NET framework that helps us to resolved dependency. Some are listed [here](https://www.hanselman.com/blog/ListOfNETDependencyInjectionContainersIOC.aspx).

