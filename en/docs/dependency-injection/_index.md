---
title : dependency-injection Tutorial
slug : dependency-injection-tutorial
weight : 9967
draft : false
images : []
type : docs
---

In object-oriented programming, objects often depend on other objects in order to do things.

**Dependency Injection** (DI) is giving an object the things that it depends on so that it doesn't have to worry about getting them itself. That is, the dependencies are *injected* into the object. This is most often done with *constructor injection* or *property injection*.

Dependency injection is a form of **Inversion of Control** (IoC). IoC is a broader term that describes a pattern of software design.

In traditional procedural programming, the flow of control follows logically in steps. The control is in the hands of the object or function performing operations. Step-by-step the program performs a series of operations that it controls explicitly.

Instead of the object or function detailing every step, the flow of control can be *inverted* by making the operations be performed by more generic and abstract objects - usually a framework that is broader in scope.

