---
title: "Traits"
slug: "traits"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Traits are structural construction objects in the Groovy language.

Traits enable implementation of interfaces. 
They are compatible with static type checking and compilation
Traits are behaved as interfaces with default implementations and state.

Declaration of a trait is by using the **trait** keyword.


----------


Traits methods scope support only **public** and **private** methods.



## Basic Usage
A `trait` is a reusable set of methods and fields that can be added to one or more classes. 

    trait BarkingAbility {
        String bark(){ "I'm barking!!" }
    }

They can be used like normal interfaces, using `implements` keyword:  

    class Dog implements BarkingAbility {}
    def d = new Dog()
    assert d.bark() = "I'm barking!!"

Also they can be used to implement multiple inheritance (avoiding diamond issue).

Dogs can scratch his head, so: 

    trait ScratchingAbility {
        String scratch() { "I'm scratching my head!!" }
    }

    class Dog implements BarkingAbility, ScratchingAbility {}
    def d = new Dog()
    assert d.bark() = "I'm barking!!"
    assert d.scratch() = "I'm scratching my head!!"

       



## Multiple inheritance problem
Class can implement multiple traits. In case if one trait defines method with the same signature like another trait, there is a multiple inheritance problem. In that case the method from **last declared trait** is used:

    trait Foo {
      def hello() {'Foo'}
    }
    trait Bar {
      def hello() {'Bar'}
    }

    class FooBar implements Foo, Bar {}

    assert new FooBar().hello() == 'Bar'

