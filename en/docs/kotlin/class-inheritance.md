---
title: "Class Inheritance"
slug: "class-inheritance"
draft: false
images: []
weight: 9854
type: docs
toc: true
---

Any object-oriented programming language has some form of class inheritance. Let me revise:

Imagine you had to program a bunch of fruit: `Apples`, `Oranges` and `Pears`. They all differ in size, shape and color, that's why we have different classes.

But let's say their differences don't matter for a second and you just want a `Fruit`, no matter which exactly? What return type would `getFruit()` have?

The answer is class `Fruit`. 
We create a new class and make all fruits inherit from it!

## Syntax
 - open {Base Class}
 - class {Derived Class} : {Base Class}({Init Arguments})
 - override {Function Definition}
 - {DC-Object} is {Base Class} == true
 

## Parameters
Parameter|Details
---|---
Base Class|Class that is inherited from
Derived Class|Class that inherits from Base Class
Init Arguments|Arguments passed to constructor of Base Class
Function Definition|Function in Derived Class that has different code than the same in the Base Class
DC-Object|”Derived Class-Object“ Object that has the type of the Derived Class

## Basics: the 'open' keyword
In Kotlin, classes are **final by default** which means they cannot be inherited from.

To allow inheritance on a class, use the `open` keyword.

```
open class Thing {
    // I can now be extended!
}
```

> **Note:** abstract classes, sealed classes and interfaces will
> be `open` by default.

## Inheriting fields from a class
### Defining the base class:

    open class BaseClass {
        val x = 10
    }
    
### Defining the derived class:

    class DerivedClass: BaseClass() {
        fun foo() {
            println("x is equal to " + x)
        }
    }
    
### Using the subclass:

    fun main(args: Array<String>) {
        val derivedClass = DerivedClass()
        derivedClass.foo() // prints: 'x is equal to 10'
    }




## Inheriting methods from a class
### Defining the base class:
```
open class Person {
    fun jump() {
        println("Jumping...")
    }
}
```

### Defining the derived class:
```
class Ninja: Person() {
    fun sneak() {
        println("Sneaking around...")
    }
}
```
### The Ninja has access to all of the methods in Person

```
fun main(args: Array<String>) {
    val ninja = Ninja()
    ninja.jump()  // prints: 'Jumping...'
    ninja.sneak() // prints: 'Sneaking around...'
}
```

## Overriding properties and methods
Overriding properties (both read-only and mutable):
-----------

    abstract class Car {
        abstract val name: String;
        open var speed: Int = 0;
    }
    
    class BrokenCar(override val name: String) : Car() {
        override var speed: Int
            get() = 0
            set(value) {
                throw UnsupportedOperationException("The car is bloken")
            }
    }
    
    fun main(args: Array<String>) {
        val car: Car = BrokenCar("Lada")
        car.speed = 10
    }

Overriding methods:
---------------

    interface Ship {
        fun sail()
        fun sink()
    }
    
    object Titanic : Ship {
    
        var canSail = true
    
        override fun sail() {
            sink()
        }
    
        override fun sink() {
            canSail = false
        }
    }


