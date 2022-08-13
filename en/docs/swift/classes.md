---
title: "Classes"
slug: "classes"
draft: false
images: []
weight: 9939
type: docs
toc: true
---

The `init()` is a special method in classes which is used to declare an initializer for the class. More information can be found here: [Initializers][1]


  [1]: https://www.wikiod.com/swift/initializers

## Defining a Class
You define a class like this:

    class Dog {}

A class can also be a subclass of another class:

    class Animal {}
    class Dog: Animal {}

In this example, `Animal` could also be a [protocol][1] that `Dog` conforms to.


  [1]: https://www.wikiod.com/swift/protocols

## Properties and Methods
Classes can define properties that instances of the class can use. In this example, `Dog` has two properties: `name` and `dogYearAge`:

    class Dog {
        var name = ""
        var dogYearAge = 0
    }

You can access the properties with dot syntax:

    let dog = Dog()
    print(dog.name)
    print(dog.dogYearAge)

Classes can also define [methods][1] that can be called on the instances, they are declared similar to normal [functions][2], just inside the class:

    class Dog {
        func bark() {
            print("Ruff!")
        }
    }

Calling methods also uses dot syntax:

    dog.bark()


  [1]: https://www.wikiod.com/swift/functions#Methods
  [2]: https://www.wikiod.com/swift/functions

## Reference Semantics
Classes are **reference types**, meaning that multiple variables can refer to the same instance.

<pre><code>class Dog {
    var name = ""
}

let firstDog = Dog()
firstDog.name = "Fido"

let otherDog = firstDog  // otherDog points to <b>the same</b> Dog instance
otherDog.name = "Rover"  // modifying otherDog <b>also modifies firstDog</b>

print(firstDog.name)  // prints "Rover"</code></pre>

Because classes are reference types, even if the class is a constant, its variable properties can still be modified.

    class Dog {
        var name: String // name is a variable property.
        let age: Int // age is a constant property.
        init(name: String, age: Int) {
            self.name = name
            self.age = age
        }
    }
    
    let constantDog = Dog(name: "Rover", age: 5)// This instance is a constant.
    var variableDog = Dog(name: "Spot", age 7)// This instance is a variable.

    constantDog.name = "Fido" // Not an error because name is a variable property.
    constantDog.age = 6 // Error because age is a constant property.
    constantDog = Dog(name: "Fido", age: 6)
    /* The last one is an error because you are changing the actual reference, not
    just what the reference points to. */
    
    variableDog.name = "Ace" // Not an error because name is a variable property.
    variableDog.age = 8 // Error because age is a constant property.
    variableDog = Dog(name: "Ace", age: 8)
    /* The last one is not an error because variableDog is a variable instance and
    therefore the actual reference can be changed. */

Test whether two objects are *identical* (point to the exact same instance) using `===`:

    class Dog: Equatable {
        let name: String
        init(name: String) { self.name = name }
    }
    
    // Consider two dogs equal if their names are equal.
    func ==(lhs: Dog, rhs: Dog) -> Bool {
        return lhs.name == rhs.name
    }
    
    // Create two Dog instances which have the same name.
    let spot1 = Dog(name: "Spot")
    let spot2 = Dog(name: "Spot")

    spot1 == spot2   // true, because the dogs are equal
    spot1 != spot2   // false

    spot1 === spot2  // false, because the dogs are different instances
    spot1 !== spot2  // true



## Classes and Multiple Inheritance
Swift does not support multiple inheritance. That is, you cannot inherit from more than one class.

    class Animal { ... }
    class Pet { ... }

    class Dog: Animal, Pet { ... } // This will result in a compiler error.

Instead you are encouraged to use composition when creating your types. This can be accomplished by using [protocols][1].


  [1]: https://www.wikiod.com/swift/protocols

## deinit


