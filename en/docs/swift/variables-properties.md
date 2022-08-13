---
title: "Variables & Properties"
slug: "variables--properties"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

**Properties**: Associated with a type

**Variables**: Not associated with a type

See the [Swift Programming Language iBook][1] for more information.


  [1]: https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Properties.html#//apple_ref/doc/uid/TP40014097-CH14-ID254

## Creating a Variable
Declare a new variable with `var`, followed by a name, type, and value:

    var num: Int = 10

Variables can have their values changed:

    num = 20 // num now equals 20

Unless they're defined with `let`:

    let num: Int = 10 // num cannot change

Swift infers the type of variable, so you don't always have to declare variable type:

    let ten = 10 // num is an Int
    let pi = 3.14 // pi is a Double
    let floatPi: Float = 3.14 // floatPi is a Float

Variable names aren't restricted to letters and numbers - they can also contain most other unicode characters, although there are some restrictions

>Constant and variable names cannot contain whitespace characters, mathematical symbols, arrows, private-use (or invalid) Unicode code points, or line- and box-drawing characters. Nor can they begin with a number
>
><sub>**Source** [developer.apple.com](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html)</sub>

    var π: Double = 3.14159
    var 🍎🍏: String = "Apples"

## Property Observers
Property observers respond to changes to a property's value.

    var myProperty = 5 {
        willSet {
            print("Will set to \(newValue). It was previously \(myProperty)")
        }
        didSet {
            print("Did set to \(myProperty). It was previously \(oldValue)")
        }
    }
    myProperty = 6
    // prints: Will set to 6, It was previously 5
    // prints: Did set to 6. It was previously 5

- `willSet` is called **before** `myProperty` is set. The new value is available as `newValue`, and the old value is still available as `myProperty`.
- `didSet` is called **after** `myProperty` is set. The old value is available as `oldValue`, and the new value is now available as `myProperty` .

>**Note:** `didSet` and `willSet` will not be called in the following cases:
> * Assigning an initial value
> * Modifying the variable within its own `didSet` or `willSet`

- The parameter names for `oldValue` and `newValue` of `didSet` and `willSet` can also be declared to increase readability:


    var myFontSize = 10 {
        willSet(newFontSize) {
            print("Will set font to \(newFontSize), it was \(myFontSize)")
        }
        didSet(oldFontSize) {
            print("Did set font to \(myFontSize), it was \(oldFontSize)")
        }
    }

>**Caution:** While it is supported to declare setter parameter names, one should be cautious not to mix names up:
>* `willSet(oldValue)` and `didSet(newValue)` are entirely legal, but will considerably confuse readers of your code. 

## Lazy Stored Properties
Lazy stored properties have values that are not calculated until first accessed. This is useful for memory saving when the variable's calculation is computationally expensive. You declare a lazy property with `lazy`:

    lazy var veryExpensiveVariable = expensiveMethod()

Often it is assigned to a return value of a closure:

    lazy var veryExpensiveString = { () -> String in
        var str = expensiveStrFetch()
        str.expensiveManipulation(integer: arc4random_uniform(5))
        return str
    }()

Lazy stored properties must be declared with `var`.

## Property Basics
Properties can be added to a [class][1] or [struct][2] (technically [enums][3] too, see "Computed Properties" example). These add values that associate with instances of classes/structs:

    class Dog {
        var name = ""
    }

In the above case, instances of `Dog` have a property named `name` of type `String`. The property can be accessed and modified on instances of `Dog`:

    let myDog = Dog()
    myDog.name = "Doggy" // myDog's name is now "Doggy"

These types of properties are considered **stored properties**, as they store something on an object and affect its memory.

  [1]: https://www.wikiod.com/swift/classes
  [2]: https://www.wikiod.com/swift/structs
  [3]: https://www.wikiod.com/swift/enums

## Computed Properties
Different from stored properties, **computed properties** are built with a getter and a setter, performing necessary code when accessed and set. Computed properties must define a type:

    var pi = 3.14

    class Circle {
        var radius = 0.0
        var circumference: Double {
            get {
                return pi * radius * 2
            }
            set {
                radius = newValue / pi / 2
            }
        }
    }
    
    let circle = Circle()
    circle.radius = 1
    print(circle.circumference) // Prints "6.28"
    circle.circumference = 14
    print(circle.radius) // Prints "2.229..."

A read-only computed property is still declared with a `var`:

    var circumference: Double {
        get {
            return pi * radius * 2
        }
    }

Read-only computed properties can be shortened to exclude `get`:

    var circumference: Double {
        return pi * radius * 2
    }


  [1]: https://www.wikiod.com/swift/classes
  [2]: https://www.wikiod.com/swift/structs
  [3]: https://www.wikiod.com/swift/enums
  [4]: https://www.wikiod.com/swift/extensions

## Local and Global Variables
Local variables are defined within a function, method, or closure:

    func printSomething() {
        let localString = "I'm local!"
        print(localString)
    }

    func printSomethingAgain() {
        print(localString) // error
    }

Global variables are defined outside of a function, method, or closure, and are not defined within a type (think outside of all brackets). They can be used anywhere:

    let globalString = "I'm global!"
    print(globalString)

    func useGlobalString() {
        print(globalString) // works!
    }

    for i in 0..<2 {
        print(globalString) // works!
    }

    class GlobalStringUser {
        var computeGlobalString {
            return globalString // works!
        }
    }

Global variables are defined lazily (see "Lazy Properties" example).

## Type Properties
Type properties are properties on the type itself, not on the instance. They can be both stored or computed properties. You declare a type property with `static`:

    struct Dog {
        static var noise = "Bark!"
    }
    
    print(Dog.noise) // Prints "Bark!"

In a class, you can use the `class` keyword instead of `static` to make it overridable. However, you can only apply this on computed properties:

    class Animal {
        class var noise: String {
            return "Animal noise!"
        }
    }
    class Pig: Animal {
        override class var noise: String {
            return "Oink oink!"
        }
    }

This is used often with the [singleton pattern][1].


  [1]: https://www.wikiod.com/swift/classes

