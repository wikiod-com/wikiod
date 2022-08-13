---
title: "Extensions"
slug: "extensions"
draft: false
images: []
weight: 9881
type: docs
toc: true
---

You can read more about extensions in [The Swift Programming Language](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Extensions.html).

## What are Extensions?
[**Extensions**](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Extensions.html) are used to extend the functionality of existing types in Swift. Extensions can add subscripts, functions, initializers, and computed properties. They can also make types conform to [protocols](https://www.wikiod.com/swift/protocols).


Suppose you want to be able to compute the [factorial](https://en.wikipedia.org/wiki/Factorial) of an `Int`. You can add a computed property in an extension:

    extension Int {
        var factorial: Int {
            return (1..<self+1).reduce(1, combine: *)
        }
    }

Then you can access the property just as if it had been included in original Int API.

    let val1: Int = 10

    val1.factorial  // returns 3628800

## Variables and functions
Extensions can contain functions and computed/constant get variables. They are in the format

    extension ExtensionOf {
        //new functions and get-variables
    }

To reference the instance of the extended object, `self` can be used, just as it could be used 

To create an extension of `String` that adds a `.length()` function which returns the length of the string, for example

    extension String {
        func length() -> Int {
            return self.characters.count
        }
    }

    "Hello, World!".length() // 13

Extensions can also contain `get` variables. For example, adding a `.length` variable to the string which returns the length of the string

    extension String {
        var length: Int {
            get {
                return self.characters.count
            }
        }
    }

    "Hello, World!".length // 13

## Initializers in Extensions
Extensions can contain convenience initializers. For example, a failable initializer for `Int` that accepts a `NSString`:

    extension Int {
        init?(_ string: NSString) {
            self.init(string as String)  // delegate to the existing Int.init(String) initializer
        }
    }

    let str1: NSString = "42"
    Int(str1) // 42

    let str2: NSString = "abc"
    Int(str2) // nil

## Protocol extensions
A very useful feature of Swift 2.2 is having the ability of extending protocols.

It works pretty much like abstract classes when regarding a functionality you want to be available in all the classes that implements some protocol (without having to inherit from a base common class).

    protocol FooProtocol {
        func doSomething()
    }
    
    extension FooProtocol {
        func doSomething() {
            print("Hi")
        }
    }
    
    class Foo: FooProtocol {
        func myMethod() {
            doSomething() // By just implementing the protocol this method is available
        }
    }

This is also possible using generics.

## Subscripts
Extensions can add new subscripts to an existing type. 

This example gets the character inside a String using the given index:

<!-- if version [eq 2.2] -->
    extension String {
        subscript(index: Int) -> Character {
            let newIndex = startIndex.advancedBy(index)
            return self[newIndex]
        }
    }
    
    var myString = "StackOverFlow"
    print(myString[2]) // a
    print(myString[3]) // c
<!-- end version if -->

<!-- if version [eq 3.0] -->

    extension String {
        subscript(offset: Int) -> Character {
            let newIndex = self.index(self.startIndex, offsetBy: offset)
            return self[newIndex]
        }
    }
    
    var myString = "StackOverFlow"
    print(myString[2]) // a
    print(myString[3]) // c

<!-- end version if -->

## Restrictions
It is possible to write a method on a generic type that is more restrictive using where sentence.

    extension Array where Element: StringLiteralConvertible {
      func toUpperCase() -> [String] {
          var result = [String]()
          for value in self {
              result.append(String(value).uppercaseString)
          }
          return result
        }        
    }

Example of use

    let array = ["a","b","c"]
    let resultado = array.toUpperCase()

## What are extensions and when to use them
Extensions add new functionality to an existing class, structure, enumeration, or protocol type. This includes the ability to extend types for which you do not have access to the original source code.

Extensions in Swift can:   
- Add computed properties and computed type properties   
- Define instance methods and type methods   
- Provide new initializers   
- Define subscripts   
- Define and use new nested types   
- Make an existing type conform to a protocol   

When to use Swift Extensions:

- Additional functionality to Swift
- Additional functionality to UIKit / Foundation
- Additional functionality without messing with other persons code
- Breakdown classes into: Data / Functionality / Delegate

When not to use:
- Extend your own classes from another file

Simple example:

    extension Bool {
        public mutating func toggle() -> Bool {
            self = !self
            return self
        }
    }
    
    var myBool: Bool = true
    print(myBool.toggle()) // false
[Source][1]


  [1]: https://github.com/goktugyil/EZSwiftExtensions/blob/master/Sources/BoolExtensions.swift

