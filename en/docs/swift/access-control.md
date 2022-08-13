---
title: "Access Control"
slug: "access-control"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Syntax
 - private class Project
 - let car = Car("Ford", model: "Escape") //default internal
 - public enum Genre 
 - private func calculateMarketCap() 
 - override internal func setupView()
 - private(set) var area = 0




1. ***Basic Remark:***

Below are the three access levels from highest access (least-restrictive) to lowest access (most-restrictive)

**Public** access allows access to classes, structs, variables, etc from any file within the model, but more importantly outside the module if the external file imports the module containing the public access code. It is popular to use public access when you create a framework. 

**Internal** access allows files only with the module of the entities to use the entities. All entities have **internal** acccess level by default (with a few exceptions). 

**Private** access prevents the entity from being used outside of that file. 


2. ***Subclassing Remark:***

A subclass cannot have a higher access than its superclass. 

3. ***Getter & Setter Remark:***

If property’s setter is private the getter is internal (which is the default). Also you can assign access level for both the getter and the setter. These principles also apply to *subscripts* as well

4. ***General Remark:***

Other entity types include: Initializers, Protocols, Extensions, Generics, and Type Aliases





## Basic Example using a Struct
<!-- if version [gte 3.0] -->

In Swift 3 there are multiple access-levels. This example uses them all except for `open`:

    public struct Car {
        
        public let make: String
        let model: String //Optional keyword: will automatically be "internal"
        private let fullName: String
        fileprivate var otherName: String
        
        public init(_ make: String, model: String) {
            self.make = make
            self.model = model
            self.fullName = "\(make)\(model)"
            self.otherName = "\(model) - \(make)"
        }
    }

Assume `myCar` was initialized like this:

    let myCar = Car("Apple", model: "iCar")

## Car.make (public) ##

    print(myCar.make)

This print will work everywhere, including targets that import `Car`.

## Car.model (internal) ##

    print(myCar.model)

This will compile if the code is in the same target as `Car`.

## Car.otherName (fileprivate) ##

    print(myCar.otherName)

This will only work if the code is *in the same file* as `Car`.

## Car.fullName (private) ##

    print(myCar.fullName)

This won't work in Swift 3. `private` properties can only be accessed within the same `struct`/`class`.

<!-- end version if -->

<!-- if version [lte 2.3] -->
    public struct Car {
    
        public let make: String       //public
        let model: String             //internal
        private let fullName: String! //private 
    
        public init(_ make: String, model model: String) {
            self.make = make
            self.model = model
            self.fullName = "\(make)\(model)"
        }
    }

If the entity has multiple associated access levels, Swift looks for the lowest level of access. If a private variable exists in a public class, the variable will still be considered private.

## Subclassing Example


    public class SuperClass {
        private func secretMethod() {}
    }
     
    internal class SubClass: SuperClass {
        override internal func secretMethod() {
            super.secretMethod()
        }
    }

## Getters and Setters Example
    struct Square {
        private(set) var area = 0

        var side: Int = 0 {
            didSet {
                area = side*side
            }
        }
    }

    public struct Square {
        public private(set) var area = 0
        public var side: Int = 0 {
            didSet {
                area = side*side
            }
        }
        public init() {}
    }



