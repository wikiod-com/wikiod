---
title: "Initializers"
slug: "initializers"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Setting default property values
You can use an initializer to set default property values:

    struct Example {
        var upvotes: Int
        init() {
            upvotes = 42
        }
    }
    let myExample = Example() // call the initializer
    print(myExample.upvotes) // prints: 42

Or, specify default property values as a part of the property's declaration:

    struct Example {
        var upvotes = 42 // the type 'Int' is inferred here
    }

Classes and structs **must** set all stored properties to an appropriate initial value by the time an instance is created. This example will not compile, because the initializer did not give an initial value for `downvotes`:

    struct Example {
        var upvotes: Int
        var downvotes: Int
        init() {
             upvotes = 0
        } // error: Return from initializer without initializing all stored properties
    }



## Customizing initialization with paramaters
    struct MetricDistance {
        var distanceInMeters: Double

        init(fromCentimeters centimeters: Double) {
            distanceInMeters = centimeters / 100
        }
        init(fromKilometers kilos: Double) {
            distanceInMeters = kilos * 1000
        }
    }

    let myDistance = MetricDistance(fromCentimeters: 42)
    // myDistance.distanceInMeters is 0.42
    let myOtherDistance = MetricDistance(fromKilometers: 42)
    // myOtherDistance.distanceInMeters is 42000

Note that you cannot omit the parameter labels:

    let myBadDistance = MetricDistance(42) // error: argument labels do not match any available overloads

In order to allow omission of parameter labels, use an underscore `_` as the label:
    
    struct MetricDistance {
        var distanceInMeters: Double
        init(_ meters: Double) {
            distanceInMeters = meters
        }
    }
    let myDistance = MetricDistance(42) // distanceInMeters = 42


If your argument labels share names with one or more properties, use `self` to explicitly set the property values:

    struct Color {
        var red, green, blue: Double
        init(red: Double, green: Double, blue: Double) {
            self.red = red
            self.green = green
            self.blue = blue
        }
    }

## Convenience init
Swift classes supports having multiple ways of being initialized.
Following Apple's specs this 3 rules must be respected:

 1. A designated initializer must call a designated initializer from its immediate superclass.
[![For First Rule][1]][1]
 2. A convenience initializer must call another initializer from the same class.
 3. A convenience initializer must ultimately call a designated initializer.
   [![For Second and Third Rule][2]][2]


    class Foo {

        var someString: String
        var someValue: Int
        var someBool: Bool
    
        // Designated Initializer
        init(someString: String, someValue: Int, someBool: Bool)
        {
            self.someString = someString
            self.someValue = someValue
            self.someBool = someBool
        }
    
        // A convenience initializer must call another initializer from the same class.
        convenience init()
        {
            self.init(otherString: "")
        }
        
        // A convenience initializer must ultimately call a designated initializer.
    convenience init(otherString: String)
        {
            self.init(someString: otherString, someValue:  0, someBool: false)
        }
    }


    class Baz: Foo
    {
        var someFloat: Float
        
        // Designed initializer
        init(someFloat: Float)
        {
            self.someFloat = someFloat
            
            // A designated initializer must call a designated initializer from its immediate superclass.
            super.init(someString: "", someValue: 0, someBool: false)
        }
        
        // A convenience initializer must call another initializer from the same class.
        convenience init()
        {
            self.init(someFloat: 0)
        }
    }

##  Designated Initializer
    let c = Foo(someString: "Some string", someValue: 10, someBool: true)

## Convenience init()
    let a = Foo()

## Convenience init(otherString: String)
    let b = Foo(otherString: "Some string")
    
## Designated Initializer (will call the superclass Designated Initializer)
    let d = Baz(someFloat: 3)
    
## Convenience init()
    let e = Baz()

---
_Image source: [The Swift Programming Languag][3]e_


  [1]: https://i.stack.imgur.com/7ie2O.png
  [2]: https://i.stack.imgur.com/y411s.png
  [3]: https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Initialization.html

## Throwable Initilizer
   
   Using Error Handling to make Struct(or class) initializer as throwable initializer:
     
Example Error Handling enum:

    enum ValidationError: Error {
        case invalid
    }


You can use Error Handling enum to check the parameter for the Struct(or class)  meet expected requirement

    struct User {
        let name: String
    
        init(name: String?) throws {
    
            guard let name = name else { 
               ValidationError.invalid
            }
    
            self.name = name
        }
    }

Now, you can use throwable initializer by:

    do {
       let user = try User(name: "Sample name")
                
       // success
    }
    catch ValidationError.invalid {
         // handle error
    }



