---
title: "Generics"
slug: "generics"
draft: false
images: []
weight: 9874
type: docs
toc: true
---

>Generic code enables you to write flexible, reusable functions and types that can work with any type, subject to requirements that you define. You can write code that avoids duplication and expresses its intent in a clear, abstracted manner.
>
>Generics are one of the most powerful features of Swift, and much of the Swift standard library is built with generic code. For example, Swift's `Array` and `Dictionary` types are both generic collections. You can create an array that holds `Int` values, or an array that holds `String` values, or indeed an array for any other type that can be created in Swift. Similarly, you can create a dictionary to store values of any specified type, and there are no limitations on what that type can be.
>
><sub>**Source:** [Apple's Swift Programming Language][1]</sub>


  [1]: https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Generics.html

## The Basics of Generics

[Generics][1] are placeholders for types, allowing you to write flexible code that can be applied across multiple types. The advantage of using generics over [`Any`][2] is that they still allow the compiler to enforce strong type-safety.

A generic placeholder is defined within angle brackets `<>`.

## Generic Functions

For [functions][3], this placeholder is placed after the function name:

<pre><code>/// Picks one of the inputs at random, and returns it
func pickRandom<b>&#60;T&#62;</b>(_ a:<b>T</b>, _ b:<b>T</b>) -> <b>T</b> {
    return arc4random_uniform(2) == 0 ? a : b
}</code></pre>

In this case, the generic placeholder is `T`. When you come to call the function, Swift can infer the type of `T` for you (as it simply acts as a placeholder for an actual type).

    let randomOutput = pickRandom(5, 7) // returns an Int (that's either 5 or 7)

Here we’re passing two integers to the function. Therefore Swift is inferring `T == Int` – thus the function signature is inferred to be `(Int, Int) -> Int`.

Because of the strong type safety that generics offer – both the arguments and return of the function must be the *same* type. Therefore the following will not compile:

    struct Foo {}
    
    let foo = Foo()
    
    let randomOutput = pickRandom(foo, 5) // error: cannot convert value of type 'Int' to expected argument type 'Foo'
    
## Generic Types

In order to use generics with [classes][4], [structs][5] or [enums][6], you can define the generic placeholder after the type name.

<pre><code>class Bar<b>&#60;T&#62;</b> {
    var baz : <b>T</b>
    
    init(baz:<b>T</b>) {
        self.baz = baz
    }
}</code></pre>

This generic placeholder will require a type when you come to use the class `Bar`. In this case, it can be inferred from the initialiser `init(baz:T)`.

    let bar = Bar(baz: "a string") // bar's type is Bar<String>

Here the generic placeholder `T` is inferred to be of type `String`, thus creating a `Bar<String>` instance. You can also specify the type explicitly:

<pre><code>let bar = Bar<b>&#60;String&#62;</b>(baz: "a string")</code></pre>

When used with a type, the given generic placeholder will keep its type for the entire lifetime of the given instance, and cannot be changed after initialisation. Therefore when you access the property `baz`, it will always be of type `String` for this given instance.

    let str = bar.baz // of type String

## Passing Around Generic Types

When you come to pass around generic types, in most cases you have to be explicit about the generic placeholder type you expect. For example, as a function input:

<pre><code>func takeABarInt(bar:Bar<b>&#60;Int&#62;</b>) {
    ...
}</code></pre>

This function will only accept a `Bar<Int>`. Attempting to pass in a `Bar` instance where the generic placeholder type is not `Int` will result in a compiler error.

## Generic Placeholder Naming

Generic placeholder names are not just limited to single letters. If a given placeholder represents a meaningful concept, you should give it a descriptive name. For example, Swift’s [`Array`][7] has a generic placeholder called `Element`, which defines the element type of a given `Array` instance.

<pre><code>public struct Array<b>&#60;Element&#62;</b> : RandomAccessCollection, MutableCollection {
    ...
}</code></pre>

  [1]: https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Generics.html#//apple_ref/doc/uid/TP40014097-CH26-ID179
  [2]: https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TypeCasting.html#//apple_ref/doc/uid/TP40014097-CH22-ID342
  [3]: https://www.wikiod.com/swift/functions
  [4]: https://www.wikiod.com/swift/classes
  [5]: https://www.wikiod.com/swift/structs
  [6]: https://www.wikiod.com/swift/enums
  [7]: https://developer.apple.com/library/watchos/documentation/Swift/Reference/Swift_Array_Structure/index.html

## Constraining Generic Placeholder Types
It is possible to force the type parameters of a generic class to [implement a protocol](https://www.wikiod.com/swift/protocols), for example, [`Equatable`](https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_Equatable_Protocol/index.html)

<pre><code>class MyGenericClass<b>&lt;Type: Equatable&gt;</b>{
    
    var value: <b>Type</b>
    init(value: <b>Type</b>){
        self.value = value
    }
    
    func getValue() -> <b>Type</b>{
        return self.value
    }

    func valueEquals(anotherValue: <b>Type</b>) -> Bool{
        return self.value == anotherValue
    }
}</code></pre>

Whenever we create a new `MyGenericClass`, the type parameter has to implement the `Equatable` protocol (ensuring the type parameter can be compared to another variable of the same type using `==`)

<pre><code>let myFloatGeneric = MyGenericClass&lt;<b>Double</b>&gt;(value: 2.71828) // valid
let myStringGeneric = MyGenericClass&lt;<b>String</b>&gt;(value: "My String") // valid

// "Type [Int] does not conform to protocol 'Equatable'"
let myInvalidGeneric = MyGenericClass&lt;<b>[Int]</b>&gt;(value: [2]) 

let myIntGeneric = MyGenericClass&lt;<b>Int</b>&gt;(value: 72)
print(myIntGeneric.valueEquals(72)) // true
print(myIntGeneric.valueEquals(-274)) // false

// "Cannot convert value of type 'String' to expected argument type 'Int'"
print(myIntGeneric.valueEquals("My String"))
</code></pre>

## Generic Class Examples
A generic class with the type parameter `Type`
 
<pre><code>class MyGenericClass<b>&lt;Type&gt;</b>{
 
    var value: <b>Type</b>
    init(value: <b>Type</b>){
        self.value = value
    }
 
    func getValue() -> <b>Type</b>{
        return self.value
    }
 
    func setValue(value: <b>Type</b>){
        self.value = value
    }
}</code></pre>
 
 We can now create new objects using a type parameter
 
<pre><code>let myStringGeneric = MyGenericClass<b>&lt;String&gt;</b>(value: "My String Value")
let myIntGeneric = MyGenericClass<b>&lt;Int&gt;</b>(value: 42)
 
print(myStringGeneric.getValue()) // "My String Value"
print(myIntGeneric.getValue()) // 42
 
myStringGeneric.setValue("Another String")
myIntGeneric.setValue(1024)
 
print(myStringGeneric.getValue()) // "Another String"
print(myIntGeneric.getValue()) // 1024</code></pre>
 
Generics can also be created with multiple type parameters
 
<pre><code>class AnotherGenericClass<b>&lt;TypeOne, TypeTwo, TypeThree&gt;</b>{
 
    var value1: <b>TypeOne</b>
    var value2: <b>TypeTwo</b>
    var value3: <b>TypeThree</b>
    init(value1: <b>TypeOne</b>, value2: <b>TypeTwo</b>, value3: <b>TypeThree</b>){
        self.value1 = value1
        self.value2 = value2
        self.value3 = value3
    }
 
    func getValueOne() -> <b>TypeOne</b>{return self.value1}
    func getValueTwo() -> <b>TypeTwo</b>{return self.value2}
    func getValueThree() -> <b>TypeThree</b>{return self.value3}
}</code></pre>
 
 And used in the same way
 
<pre><code>let myGeneric = AnotherGenericClass<b>&lt;String, Int, Double&gt;</b>(value1: "Value of pi", value2: 3, value3: 3.14159)
 
print(myGeneric.getValueOne() is String) // true
print(myGeneric.getValueTwo() is Int) // true
print(myGeneric.getValueThree() is Double) // true
print(myGeneric.getValueTwo() is String) // false
 
print(myGeneric.getValueOne()) // "Value of pi"
print(myGeneric.getValueTwo()) // 3
print(myGeneric.getValueThree()) // 3.14159</code></pre>


## Using Generics to Simplify Array Functions
A function that extends the functionality of the array by creating an object oriented remove function.

    // Need to restrict the extension to elements that can be compared.
    // The `Element` is the generics name defined by Array for its item types.
    // This restriction also gives us access to `index(of:_)` which is also
    // defined in an Array extension with `where Element: Equatable`.
    public extension Array where Element: Equatable {
        /// Removes the given object from the array.
        mutating func remove(_ element: Element) {
            if let index = self.index(of: element ) {
                self.remove(at: index)
            } else {
                fatalError("Removal error, no such element:\"\(element)\" in array.\n")
            }
        }
    }

**Usage**

    var myArray = [1,2,3]
    print(myArray)
 
    // Prints [1,2,3]

Use the function to remove an element without need for an index. Just pass the object to remove.

    myArray.remove(2)
    print(myArray)
    
    // Prints [1,3]

## Generic Class Inheritance
Generic classes can be inherited:

    // Models
    class MyFirstModel {
    }
    
    class MySecondModel: MyFirstModel {
    }
    
    // Generic classes
    class MyFirstGenericClass<T: MyFirstModel> {
        
        func doSomethingWithModel(model: T) {
            // Do something here
        }
        
    }
    
    class MySecondGenericClass<T: MySecondModel>: MyFirstGenericClass<T> {
        
        override func doSomethingWithModel(model: T) {
            super.doSomethingWithModel(model)
            
            // Do more things here
        }
        
    }

## Use generics to enhance type-safety
Let's take this example without using generics

    protocol JSONDecodable {
        static func from(_ json: [String: Any]) -> Any?
    }

The protocol declaration seems fine unless you actually use it.

    let myTestObject = TestObject.from(myJson) as? TestObject

Why do you have to cast the result to `TestObject`? Because of the `Any` return type in the protocol declaration.

By using generics you can avoid this problem that can cause runtime errors (and we don't want to have them!)

    protocol JSONDecodable {
        associatedtype Element 
        static func from(_ json: [String: Any]) -> Element?
    }

    struct TestObject: JSONDecodable {
        static func from(_ json: [String: Any]) -> TestObject? {
        }
    }

    let testObject = TestObject.from(myJson) // testObject is now automatically `TestObject?`

## Advanced Type Constraints
It's possible to specify several type constraints for generics using the `where` clause:

    func doSomething<T where T: Comparable, T: Hashable>(first: T, second: T) {
        // Access hashable function
        guard first.hashValue == second.hashValue else {
            return
        }
        // Access comparable function
        if first == second {
            print("\(first) and \(second) are equal.")
        }
    }

It's also valid to write the `where` clause after the argument list:

    func doSomething<T>(first: T, second: T) where T: Comparable, T: Hashable {
        // Access hashable function
        guard first.hashValue == second.hashValue else {
            return
        }
        // Access comparable function
        if first == second {
            print("\(first) and \(second) are equal.")
        }
    }

Extensions can be restricted to types that satisfy conditions. The function is only available to instances which satisfy the type conditions:

    // "Element" is the generics type defined by "Array". For this example, we
    // want to add a function that requires that "Element" can be compared, that
    // is: it needs to adhere to the Equatable protocol.
    public extension Array where Element: Equatable {
        /// Removes the given object from the array.
        mutating func remove(_ element: Element) {
            // We could also use "self.index(of: element)" here, as "index(of:_)"
            // is also defined in an extension with "where Element: Equatable".
            // For the sake of this example, explicitly make use of the Equatable.
            if let index = self.index(where: { $0 == element }) {
                self.remove(at: index)
            } else {
                fatalError("Removal error, no such element:\"\(element)\" in array.\n")
            }
        }
    }




