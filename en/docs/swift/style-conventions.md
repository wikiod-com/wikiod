---
title: "Style Conventions"
slug: "style-conventions"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

Swift has an official style guide: [Swift.org API Design Guidelines](https://swift.org/documentation/api-design-guidelines/). Another popular guide is [The Official raywenderlich.com Swift Style Guide.](https://github.com/raywenderlich/swift-style-guide)

## Fluent Usage
<h1>Using natural language</h1>
Functions calls should be close to natural English language.

Example:

    list.insert(element, at: index) 

instead of 
    
    list.insert(element, position: index)
<hr>
<h1>Naming Factory Methods</h1>
Factory methods should begin with the prefix `make`.

Example:

    factory.makeObject()

<hr>
<h1>Naming Parameters in Initializers and Factory Methods</h1>
The name of the first argument should not be involved in naming a factory method or initializer.

Example:

    factory.makeObject(key: value)

Instead of:

    factory.makeObject(havingProperty: value)
<hr>
<h1>Naming according to side effects</h1>
<ul><li>Functions with side effects (mutating functions) should be named using verbs or nouns prefixed with <code>form-</code> .</li>
<li>Functions without side effects (nonmutating functions) should be named using nouns or verbs with the suffix <code>-ing</code> or <code>-ed</code>.
</ul>
Example:
Mutating functions:
    
    print(value)
    array.sort()                 // in place sorting
    list.add(value)              // mutates list
    set.formUnion(anotherSet)    // set is now the union of set and anotherSet

Nonmutating functions:

    let sortedArray = array.sorted()     // out of place sorting
    let union = set.union(anotherSet)    // union is now the union of set and another set
<hr>
<h1>Boolean functions or variables</h1>
Statements involving booleans should read as assertions.

Example:

    set.isEmpty
    line.intersects(anotherLine)

<hr>
<h1>Naming Protocols</h1>
<ul>
<li>Protocols describing what something is should be named using nouns.</li>
<li>Protocols describing capabilities should have <code>-able</code>, <code>-ible</code> or <code>-ing</code> as suffix.</li>
</ul>
Example:

    Collection        // describes that something is a collection
    ProgressReporting // describes that something has the capability of reporting progress
    Equatable         // describes that something has the capability of being equal to something
<hr>
<h1>Types and Properties</h1>
Types, variables and properties should read as nouns.

Example:

    let factory = ...
    let list = [1, 2, 3, 4]
    

## Clear Usage
<h1>Avoid Ambiguity</h1>
The name of classes, structures, functions and variables should avoid ambiguity. 

Example:

    extension List {
        public mutating func remove(at position: Index) -> Element {
            // implementation
        }
    }

The function call to this function will then look like this:

    list.remove(at: 42)

This way, ambiguity is avoided. If the function call would be just `list.remove(42)` it would be unclear, if an Element equal to 42 would be removed or if the Element at Index 42 would be removed.
<hr>
<h1>Avoid Redundancy</h1>
The name of functions should not contain redundant information.

A bad example would be:

    extension List {
        public mutating func removeElement(element: Element) -> Element? {
            // implementation
        }
    }

A call to the function may look like `list.removeElement(someObject)`. The variable `someObject` already indicates, that an Element is removed. It would be better for the function signature to look like this:

    extension List {
        public mutating func remove(_ member: Element) -> Element? {
            // implementation
        }
    }

The call to this function looks like this: `list.remove(someObject)`.
<hr>
<h1>Naming variables according to their role</h1>
Variables should be named by their role (e.g. supplier, greeting) instead of their type (e.g. factory, string, etc..)
<br><br>
<h2>High coupling between Protocol Name and Variable Names</h2>
If the name of the type describes its role in most cases (e.g. Iterator), the type should be named with the suffix `Type`. (e.g. IteratorType)
<hr>
<h1>Provide additional details when using weakly typed parameters</h1>
If the type of an object does not indicate its usage in a function call clearly, the function should be named with a preceding noun for every weakly typed parameter, describing its usage. <br>
Example:

    func addObserver(_ observer: NSObject, forKeyPath path: String)

to which a call would look like `object.addObserver(self, forKeyPath: path)

instead of 

    func add(_ observer: NSObject, for keyPath: String)

to which a call would look like `object.add(self, for: path)`

## Capitalization
<h1>Types & Protocols</h1>

Type and protocol names should start with an uppercase letter.

Example:

    protocol Collection {}
    struct String {}
    class UIView {}
    struct Int {}
    enum Color {}

<h1>Everything else...</h1>

Variables, constants, functions and enumeration cases should start with a lowercase letter.

Example:

    let greeting = "Hello"
    let height = 42.0

    enum Color {
        case red
        case green
        case blue
    }
    
    func print(_ string: String) {
        ...
    }

<h1>Camel Case:</h1>

All naming should use the appropriate camel case. Upper camel case for type/protocol names and lower camel case for everything else.

Upper Camel Case:

    protocol IteratorType { ... }

Lower Camel Case:

    let inputView = ...

<h1>Abbreviations</h1>

Abbreviations should be avoided unless commonly used (e.g. URL, ID).
If an abbreviation is used, all letters should have the same case.

Example:

    let userID: UserID = ...
    let urlString: URLString = ...

