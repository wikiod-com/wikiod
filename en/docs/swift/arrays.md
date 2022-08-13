---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9769
type: docs
toc: true
---

Array is an ordered, random-access collection type.

Arrays are one of the most commonly used data types in an app. We use the Array type to hold elements of a single type, the array's Element type. An array can store any kind of elements---from integers to strings to classes.

## Syntax
- Array&lt;Element&gt; // The type of an array with elements of type Element
- [Element] // Syntactic sugar for the type of an array with elements of type Element
- [element0, element1, element2, ... elementN] // An array literal 
- [[Element]&#40;&#41;](http://swiftdoc.org/v2.2/type/Array/#init) // Creates a new empty array of type [Element]
- [Array&#40;count:repeatedValue:&#41;](http://swiftdoc.org/v2.2/type/Array/#init-count_repeatedvalue_) // Creates an array of `count` elements, each initialized to `repeatedValue`
- [Array(_:)](http://swiftdoc.org/v2.2/type/Array/#init_) // Creates an array from an arbitrary sequence

Arrays are an *ordered collection* of values. Values may repeat but *must* be of the same type. 

## Basics of Arrays
[`Array`](https://developer.apple.com/reference/swift/array) is an ordered collection type in the Swift standard library. It provides O(1) random access and dynamic reallocation. Array is a [generic type][1], so the type of values it contains are known at compile time.

As `Array` is a [value type][3], its mutability is defined by whether it is annotated as a `var` (mutable) or `let` (immutable).

The type `[Int]` (meaning: an array containing `Int`s) is [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for `Array<T>`.

Read more about arrays in [The Swift Programming Language](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/CollectionTypes.html#//apple_ref/doc/uid/TP40014097-CH8-ID107).

# Empty arrays

The following three declarations are equivalent:

    // A mutable array of Strings, initially empty.

    var arrayOfStrings: [String] = []      // type annotation + array literal
    var arrayOfStrings = [String]()        // invoking the [String] initializer
    var arrayOfStrings = Array<String>()   // without syntactic sugar


# Array literals

An array literal is written with square brackets surrounding comma-separated elements:

    // Create an immutable array of type [Int] containing 2, 4, and 7
    let arrayOfInts = [2, 4, 7]

The compiler can usually infer the type of an array based on the elements in the literal, but explicit **type annotations** can override the default:

    let arrayOfUInt8s: [UInt8] = [2, 4, 7]   // type annotation on the variable
    let arrayOfUInt8s = [2, 4, 7] as [UInt8] // type annotation on the initializer expression
    let arrayOfUInt8s = [2 as UInt8, 4, 7]   // explicit for one element, inferred for the others

# Arrays with repeated values

    // An immutable array of type [String], containing ["Example", "Example", "Example"]
    let arrayOfStrings = Array(repeating: "Example",count: 3)

# Creating arrays from other sequences

    let dictionary = ["foo" : 4, "bar" : 6]

    // An immutable array of type [(String, Int)], containing [("bar", 6), ("foo", 4)]
    let arrayOfKeyValuePairs = Array(dictionary)


# Multi-dimensional arrays

In Swift, a multidimensional array is created by nesting arrays: a 2-dimensional array of `Int` is `[[Int]]` (or `Array<Array<Int>>`).

    let array2x3 = [
        [1, 2, 3],
        [4, 5, 6]
    ]
    // array2x3[0][1] is 2, and array2x3[1][2] is 6.

To create a multidimensional array of repeated values, use nested calls of the array initializer:

    var array3x4x5 = Array(repeating: Array(repeating: Array(repeating: 0,count: 5),count: 4),count: 3)


  [1]: https://www.wikiod.com/swift/generics
  [2]: https://www.wikiod.com/swift/dictionaries
  [3]: https://www.wikiod.com/swift/structs#Structs are value types

## Extracting values of a given type from an Array with flatMap(_:)
The `things` Array contains values of `Any` type.

    let things: [Any] = [1, "Hello", 2, true, false, "World", 3]

We can extract values of a given type and create a new Array of that specific type. Let's say we want to extract all the `Int(s)` and put them into an `Int` Array in a safe way.

    let numbers = things.flatMap { $0 as? Int }
   
Now `numbers` is defined as `[Int]`. The `flatMap` function discard all `nil` elements and the result thus contains only the following values:

    [1, 2, 3]

## Flattening the result of an Array transformation with flatMap(_:)
As well as being able to create an array by [filtering out `nil`][1] from the transformed elements of a sequence, there is also a version of [`flatMap(_:)`][2] that expects the transformation [closure][3] to return a sequence `S`.

    extension SequenceType {
        public func flatMap<S : SequenceType>(transform: (Self.Generator.Element) throws -> S) rethrows -> [S.Generator.Element]
    }

Each sequence from the transformation will be concatenated, resulting in an array containing the combined elements of each sequence – `[S.Generator.Element]`. 


## Combining the characters in an array of strings

For example, we can use it to take an array of prime strings and combine their characters into a single array:

    let primes = ["2", "3", "5", "7", "11", "13", "17", "19"]
    let allCharacters = primes.flatMap { $0.characters }
    // => "["2", "3", "5", "7", "1", "1", "1", "3", "1", "7", "1", "9"]"

Breaking the above example down:

1. `primes` is a `[String]` (As an array is a sequence, we can call `flatMap(_:)` on it).
2. The transformation closure takes in one of the elements of `primes`, a [`String`][5] (`Array<String>.Generator.Element`).
3. The closure then returns a sequence of type [`String.CharacterView`][6].
4. The result is then an array containing the combined elements of all the sequences from each of the transformation closure calls – `[String.CharacterView.Generator.Element]`.

## Flattening a multidimensional array

As `flatMap(_:)` will concatenate the sequences returned from the transformation closure calls, it can be used to flatten a multidimensional array – such as a 2D array into a 1D array, a 3D array into a 2D array etc.

This can simply be done by returning the given element `$0` (a nested array) in the closure:

    // A 2D array of type [[Int]]
    let array2D = [[1, 3], [4], [6, 8, 10], [11]]
    
    // A 1D array of type [Int]
    let flattenedArray = array2D.flatMap { $0 }
    
    print(flattenedArray) // [1, 3, 4, 6, 8, 10, 11]


  [1]: https://www.wikiod.com/swift/arrays#Extracting values of a given type from an Array with flatMap(_:)
  [2]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func--flatmap-s_-sequencetype_-self-generator-element-throws-s
  [3]: https://www.wikiod.com/swift/closures
  [4]: https://www.wikiod.com/swift/arrays#Transforming the elements of an Array with map(_:)
  [5]: http://swiftdoc.org/v2.2/type/String/
  [6]: http://swiftdoc.org/v2.2/type/String.CharacterView/

## Combining an Array's elements with reduce(_:combine:)
[`reduce(_:combine:)`][1] can be used in order to combine the elements of a sequence into a single value. It takes an initial value for the result, as well as a [closure][2] to apply to each element – which will return the new accumulated value.

For example, we can use it to sum an array of numbers:

    let numbers = [2, 5, 7, 8, 10, 4]
    
    let sum = numbers.reduce(0) {accumulator, element in
        return accumulator + element
    }
    
    print(sum) // 36

We're passing `0` into the initial value, as that's the logical initial value for a summation. If we passed in a value of `N`, the resulting `sum` would be `N + 36`. The closure passed to `reduce` has two arguments. `accumulator` is the current accumulated value, which is assigned the value that the closure returns at each iteration. `element` is the current element in the iteration.

As in this example, we're passing an `(Int, Int) -> Int` closure to `reduce`, which is simply outputting the addition of the two inputs – we can actually pass in the `+` operator directly, as operators are functions in Swift:
    
    let sum = numbers.reduce(0, combine: +)
    



  [1]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func--reduce_combine_
  [2]: https://www.wikiod.com/swift/closures





## Filtering out nil from an Array transformation with flatMap(_:)
You can use [`flatMap(_:)`][1] in a similar manner to [`map(_:)`][2] in order to create an array by applying a transform to a sequence's elements.

    extension SequenceType {
        public func flatMap<T>(@noescape transform: (Self.Generator.Element) throws -> T?) rethrows -> [T]
    }

The difference with this version of `flatMap(_:)` is that it expects the transform [closure][3] to return an [Optional][4] value `T?` for each of the elements. It will then safely unwrap each of these optional values, filtering out `nil` – resulting in an array of `[T]`.


For example, you can this in order to transform a `[String]` into a `[Int]` using [`Int`'s failable `String` initializer][5], filtering out any elements that cannot be converted:

    let strings = ["1", "foo", "3", "4", "bar", "6"]
    
    let numbersThatCanBeConverted = strings.flatMap { Int($0) }
    
    print(numbersThatCanBeConverted) // [1, 3, 4, 6]


You can also use `flatMap(_:)`'s ability to filter out `nil` in order to simply convert an array of optionals into an array of non-optionals:

    let optionalNumbers : [Int?] = [nil, 1, nil, 2, nil, 3]
    
    let numbers = optionalNumbers.flatMap { $0 }
    
    print(numbers) // [1, 2, 3]


  [1]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func--flatmap-t_-self-generator-element-throws-t
  [2]: https://www.wikiod.com/swift/arrays#Transforming the elements of an Array with map(_:)
  [3]: https://www.wikiod.com/swift/closures
  [4]: https://www.wikiod.com/swift/optionals
  [5]: http://swiftdoc.org/v2.2/type/Int/#init_radix_

## Lazily flattening a multidimensional Array with flatten()
We can use [`flatten()`][1] in order to [lazily][2] reduce the nesting of a multi-dimensional sequence.

For example, lazy flattening a 2D array into a 1D array:

    // A 2D array of type [[Int]]
    let array2D = [[1, 3], [4], [6, 8, 10], [11]]
    
    // A FlattenBidirectionalCollection<[[Int]]>
    let lazilyFlattenedArray = array2D.flatten()

    print(lazilyFlattenedArray.contains(4)) // true


In the above example, `flatten()` will return a [`FlattenBidirectionalCollection`][3], which will lazily apply the flattening of the array. Therefore [`contains(_:)`][4] will only require the first two nested arrays of `array2D` to be flattened – as it will short-circuit upon finding the desired element.




  [1]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-sequencetype-flatten
  [2]: https://en.wikipedia.org/wiki/Lazy_evaluation
  [3]: http://swiftdoc.org/v2.2/type/FlattenBidirectionalCollection/
  [4]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-equatable-contains_
  [5]: https://www.wikiod.com/swift/arrays#Transforming the elements of an Array with map(_:)
  [6]: http://swiftdoc.org/v2.2/type/Array/#init_


## Subscripting an Array with a Range
One can extract a series of consecutive elements from an Array using a Range.

    let words = ["Hey", "Hello", "Bonjour", "Welcome", "Hi", "Hola"]
    let range = 2...4
    let slice = words[range] // ["Bonjour", "Welcome", "Hi"]

Subscripting an Array with a Range returns an `ArraySlice`. It's a subsequence of the Array. 

In our example, we have an Array of Strings, so we get back `ArraySlice<String>`.

Although an ArraySlice conforms to `CollectionType` and can be used with `sort`, `filter`, etc, its purpose is not for long-term storage but for transient computations: it should be converted back into an Array as soon as you've finished working with it.

For this, use the `Array()` initializer:

    let result = Array(slice)

To sum up in a simple example without intermediary steps:

    let words = ["Hey", "Hello", "Bonjour", "Welcome", "Hi", "Hola"]
    let selectedWords = Array(words[2...4]) // ["Bonjour", "Welcome", "Hi"]

## Removing element from an array without knowing it's index
Generally, if we want to remove an element from an array, we need to know it's index so that we can remove it easily using `remove(at:)` function.

But what if we don't know the index but we know the value of element to be removed!

So here is the simple extension to an array which will allow us to remove an element from array easily without knowing it's index:

**Swift3**
-----

    extension Array where Element: Equatable {
    
        mutating func remove(_ element: Element) {
            _ = index(of: element).flatMap {
                self.remove(at: $0)
            }
        }
    }

e.g.

        var array = ["abc", "lmn", "pqr", "stu", "xyz"]
        array.remove("lmn")
        print("\(array)")    //["abc", "pqr", "stu", "xyz"]
        
        array.remove("nonexistent")
        print("\(array)")    //["abc", "pqr", "stu", "xyz"]
        //if provided element value is not present, then it will do nothing!

Also if, by mistake, we did something like this: `array.remove(25)`
i.e. we provided value with different data type, compiler will throw an error mentioning-  
`cannot convert value to expected argument type`

## Filtering an Array
You can use the [`filter(_:)`][1] method on [`SequenceType`][2] in order to create a new array containing the elements of the sequence that satisfy a given predicate, which can be provided as a [closure][3].

For example, filtering even numbers from an `[Int]`:

    let numbers = [22, 41, 23, 30]
    
    let evenNumbers = numbers.filter { $0 % 2 == 0 }
    
    print(evenNumbers)  // [22, 30]

Filtering a `[Person]`, where their age is less than 30:

    struct Person {
        var age : Int
    }
    
    let people = [Person(age: 22), Person(age: 41), Person(age: 23), Person(age: 30)]
    
    let peopleYoungerThan30 = people.filter { $0.age < 30 }
    
    print(peopleYoungerThan30) // [Person(age: 22), Person(age: 23)]
     


  [1]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func-filter_
  [2]: http://swiftdoc.org/v2.2/protocol/SequenceType
  [3]: https://www.wikiod.com/swift/closures


## Sorting an Array of Strings
<!-- if version [eq 3.0] -->
The most simple way is to use `sorted()`:

    let words = ["Hello", "Bonjour", "Salute", "Ahola"]
    let sortedWords = words.sorted()
    print(sortedWords) // ["Ahola", "Bonjour", "Hello", "Salute"]

or `sort()`

    var mutableWords = ["Hello", "Bonjour", "Salute", "Ahola"]
    mutableWords.sort()
    print(mutableWords) // ["Ahola", "Bonjour", "Hello", "Salute"]

You can pass a closure as an argument for sorting:

    let words = ["Hello", "Bonjour", "Salute", "Ahola"]
    let sortedWords = words.sorted(isOrderedBefore: { $0 > $1 })
    print(sortedWords) // ["Salute", "Hello", "Bonjour", "Ahola"]

Alternative syntax with a trailing closure:

    let words = ["Hello", "Bonjour", "Salute", "Ahola"]
    let sortedWords = words.sorted() { $0 > $1 }
    print(sortedWords) // ["Salute", "Hello", "Bonjour", "Ahola"]

But there will be unexpected results if the elements in the array are not consistent:

    let words = ["Hello", "bonjour", "Salute", "ahola"]
    let unexpected = words.sorted()
    print(unexpected) // ["Hello", "Salute", "ahola", "bonjour"]

To address this issue, either sort on a lowercase version of the elements:

    let words = ["Hello", "bonjour", "Salute", "ahola"]
    let sortedWords = words.sorted { $0.lowercased() < $1.lowercased() }
    print(sortedWords) // ["ahola", "bonjour", "Hello", "Salute"]

Or `import Foundation` and use NSString's comparison methods like `caseInsensitiveCompare`:

    let words = ["Hello", "bonjour", "Salute", "ahola"]
    let sortedWords = words.sorted { $0.caseInsensitiveCompare($1) == .orderedAscending }
    print(sortedWords) // ["ahola", "bonjour", "Hello", "Salute"]

*Alternatively, use `localizedCaseInsensitiveCompare`, which can manage diacritics.*

To properly sort Strings by the *numeric* value they contain, use `compare` with the `.numeric` option:

    let files = ["File-42.txt", "File-01.txt", "File-5.txt", "File-007.txt", "File-10.txt"]
    let sortedFiles = files.sorted() { $0.compare($1, options: .numeric) == .orderedAscending }
    print(sortedFiles) // ["File-01.txt", "File-5.txt", "File-007.txt", "File-10.txt", "File-42.txt"]
<!-- end version if -->

## Accessing indices safely
By adding the following extension to array indices can be accessed without knowing if the index is inside bounds.

    extension Array {
        subscript (safe index: Int) -> Element? {
            return indices ~= index ? self[index] : nil
        }
    }

example:

    if let thirdValue = array[safe: 2] {
        print(thirdValue)
    }

## Useful Methods
Determine whether an array is empty or return its size

    var exampleArray = [1,2,3,4,5]
    exampleArray.isEmpty //false
    exampleArray.count //5
Reverse an Array 
**Note: The result is not performed on the array the method is called on and must be put into its own variable.** 

    exampleArray = exampleArray.reverse()
    //exampleArray = [9, 8, 7, 6, 5, 3, 2]

## Sorting an Array


    var array = [3, 2, 1]

## Creating a new sorted array

As [`Array`][1] conforms to [`SequenceType`][2], we can generate a new array of the sorted elements using a built in sort method.

<!-- if version [eq 2.1] [eq 2.2] -->

In Swift 2, this is done with the [`sort()`][3] method.

    let sorted = array.sort()  // [1, 2, 3]

<!-- end version if -->

<!-- if version [gte 3.0] -->
As of Swift 3, it has been re-named to [`sorted()`][4].

    let sorted = array.sorted()  // [1, 2, 3]
<!-- end version if -->

## Sorting an existing array in place

As `Array` conforms to [`MutableCollectionType`][5], we can sort its elements in place.

<!-- if version [eq 2.1] [eq 2.2] -->

In Swift 2, this is done using the [`sortInPlace()`][6] method.

    array.sortInPlace() // [1, 2, 3]

<!-- end version if -->


<!-- if version [gte 3.0] -->
As of Swift 3, it has been renamed to [`sort()`][7].

    array.sort() // [1, 2, 3]

<!-- end version if -->

> Note: In order to use the above methods, the elements must conform to the [`Comparable`][8] protocol. 

## Sorting an array with a custom ordering

You may also sort an array using a [closure][9] to define whether one element should be ordered before another – which isn't restricted to arrays where the elements must be `Comparable`. For example, it doesn't make sense for a `Landmark` to be `Comparable` – but you can still sort an array of landmarks by height or name.

    struct Landmark {
        let name : String
        let metersTall : Int
    }
    
    var landmarks = [Landmark(name: "Empire State Building", metersTall: 443),
                     Landmark(name: "Eifell Tower", metersTall: 300),
                     Landmark(name: "The Shard", metersTall: 310)]

<!-- if version [eq 2.1] [eq 2.2] -->

    // sort landmarks by height (ascending)
    landmarks.sortInPlace {$0.metersTall < $1.metersTall}
    
    print(landmarks) // [Landmark(name: "Eifell Tower", metersTall: 300), Landmark(name: "The Shard", metersTall: 310), Landmark(name: "Empire State Building", metersTall: 443)]

    // create new array of landmarks sorted by name
    let alphabeticalLandmarks = landmarks.sort {$0.name < $1.name}
    
    print(alphabeticalLandmarks) // [Landmark(name: "Eifell Tower", metersTall: 300), Landmark(name: "Empire State Building", metersTall: 443), Landmark(name: "The Shard", metersTall: 310)]

<!-- end version if -->

<!-- if version [gte 3.0] -->

    // sort landmarks by height (ascending)
    landmarks.sort {$0.metersTall < $1.metersTall}
    
    // create new array of landmarks sorted by name
    let alphabeticalLandmarks = landmarks.sorted {$0.name < $1.name}

<!-- end version if -->

> Note: String comparison can yield unexpected results if the strings are inconsistent, see [Sorting an Array of Strings][10].


  [1]: http://swiftdoc.org/v2.2/type/Array/
  [2]: http://swiftdoc.org/v2.2/protocol/SequenceType
  [3]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-comparable-sort
  [4]: http://swiftdoc.org/v3.0/protocol/Sequence/#func-iterator-element_-comparable-sorted
  [5]: http://swiftdoc.org/v2.2/protocol/MutableCollectionType/
  [6]: http://swiftdoc.org/v2.2/protocol/MutableCollectionType/#func-index_-randomaccessindextype-sortinplace_
  [7]: http://swiftdoc.org/v3.0/protocol/MutableCollection/#func-self_-randomaccesscollection-sort_
  [8]: https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_Comparable_Protocol/index.html#//apple_ref/swift/intf/s:Ps10Comparable
  [9]: https://www.wikiod.com/swift/closures
  [10]: https://www.wikiod.com/swift/arrays#Sorting an Array of Strings

## Transforming the elements of an Array with map(_:)
As [`Array`][1] conforms to [`SequenceType`][2], we can use [`map(_:)`][3] to transform an array of `A` into an array of `B` using a [closure][4] of type `(A) throws -> B`.

For example, we could use it to transform an array of [`Int`][5]s into an array of [`String`][6]s like so:

    let numbers = [1, 2, 3, 4, 5]
    let words = numbers.map { String($0) }
    print(words) // ["1", "2", "3", "4", "5"]

`map(_:)` will iterate through the array, applying the given closure to each element. The result of that closure will be used to populate a new array with the transformed elements.

Since `String` has an initialiser that receives an `Int` we can also use this clearer syntax:

    let words = numbers.map(String.init)

A `map(_:)` transform need not change the type of the array – for example, it could also be used to multiply an array of `Int`s by two:

    let numbers = [1, 2, 3, 4, 5]
    let numbersTimes2 = numbers.map {$0 * 2}
    print(numbersTimes2) // [2, 4, 6, 8, 10]


  [1]: http://swiftdoc.org/v2.2/type/Array/
  [2]: http://swiftdoc.org/v2.2/protocol/SequenceType/
  [3]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func-map_
  [4]: https://www.wikiod.com/swift/closures
  [5]: http://swiftdoc.org/v2.2/type/Int/
  [6]: http://swiftdoc.org/v2.2/type/String/


## Finding the minimum or maximum element of an Array
<!-- if version [eq 2.1] [eq 2.2] -->

You can use the [`minElement()`][1] and [`maxElement()`][2] methods to find the minimum or maximum element in a given sequence. For example, with an array of numbers:

    let numbers = [2, 6, 1, 25, 13, 7, 9]
    
    let minimumNumber = numbers.minElement() // Optional(1)
    let maximumNumber = numbers.maxElement() // Optional(25)

<!-- end version if -->

<!-- if version [gte 3.0] -->

As of Swift 3, the methods have been renamed to [`min()`][3] and [`max()`][4] respectively:
    
    let minimumNumber = numbers.min() // Optional(1)
    let maximumNumber = numbers.max() // Optional(25)

<!-- end version if -->

The returned values from these methods are [Optional][5] to reflect the fact that the array could be empty – if it is, `nil` will be returned.


> Note: The above methods require the elements to conform to the [`Comparable`][6] protocol.

## Finding the minimum or maximum element with a custom ordering

You may also use the above methods with a custom [closure][7], defining whether one element should be ordered before another, allowing you to find the minimum or maximum element in an array where the elements aren't necessarily `Comparable`.

For example, with an array of vectors:

    struct Vector2 {
        let dx : Double
        let dy : Double
        
        var magnitude : Double {return sqrt(dx*dx+dy*dy)}
    }
    
    let vectors = [Vector2(dx: 3, dy: 2), Vector2(dx: 1, dy: 1), Vector2(dx: 2, dy: 2)]

<!-- if version [eq 2.1] [eq 2.2] -->

    // Vector2(dx: 1.0, dy: 1.0)
    let lowestMagnitudeVec2 = vectors.minElement { $0.magnitude < $1.magnitude } 

    // Vector2(dx: 3.0, dy: 2.0)
    let highestMagnitudeVec2 = vectors.maxElement { $0.magnitude < $1.magnitude } 

<!-- end version if -->

<!-- if version [gte 3.0] -->

    let lowestMagnitudeVec2 = vectors.min { $0.magnitude < $1.magnitude }
    let highestMagnitudeVec2 = vectors.max { $0.magnitude < $1.magnitude }

<!-- end version if -->


  [1]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-comparable-minelement
  [2]: http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-comparable-maxelement
  [3]: http://swiftdoc.org/v3.0/protocol/Sequence/#func-iterator-element_-comparable-min
  [4]: http://swiftdoc.org/v3.0/protocol/Sequence/#func-iterator-element_-comparable-max
  [5]: https://www.wikiod.com/swift/optionals
  [6]: http://swiftdoc.org/v2.2/protocol/Comparable/
  [7]: https://www.wikiod.com/swift/closures

## Value Semantics
Copying an array will copy all of the items inside the original array.

Changing the new array *will not change* the original array.

    var originalArray = ["Swift", "is", "great!"]
    var newArray = originalArray
    newArray[2] = "awesome!"
    //originalArray = ["Swift", "is", "great!"]
    //newArray = ["Swift", "is", "awesome!"]

Copied arrays will share the same space in memory as the original until they are changed. As a result of this there is a performance hit when the copied array is given its own space in memory as it is changed for the first time.


## Accessing Array Values
The following examples will use this array to demonstrate accessing values

    var exampleArray:[Int] = [1,2,3,4,5]
    //exampleArray = [1, 2, 3, 4, 5]

To access a value at a known index use the following syntax:

    let exampleOne = exampleArray[2]
    //exampleOne = 3
**Note:** The value at *index two is the third value* in the `Array`. `Array`s use a *zero based index* which means the first element in the `Array` is at index 0.

    let value0 = exampleArray[0]
    let value1 = exampleArray[1]
    let value2 = exampleArray[2]
    let value3 = exampleArray[3]
    let value4 = exampleArray[4]
    //value0 = 1
    //value1 = 2 
    //value2 = 3
    //value3 = 4
    //value4 = 5
Access a subset of an `Array` using filter:

    var filteredArray = exampleArray.filter({ $0 < 4 })
    //filteredArray = [1, 2, 3]

Filters can have complex conditions like filtering only even numbers:

    var evenArray = exampleArray.filter({ $0 % 2 == 0 })
    //evenArray = [2, 4]

It is also possible to return the index of a given value, returning `nil` if the value wasn't found.

    exampleArray.indexOf(3) // Optional(2)
    
There are methods for the first, last, maximum or minimum value in an `Array`. These methods will return `nil` if the `Array` is empty.

    exampleArray.first // Optional(1)
    exampleArray.last // Optional(5)
    exampleArray.maxElement() // Optional(5)
    exampleArray.minElement() // Optional(1)

## Modifying values in an array
There are multiple ways to append values onto an array

    var exampleArray = [1,2,3,4,5]
    exampleArray.append(6)
    //exampleArray = [1, 2, 3, 4, 5, 6]
    var sixOnwards = [7,8,9,10]
    exampleArray += sixOnwards
    //exampleArray = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
and remove values from an array

    exampleArray.removeAtIndex(3)
    //exampleArray = [1, 2, 3, 5, 6, 7, 8, 9, 10]
    exampleArray.removeLast()
    //exampleArray = [1, 2, 3, 5, 6, 7, 8, 9]
    exampleArray.removeFirst()
    //exampleArray = [2, 3, 5, 6, 7, 8, 9]




## Grouping Array values
If we have a struct like this

    struct Box {
        let name: String
        let thingsInside: Int
    }

and an array of `Box(es)`

    let boxes = [
        Box(name: "Box 0", thingsInside: 1),
        Box(name: "Box 1", thingsInside: 2),
        Box(name: "Box 2", thingsInside: 3),
        Box(name: "Box 3", thingsInside: 1),
        Box(name: "Box 4", thingsInside: 2),
        Box(name: "Box 5", thingsInside: 3),
        Box(name: "Box 6", thingsInside: 1)
    ]

we can group the boxes by the `thingsInside` property in order to get a `Dictionary` where the `key` is the number of things and the value is an array of boxes.

    let grouped = boxes.reduce([Int:[Box]]()) { (res, box) -> [Int:[Box]] in
        var res = res
        res[box.thingsInside] = (res[box.thingsInside] ?? []) + [box]
        return res
    }

Now grouped is a `[Int:[Box]]` and has the following content

    [
        2: [Box(name: "Box 1", thingsInside: 2), Box(name: "Box 4", thingsInside: 2)], 
        3: [Box(name: "Box 2", thingsInside: 3), Box(name: "Box 5", thingsInside: 3)],
        1: [Box(name: "Box 0", thingsInside: 1), Box(name: "Box 3", thingsInside: 1), Box(name: "Box 6", thingsInside: 1)]
    ]


## Comparing 2 Arrays with zip
The `zip` function accepts 2 parameters of type `SequenceType` and returns a `Zip2Sequence` where each element contains a value from the first sequence and one from the second sequence.

Example

    let nums = [1, 2, 3]
    let animals = ["Dog", "Cat", "Tiger"]
    let numsAndAnimals = zip(nums, animals)

nomsAndAnimals now contains the following values

| sequence1 | sequence1 |
| ------ | ------ |
| `1`   | `"Dog"`   |
| `2`   | `"Cat"`   |
| `3`   | `"Tiger"`   |

This is useful when you want to perform some kind of comparation between the n-th element of each Array.

**Example**

Given 2 Arrays of `Int(s)`

    let list0 = [0, 2, 4]
    let list1 = [0, 4, 8]

you want to check whether each value into `list1` is the double of the related value in `list0`.

    let list1HasDoubleOfList0 = !zip(list0, list1).filter { $0 != (2 * $1)}.isEmpty


