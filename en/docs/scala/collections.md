---
title: "Collections"
slug: "collections"
draft: false
images: []
weight: 9871
type: docs
toc: true
---

## List and Vector Cheatsheet
> It is now a best-practice to use `Vector` instead of `List` because the implementations have better performance [Performance characteristics can be found here][traversableperf]. `Vector` can be used wherever `List` is used.

[traversableperf]: http://docs.scala-lang.org/overviews/collections/performance-characteristics.html

**List creation**

    List[Int]()         // Declares an empty list of type Int
    List.empty[Int]     // Uses `empty` method to declare empty list of type Int
    Nil                 // A list of type Nothing that explicitly has nothing in it
    
    List(1, 2, 3)       // Declare a list with some elements
    1 :: 2 :: 3 :: Nil  // Chaining element prepending to an empty list, in a LISP-style

**Take element**

    List(1, 2, 3).headOption // Some(1)
    List(1, 2, 3).head       // 1
    
    List(1, 2, 3).lastOption // Some(3)
    List(1, 2, 3).last       // 3, complexity is O(n)
    
    List(1, 2, 3)(1)         // 2, complexity is O(n)
    List(1, 2, 3)(3)         // java.lang.IndexOutOfBoundsException: 4

**Prepend Elements**

    0 :: List(1, 2, 3)       // List(0, 1, 2, 3)

**Append Elements**

    List(1, 2, 3) :+ 4       // List(1, 2, 3, 4), complexity is O(n)

**Join (Concatenate) Lists**
    
    List(1, 2) ::: List(3, 4) // List(1, 2, 3, 4)
    List.concat(List(1,2), List(3, 4)) // List(1, 2, 3, 4)
    List(1, 2) ++ List(3, 4)  // List(1, 2, 3, 4)

**Common operations**

    List(1, 2, 3).find(_ == 3)                     // Some(3)
    List(1, 2, 3).map(_ * 2)                       // List(2, 4, 6)
    List(1, 2, 3).filter(_ % 2 == 1)               // List(1, 3)
    List(1, 2, 3).fold(0)((acc, i) => acc + i * i) // 1 * 1 + 2 * 2 + 3 * 3 = 14
    List(1, 2, 3).foldLeft("Foo")(_ + _.toString)  // "Foo123"
    List(1, 2, 3).foldRight("Foo")(_ + _.toString) // "123Foo"





## Introduction to Scala Collections
The Scala Collections framework, [according to its authors][intro], is designed to be easy to use, concise, safe, fast, and universal.

The framework is made up of Scala [traits][1] that are designed to be building blocks for creating collections. For more information on these building blocks, [read the official Scala collections overview][overview].

These built-in collections are separated into the immutable and mutable packages. By default, the immutable versions are used. Constructing a `List()` (without importing anything) will construct an *immutable* list.

One of the most powerful features of the framework is the consistent and easy-to-use interface across like-minded collections. For example, summing all elements in a collection is the same for Lists, Sets, Vectors, Seqs and Arrays:

```
val numList = List[Int](1, 2, 3, 4, 5)
numList.reduce((n1, n2) => n1 + n2)  // 15

val numSet = Set[Int](1, 2, 3, 4, 5)
numSet.reduce((n1, n2) => n1 + n2)   // 15

val numArray = Array[Int](1, 2, 3, 4, 5)
numArray.reduce((n1, n2) => n1 + n2) // 15
```

These like-minded types inherit from the `Traversable` trait.

> It is now a best-practice to use `Vector` instead of `List` because the implementations have better performance [Performance characteristics can be found here][traversableperf]. `Vector` can be used wherever `List` is used.

[traversableperf]: http://docs.scala-lang.org/overviews/collections/performance-characteristics.html
[intro]: http://docs.scala-lang.org/overviews/collections/introduction.html
[overview]: http://docs.scala-lang.org/overviews/collections/overview

## [Traversable types][traversable]

Collection classes that have the `Traversable` trait implement `foreach` and inherit many methods for performing common operations to collections, which all function identically. The most common operations are listed here:

* [Map][map] - `map`, `flatMap`, and `collect` produce new collections by applying a function to each element in the original collection.
```
List(1, 2, 3).map(num => num * 2) // double every number = List(2, 4, 6)

// split list of letters into individual strings and put them into the same list
List("a b c", "d e").flatMap(letters => letters.split(" ")) // = List("a", "b", "c", "d", "e")
```
* [Conversions][toList] - `toList`, `toArray`, and many other conversion operations change the current collection into a more specific kind of collection. These are usually methods prepended with 'to' and the more specific type (i.e. 'toList' converts to a `List`).
```
val array: Array[Int] = List[Int](1, 2, 3).toArray // convert list of ints to array of ints
```
* [Size info][isEmpty] - `isEmpty`, `nonEmpty`, `size`, and `hasDefiniteSize` are all metadata about the set. This allows conditional operations on the collection, or for code to determine the size of the collection, including whether it's infinite or discrete.
```
List().isEmpty // true
List(1).nonEmpty // true
```
* [Element retrieval][head] - `head`, `last`, `find`, and their [`Option`][3] variants are used to retrieve the first or last element, or find a specific element in the collection.
```
val list = List(1, 2, 3)
list.head // = 1
list.last // = 3
```
* [Sub-collection retrieval operations][filter] - `filter`, `tail`, `slice`, `drop`, and other operations allow for choosing parts of the collection to operate on further.
```
List(-2, -1, 0, 1, 2).filter(num => num > 0) // = List(1, 2)
```
* [Subdivision operations][partition] - `partition`, `splitAt`, `span`, and `groupBy` split the current collection into different parts.
```
// split numbers into < 0 and >= 0
List(-2, -1, 0, 1, 2).partition(num => num < 0) // = (List(-2, -1), List(0, 1, 2))
```
* [Element tests][exists] - `exists`, `forall`, and `count` are operations used to check this collection to see if it satisfies a predicate.
```
List(1, 2, 3, 4).forall(num => num > 0) // = true, all numbers are positive
List(-3, -2, -1, 1).forall(num => num < 0) // = false, not all numbers are negative
```
* [Folds][foldLeft] - `foldLeft` (`/:`), `foldRight` (`:\`), `reduceLeft`, and `reduceRight` are used to apply binary functions to successive elements in the collection. [Go here for fold examples][4] and [go here for reduce examples][5].

[traversable]: http://docs.scala-lang.org/overviews/collections/trait-traversable

[map]: http://www.scala-lang.org/api/current/index.html#scala.collection.Traversable@map[B](f:A=>B):Traversable[B]
[toList]: http://www.scala-lang.org/api/current/index.html#scala.collection.Traversable@toList:List[A]
[isEmpty]: http://www.scala-lang.org/api/current/index.html#scala.collection.Traversable@isEmpty:Boolean
[head]: http://www.scala-lang.org/api/current/index.html#scala.collection.Traversable@head:A
[filter]: http://www.scala-lang.org/api/current/index.html#scala.collection.Traversable@filter(p:A=>Boolean):Repr
[partition]: http://www.scala-lang.org/api/current/index.html#scala.collection.Traversable@partition(p:A=>Boolean):(Repr,Repr)
[exists]: http://www.scala-lang.org/api/current/index.html#scala.collection.Traversable@exists(p:A=>Boolean):Boolean
[foldLeft]: http://www.scala-lang.org/api/current/index.html#scala.collection.Traversable@foldLeft


  [1]: http://docs.scala-lang.org/tutorials/tour/traits.html
  [3]: https://www.wikiod.com/scala/option-class
  [4]: https://www.wikiod.com/scala/collections#Fold
  [5]: https://www.wikiod.com/scala/collections#Reduce

## Map Collection Cheatsheet
> Note that this deals with the creation of a collection of type `Map`, which is distinct from the `map` method.

**Map Creation**

    Map[String, Int]() 
    val m1: Map[String, Int] = Map()
    val m2: String Map Int = Map()

A map can be considered a collection of `tuples` for most operations, where the first element is the key and the second is the value.

    val l = List(("a", 1), ("b", 2), ("c", 3))
    val m = l.toMap                               // Map(a -> 1, b -> 2, c -> 3)

**Get element**

    val m = Map("a" -> 1, "b" -> 2, "c" -> 3)
    
    m.get("a")  // Some(1)
    m.get("d")  // None
    m("a")      // 1
    m("d")      // java.util.NoSuchElementException: key not found: d
    
    m.keys      // Set(a, b, c)
    m.values    // MapLike(1, 2, 3)
**Add element(s)**

    Map("a" -> 1, "b" -> 2) + ("c" -> 3)               // Map(a -> 1, b -> 2, c -> 3)
    Map("a" -> 1, "b" -> 2) + ("a" -> 3)               // Map(a -> 3, b -> 2)
    Map("a" -> 1, "b" -> 2) ++ Map("b" -> 3, "c" -> 4) // Map(a -> 1, b -> 3, c -> 4)

**Common operations**

In operations where an iteration over a map occurs (`map`, `find`, `forEach`, etc), the elements of the collection are `tuples`. The function parameter can either use the tuple accessors (`_1`, `_2`), or a partial function with a case block:

    m.find(_._1 == "a")  // Some((a,1))
    m.map {
      case (key, value) => (value, key)
    }                    // Map(1 -> a, 2 -> b, 3 -> c)
    m.filter(_._2 == 2)  // Map(b -> 2)
    m.foldLeft(0){
      case (acc, (key, value: Int)) => acc + value
    }                    // 6



## Sort A List
Supposing the following [list][1] we can sort a variety of ways.

    val names = List("Kathryn", "Allie", "Beth", "Serin", "Alana")

The default behavior of `sorted()` is to use [`math.Ordering`][2], which for strings results in a [lexographic][3] sort:

    names.sorted
    // results in: List(Alana, Allie, Beth, Kathryn, Serin)

`sortWith` allows you to provide your own ordering utilizing a comparison function:

    names.sortWith(_.length < _.length)
    // results in: List(Beth, Allie, Serin, Alana, Kathryn)

`sortBy` allows you to provide a transformation function:

    //A set of vowels to use
    val vowels = Set('a', 'e', 'i', 'o', 'u')

    //A function that counts the vowels in a name
    def countVowels(name: String) = name.count(l => vowels.contains(l.toLower))
    
    //Sorts by the number of vowels
    names.sortBy(countVowels)
    //result is: List(Kathryn, Beth, Serin, Allie, Alana)

You can always reverse a list, or a sorted list, using `reverse:

    names.sorted.reverse
    //results in: List(Serin, Kathryn, Beth, Allie, Alana)
    
Lists can also be sorted using Java method [`java.util.Arrays.sort`][4] and its Scala wrapper [`scala.util.Sorting.quickSort`][5]

    java.util.Arrays.sort(data)
    scala.util.Sorting.quickSort(data)

These methods can improve performance when sorting larger collections if the collection conversions and unboxing/boxing can be avoided. For a more detailed discussion on the performance differences, read about [Scala Collection sorted, sortWith and sortBy Performance][6].


  [1]: http://www.scala-lang.org/api/2.11.8/#scala.collection.immutable.List
  [2]: http://www.scala-lang.org/api/2.11.8/index.html#scala.math.Ordering
  [3]: https://en.wikipedia.org/wiki/Lexicographical_order
  [4]: https://docs.oracle.com/javase/7/docs/api/java/util/Arrays.html
  [5]: http://www.scala-lang.org/api/2.12.0-M4/scala/util/Sorting$.html
  [6]: http://stackoverflow.com/questions/23588615/scala-collection-sorted-sortwith-and-sortby-performance

## Create a List containing n copies of x
To create a collection of `n` copies of some object `x`, use the [fill][1] method. This example creates a `List`, but this can work with other collections for which `fill` makes sense:

    // List.fill(n)(x)
    scala > List.fill(3)("Hello World")
    res0: List[String] = List(Hello World, Hello World, Hello World)

  [1]: http://www.scala-lang.org/docu/files/collections-api/collections_45.html

## Map and Filter Over A Collection
# Map
'Mapping' across a collection uses the `map` function to transform each element of that collection in a similar way. The general syntax is:

    val someFunction: (A) => (B) = ???
    collection.map(someFunction)

You can provide an anonymous function:

    collection.map((x: T) => /*Do something with x*/)

## Multiplying integer numbers by two

    // Initialize 
    val list = List(1,2,3)
    // list: List[Int] = List(1, 2, 3)
    
    // Apply map
    list.map((item: Int) => item*2)
    // res0: List[Int] = List(2, 4, 6)
    
    // Or in a more concise way
    list.map(_*2)
    // res1: List[Int] = List(2, 4, 6)

# Filter

`filter` is used when you want to exclude or 'filter out' certain elements of a collection. As with `map`, the general syntax takes a function, but that function must return a `Boolean`:

    val someFunction: (a) => Boolean = ???
    collection.filter(someFunction)

You can provide an anonymous function directly:

    collection.filter((x: T) => /*Do something that returns a Boolean*/)

## Checking pair numbers

    val list = 1 to 10 toList
    // list: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    
    // Filter out all elements that aren't evenly divisible by 2
    list.filter((item: Int) => item % 2==0)
    // res0: List[Int] = List(2, 4, 6, 8, 10)

# More Map and Filter examples

    case class Person(firstName: String,
                      lastName: String,
                      title: String)
    
    // Make a sequence of people
    val people = Seq(
      Person("Millie", "Fletcher", "Mrs"),
      Person("Jim", "White", "Mr"),
      Person("Jenny", "Ball", "Miss") )
    
    
    // Make labels using map
    val labels = people.map( person =>
      s"${person.title}. ${person.lastName}"
    )
    
    // Filter the elements beginning with J
    val beginningWithJ = people.filter(_.firstName.startsWith("J"))
    
    // Extract first names and concatenate to a string
    val firstNames = people.map(_.firstName).reduce( (a, b) => a + "," + b )

## Fold
The `fold` method iterates over a collection, using an initial accumulator value and applying a function that uses each element to update the accumulator successfully:

    val nums = List(1,2,3,4,5)
    var initialValue:Int = 0;
    var sum = nums.fold(initialValue){
      (accumulator,currentElementBeingIterated) => accumulator + currentElementBeingIterated
    }
    println(sum) //prints 15 because 0+1+2+3+4+5 = 15

In the above example, an anonymous function was supplied to `fold()`. You can also use a named function that takes two arguments. Bearing this in my, the above example can be re-written thus:

    def sum(x: Int, y: Int) = x+ y
    val nums = List(1, 2, 3, 4, 5)
    var initialValue: Int = 0
    val sum = nums.fold(initialValue)(sum)
    println(sum) // prints 15 because 0 + 1 + 2 + 3 + 4 + 5 = 15


Changing the initial value will affect the result:

    initialValue = 2;
    sum = nums.fold(initialValue){ 
     (accumulator,currentElementBeingIterated) => accumulator + currentElementBeingIterated
    }
    println(sum) //prints 17 because 2+1+2+3+4+5 = 17

The `fold` method has two variants - `foldLeft` and `foldRight`.

`foldLeft()` iterates from left to right (from the first element of the collection to the last in that order). `foldRight()` iterates from right to left (from the last element to the first element). `fold()` iterates from left to right like `foldLeft()`. In fact, `fold()` actually calls `foldLeft()` internally.

    def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = foldLeft(z)(op)

`fold()`, `foldLeft()` and `foldRight()` will return a value that has the same type with the initial value it takes. However, unlike `foldLeft()` and `foldRight()`, the initial value given to `fold()` can only be of the same type or a supertype of the type of the collection.

In this example the order is not relevant, so you can change `fold()` to `foldLeft()` or `foldRight()` and the result will remain the same. Using a function that is sensitive to order will alter results.

If in doubt, prefer `foldLeft()` over `foldRight()`. `foldRight()` is less performant.

## Foreach
`foreach` is unusual among the collections iterators in that it does not return a result. Instead it applies a function to each element that has only side effects.  For example:

    scala> val x = List(1,2,3)
    x: List[Int] = List(1, 2, 3)
    
    scala> x.foreach { println }
    1
    2
    3

The function supplied to `foreach` can have any return type, but [the result will be discarded][1].  Typically `foreach` is used when side effects are desirable.  If you want to transform data consider using `map`, `filter`, a `for comprehension`, or another option.

**Example of discarding results**
```
def myFunc(a: Int) : Int = a * 2
List(1,2,3).foreach(myFunc) // Returns nothing
```


  [1]: http://scala-lang.org/api/2.11.2/index.html#scala.collection.SeqLike@foreach(f:A=%3EUnit):Unit

## Reduce
The `reduce()`, `reduceLeft()` and `reduceRight` methods are similar to folds. The function passed to reduce takes two values and yields a third. When operating on a list, the first two values are the first two values in the list. The result of the function and the next value in the list are then re-applied to the function, yielding a new result. This new result is applied with the next value of the list and so on until there are no more elements. The final result is returned.

    val nums = List(1,2,3,4,5)
    sum = nums.reduce({ (a, b) => a + b })
    println(sum) //prints 15

    val names = List("John","Koby", "Josh", "Matilda", "Zac", "Mary Poppins")
    
    def findLongest(nameA:String, nameB:String):String = {
      if (nameA.length > nameB.length) nameA else nameB
    }

    def findLastAlphabetically(nameA:String, nameB:String):String = {
      if (nameA > nameB) nameA else nameB
    }
    
    val longestName:String = names.reduce(findLongest(_,_))
    println(longestName) //prints Mary Poppins

    //You can also omit the arguments if you want
    val lastAlphabetically:String = names.reduce(findLastAlphabetically)
    println(lastAlphabetically) //prints Zac

There are some differences in how the reduce functions work as compared to the fold functions. They are:

 1. The reduce functions have no initial accumulator value.
 2. Reduce functions cannot be called on empty lists.
 3. Reduce functions can only return the type or supertype of the list.

