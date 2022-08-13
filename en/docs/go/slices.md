---
title: "Slices"
slug: "slices"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

A slice is a data structure that encapsulates an array so that the programmer can add as many elements as needed without having to worry about memory management. Slices can be cut into sub-slices very efficiently, since the resulting slices all point to the same internal array. Go programmers often take advantage of this to avoid copying arrays, which would typically be done in many other programming languages.

## Syntax
 - slice := make([]type, len, cap) // create a new slice
 - slice = append(slice, item) // append a item to a slice
 - slice = append(slice, items...) // append slice of items to a slice
 - len := len(slice) // get the length of a slice
 - cap := cap(slice) // get the capacity of a slice
 - elNum := copy(dst, slice) // copy a the contents of a slice to an other slice

## Adding Two slices together
    slice1 := []string{"!"}
    slice2 := []string{"Hello", "world"}
    slice  := append(slice1, slice2...)

[Run in the Go Playground](https://play.golang.org/p/H3EsjlcMu5)

## Length and Capacity
Slices have both length and capacity.  The length of a slice is the number of elements *currently* in the slice, while the capacity is the number of elements the slice *can hold* before needing to be reallocated.

When creating a slice using the built-in `make()` function, you can specify its length, and optionally its capacity. If the capacity is not explicitly specified, it will be the specified length.
```
var s = make([]int, 3, 5) // length 3, capacity 5
```

You can check the length of a slice with the built-in `len()` function:
```
var n = len(s) // n == 3
```

You can check the capacity with the built-in `cap()` function:
```
var c = cap(s) // c == 5
```

Elements created by `make()` are set to the zero value for the element type of the slice:
```
for idx, val := range s {
    fmt.Println(idx, val)
}
// output:
// 0 0
// 1 0
// 2 0
```
[Run it on play.golang.org][1]

You cannot access elements beyond the length of a slice, even if the index is within capacity:
```
var x = s[3] // panic: runtime error: index out of range
```
However, as long as the capacity exceeds the length, you can append new elements without reallocating:
```
var t = []int{3, 4}
s = append(s, t) // s is now []int{0, 0, 0, 3, 4}
n = len(s) // n == 5
c = cap(s) // c == 5
```

If you append to a slice which lacks the capacity to accept the new elements, the underlying array will be reallocated for you with sufficient capacity:
```
var u = []int{5, 6}
s = append(s, u) // s is now []int{0, 0, 0, 3, 4, 5, 6}
n = len(s) // n == 7
c = cap(s) // c > 5
```
It is, therefore, generally good practice to allocate sufficient capacity when first creating a slice, if you know how much space you'll need, to avoid unnecessary reallocations.

  [1]: https://play.golang.org/p/E1OoWPDjwW

## Removing elements / "Slicing" slices
If you need to remove one or more elements from a slice, or if you need to work with a sub slice of another existing one; you can use the following method.

> Following examples uses slice of int, but that works with all type of slice.

So for that, we need a slice, from witch we will remove some elements:

    slice := []int{1, 2, 3, 4, 5, 6} 
    // > [1 2 3 4 5 6]

We need also the indexes of elements to remove:
 
    // index of first element to remove (corresponding to the '3' in the slice)
    var first = 2

    // index of last element to remove (corresponding to the '5' in the slice)
    var last = 4 

And so we can "slice" the slice, removing undesired elements:

    // keeping elements from start to 'first element to remove' (not keeping first to remove), 
    // removing elements from 'first element to remove' to 'last element to remove'
    // and keeping all others elements to the end of the slice
    newSlice1 := append(slice[:first], slice[last+1:]...)
    // > [1 2 6]

    // you can do using directly numbers instead of variables
    newSlice2 := append(slice[:2], slice[5:]...)
    // > [1 2 6]

    // Another way to do the same
    newSlice3 := slice[:first + copy(slice[first:], slice[last+1:])]
    // > [1 2 6]

    // same that newSlice3 with hard coded indexes (without use of variables)
    newSlice4 := slice[:2 + copy(slice[2:], slice[5:])]
    // > [1 2 6]

To remove only one element, just have to put the index of this element as the first AND as the last index to remove, just like that:

    var indexToRemove = 3
    newSlice5 := append(slice[:indexToRemove], slice[indexToRemove+1:]...)
    // > [1 2 3 5 6]

    // hard-coded version:
    newSlice5 := append(slice[:3], slice[4:]...)
    // > [1 2 3 5 6]

And you can also remove elements from the beginning of the slice:

    newSlice6 := append(slice[:0], slice[last+1:]...)
    // > [6]

    // That can be simplified into
    newSlice6 := slice[last+1:]
    // > [6]

You can also removing some elements from the end of the slice:

    newSlice7 := append(slice[:first], slice[first+1:len(slice)-1]...)
    // > [1 2]

    // That can be simplified into
    newSlice7 := slice[:first]
    // > [1 2]

> If the new slice have to contains exactly the same elements than the first one, you can use the same thing but with `last := first-1`.  
(This can be useful in case of your indexes are previously computed)

## Creating Slices
Slices are the typical way go programmers store lists of data.

To declare a slice variable use the `[]Type` syntax.

    var a []int

To declare and initialize a slice variable in one line use the `[]Type{values}` syntax.

    var a []int = []int{3, 1, 4, 1, 5, 9}

Another way to initialize a slice is with the `make` function. It three arguments: the `Type` of the slice (or [map][1]), the `length`, and the `capacity`.

    a := make([]int, 0, 5)

You can add elements to your new slice using `append`.
    
    a = append(a, 5)

Check the number of elements in your slice using `len`.

    length := len(a)

Check the capacity of your slice using `cap`. The capacity is the number of elements currently allocated to be in memory for the slice. You can always append to a slice at capacity as Go will automatically create a bigger slice for you. 

    capacity := cap(a)

You can access elements in a slice using typical indexing syntax.

    a[0]  // Gets the first member of `a`

You can also use a `for` loop over slices with `range`. The first variable is the index in the specified array, and the second variable is the value for the index.

    for index, value := range a {
        fmt.Println("Index: " + index + " Value: " + value)  // Prints "Index: 0 Value: 5" (and continues until end of slice)
    }

[Go Playground][2]


  [1]: https://www.wikiod.com/go/maps
  [2]: https://play.golang.org/p/l9M34jbOla

## Filtering a slice
To filter a slice without allocating a new underlying array:

    // Our base slice
    slice := []int{ 1, 2, 3, 4 }
    // Create a zero-length slice with the same underlying array
    tmp := slice[:0]

    for _, v := range slice {
      if v % 2 == 0 {
        // Append desired values to slice
        tmp = append(tmp, v)
      }
    }

    // (Optional) Reassign the slice
    slice = tmp // [2, 4]

## Zero value of slice
The zero value of slice is `nil`, which has the length and capacity `0`. A `nil` slice has no underlying array. But there are also non-nil slices of length and capacity `0`, like `[]int{}` or `make([]int, 5)[5:]`.

Any type that have nil values can be converted to `nil` slice:

    s = []int(nil)

To test whether a slice is empty, use:

    if len(s) == 0　{
        fmt.Ptintf("s is empty.")
    }


## Appending to slice
    slice = append(slice, "hello", "world")

## Copying contents from one slice to another slice
If you wish to copy the contents of a slice into an initially empty slice, following steps can be taken to accomplish it-

1) Create the source slice:


    var sourceSlice []interface{} = []interface{}{"Hello",5.10,"World",true}

2) Create the destination slice, with:

- Length = Length of sourceSlice


    var destinationSlice []interface{} = make([]interface{},len(sourceSlice))
    
3) Now that the destination slice's underlying array is big enough to accomodate all the elements of the source slice, we can proceed to copy the elements using the builtin `copy`:


    copy(destinationSlice,sourceSlice)

