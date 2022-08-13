---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Creating and Initializing Simple Arrays
**Intro**
----------
An *array* is a container object that holds a number of values. In the following image you can see an array with size 10, the first element indexed 1 and the last element 10.

[![enter image description here][1]][1]

Autohotkey offers a few ways of defining and creating arrays.

 - Array := []
 - Array := Array()

# Creating and initializing arrays with N number of items

    Array := [Item1, Item2, ..., ItemN]
    Array := Array(Item1, Item2, ..., ItemN)

In Autohotkey, it is possible to have arrays with no items:

    Array := [] ; works fine.

And elements can then later be assigned to it:

    Array[0] := 1

Array size can be determined using a method called `length`:

    msgbox % array.length()  ; shows 1 in this case.

If the array is not empty, **MinIndex** and **MaxIndex/Length** return the lowest and highest index currently in use in the array. Since the lowest index is nearly always 1, MaxIndex usually returns the number of items. However, if there are no integer keys, MaxIndex returns an empty string whereas Length returns 0.


# Creating and initializing multi-dimensional arrays

You can create a multi-dimensional array as follows:

    Array[1, 2] := 3

You can create and initialize at the same time time, and inner arrays do not need to be of same length.

    Array := [[4,5,6],7,8]

Arrays like this are also called arrays of arrays.


# Filling an array

    ; Assign an item:
    Array[Index] := Value

    ; Insert one or more items at a given index:
    Array.InsertAt(Index, Value, Value2, ...)

    ; Append one or more items:
    Array.Push(Value, Value2, ...)

The value of an index for an array element can also be a negative integer (-1, 0, 1, 2, 3, 4, ...)


# Removing Elements from an array

    ; Remove an item:
    RemovedValue := Array.RemoveAt(Index)

    ; Remove the last item:
    RemovedValue := Array.Pop()


# Adding custom Methods by overriding Array() function

AutoHotkey is a [prototype-based programming language][2], meaning you can override any built-in function/object at anytime. This example demonstrates overriding Array() function in order to add Methods within a custom Class Object. 
 
    ; Overrides Array()
    Array(Args*) {
        Args.Base := _Array
        Return Args
    }

    ; Custom Class Object with Methods
    Class _Array {
        
        ; Reverses the order of the array.
        Reverse() {
            Reversed := []
            Loop % This.MaxIndex()
                Reversed.Push(This.Pop())
            Return Reversed
        }    
        
        ; Sums all Integers in Array
        Sum(Sum=0) {
            For Each, Value In This
                Sum += Value Is Integer ? Value : 0
            Return Sum
        }
    }
    
    ; Arr == ["Hello, World!", 4, 3, 2, 1]
    Arr := [1, 2, 3, 4, "Hello, World!"].Reverse() 
                        
    ; SumOfArray == 10
    SumOfArray := Arr.Sum() 


  [1]: http://i.stack.imgur.com/qqDyL.gif

  [2]: https://en.wikipedia.org/wiki/Prototype-based_programming#Languages_supporting_prototype-based_programming

