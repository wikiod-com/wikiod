---
title: "Folds"
slug: "folds"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Intro to folds, with a handful of examples
Folds are (higher-order) functions used with sequences of elements. They collapse ```seq<'a>``` into ```'b``` where ```'b``` is any type (possibly still ```'a```). This is a bit abstract so lets get into concrete practical examples.

# Calculating the sum of all numbers
In this example, ```'a``` is an `int`. We have a list of numbers and we want to calculate sum all the numbers of it. To sum the numbers of the list `[1; 2; 3]` we write 
```
List.fold (fun x y -> x + y) 0 [1; 2; 3] // val it : int = 6
```
Let me explain, because we are dealing with lists, we use `fold` in the `List` module, hence `List.fold`. the first argument `fold` takes is a binary function, the **folder**. The second argument is the **initial value**. `fold` starts folding the list by consecutively applying the folder function to elements in the list starting with the initial value and the first element. If the list is empty, the inital value is returned!

Schematic overview of an execution example looks like this:

```
let add x y = x + y // this is our folder (a binary function, takes two arguments)
List.fold add 0 [1; 2; 3;]
=> List.fold add (add 0 1) [2; 3] 
// the binary function is passed again as the folder in the first argument
// the initial value is updated to add 0 1 = 1 in the second argument
// the tail of the list (all elements except the first one) is passed in the third argument
// it becomes this:
List.fold add 1 [2; 3]
// Repeat untill the list is empty -> then return the "inital" value
List.fold add (add 1 2) [3]
List.fold add 3 [3] // add 1 2 = 3
List.fold add (add 3 3) [] 
List.fold add 6 [] // the list is empty now -> return 6
6
```
The function `List.sum` is roughly `List.fold add LanguagePrimitives.GenericZero` where the generic zero makes it compatible with integers, floats, big integers etc.

# Counting elemets in a list (implementing `count`)
This is done almost the same as above, but by ignoring the actual value of the element in the list and instead adding 1. 
```
List.fold (fun x y -> x + 1) 0 [1; 2; 3] // val it : int = 3
```
This can also be done like this:
```
[1; 2; 3]
|> List.map (fun x -> 1) // turn every elemet into 1, [1; 2; 3] becomes [1; 1; 1]
|> List.sum // sum [1; 1; 1] is 3
```
So you could define `count` as follows:
```
let count xs = 
    xs 
    |> List.map (fun x -> 1) 
    |> List.fold (+) 0 // or List.sum
```
# Finding the maximum of list
This time we will use `List.reduce` which is like `List.fold` but without an initial value as in this case where we don't know what the type is of the values we are compairing:
```
let max x y = if x > y then x else y
// val max : x:'a -> y: 'a -> 'a when 'a : comparison, so only for types that we can compare
List.reduce max [1; 2; 3; 4; 5] // 5
List.reduce max ["a"; "b"; "c"] // "c", because "c" > "b" > "a"
List.reduce max [true; false] // true, because true > false 
```
# Finding the minimum of a list
Just like when finding the max, the folder is different
```
let min x y = if x < y then x else y
List.reduce min [1; 2; 3; 4; 5] // 1
List.reduce min ["a"; "b"; "c"] // "a"
List.reduce min [true; false] // false
```
# Concatenating lists
Here we are taking list of lists
The folder function is the `@` operator
```
// [1;2] @ [3; 4] = [1; 2; 3; 4]
let merge xs ys = xs @ ys
List.fold merge [] [[1;2;3]; [4;5;6]; [7;8;9]] // [1;2;3;4;5;6;7;8;9]
```
Or you could use binary operators as your folder function:
```
List.fold (@) [] [[1;2;3]; [4;5;6]; [7;8;9]] // [1;2;3;4;5;6;7;8;9]
List.fold (+) 0 [1; 2; 3] // 6
List.fold (||) false [true; false] // true, more on this below
List.fold (&&) true [true; false] // false, more on this below
// etc...
```
# Calculating the factorial of a number
Same idea as when summing the numbers, but now we multiply them. if we want the factorial of `n` we multiplty all elements in the list `[1 .. n]`. Code becomes:
```
// the folder
let times x y = x * y
let factorial n = List.fold times 1 [1 .. n]
// Or maybe for big integers
let factorial n = List.fold times 1I [1I .. n] 
```
# Implementing `forall`, `exists` and `contains`
the function `forall` checks if all elements in a sequence satisfy a condition. `exists` checks if atleast one element in the list satisfy the condition. First we need to know how to collapse a list of `bool` values. Well, we use folds ofcourse! boolean operators will be our folder functions.

To check if all elements in a list are `true` we collapse them with the `&&` function with `true` as initial value. 
```
List.fold (&&) true [true; true; true] // true
List.fold (&&) true [] // true, empty list -> return inital value
List.fold (&&) true [false; true] // false
```
Likewise, to check if one element is `true` in a list booleans we collapse it with the `||` operator with `false` as initial value:
```
List.fold (||) false [true; false] // true
List.fold (||) false [false; false] // false, all are false, no element is true
List.fold (||) false [] // false, empty list -> return inital value
```
Back to `forall` and `exists`. Here we take a list of any type, use the condition to transform all elements to boolean values and then we collapse it down:
```
let forall condition elements = 
    elements 
    |> List.map condition // condition : 'a -> bool
    |> List.fold (&&) true

let exists condition elements = 
    elements
    |> List.map condition
    |> List.fold (||) false
```
To check if all elements in [1; 2; 3; 4] are smaller than 5:
```
forall (fun n -> n < 5) [1 .. 4] // true
```
define the `contains` method with `exists`:
```
let contains x xs = exists (fun y -> y = x) xs
```
Or even
```
let contains x xs = exists ((=) x) xs
```
Now check if the list [1 .. 5] contains the value 2:
```
contains 2 [1..5] // true
```
# Implementing `reverse`:
```
let reverse xs = List.fold (fun acc x -> x :: acc) [] xs
reverse [1 .. 5] // [5; 4; 3; 2; 1]
```
# Implementing `map` and `filter`
```
let map f = List.fold (fun acc x -> List.append acc [f x]) List.empty
// map (fun x -> x * x) [1..5] -> [1; 4; 9; 16; 25]   

let filter p = Seq.fold (fun acc x -> seq { yield! acc; if p(x) then yield x }) Seq.empty
// filter (fun x -> x % 2 = 0) [1..10] -> [2; 4; 6; 8; 10]
```
Is there anything `fold` can't do? I dont' really know

## Calculating the sum of all elements of a list
To calculate the sum of terms (of type float, int or big integer) of a number list, it is preferable to use List.sum
In other cases, List.fold is the function that is best suited to calculate such a sum.

1. Sum of complex numbers

In this example, we declare a list of complex numbers and we calculate the sum of all terms in the list.

At the beginning of the program, add a reference to System.Numerics

open System.Numerics

To calculate the sum, we initialize the accumulator to the complex number 0.

    let clist = [new Complex(1.0, 52.0); new Complex(2.0, -2.0); new Complex(0.0, 1.0)]
    
    let sum = List.fold (+) (new Complex(0.0, 0.0)) clist

Result:

    (3, 51)

2. Sum of numbers of union type

Suppose that a list be composed of numbers of union (float or int) type and want to calculate the sum of these numbers.

Declare before the following number type:

    type number = 
    | Float of float
    | Int of int

Calculate the sum of numbers of type number of a list:

    let list = [Float(1.3); Int(2); Float(10.2)]
    
    let sum = List.fold (
                             fun acc elem -> 
                                            match elem with
                                            | Float(elem) -> acc + elem
                                            | Int(elem) -> acc + float(elem)
                            ) 0.0 list
     

Result:

    13.5

The first parameter of the function, which represents the accumulator is of type float and the second parameter, which represents an item in the list is of type number. But before we add, we need to use a pattern matching and cast to type float when elem is of type Int.

