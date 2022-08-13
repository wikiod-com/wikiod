---
title: "Lists"
slug: "lists"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Syntax
 - [] // an empty list.
   
   head::tail // a construction cell holding an element, head, and a
   list, tail. :: is called the Cons operator.
   
   let list1 = [ 1; 2; 3 ] // Note the usage of a semicolon.
   
   let list2 = 0 :: list1 // result is [ 0; 1; 2; 3 ]
   
   let list3 = list1 @ list2 // result is [ 1; 2; 3; 0; 1; 2; 3 ]. @ is
   the append operator.
   
   let list4 = [ 1..3 ] // result is [ 1; 2; 3 ]
   
   let list5 = [ 1..2..10 ] // result is [ 1; 3; 5; 7; 9 ]
   
   let list6 = [ for i in 1..10 do if i % 2 = 1 then yield i ] // result
   is [ 1; 3; 5; 7; 9 ]

## Basic List Usage
    let list1 = [ 1; 2 ]
    let list2 = [ 1 .. 100 ]
    
    // Accessing an element
    printfn "%A" list1.[0]
    
    // Pattern matching
    let rec patternMatch aList = 
        match aList with
        | [] -> printfn "This is an empty list"
        | head::tail -> printfn "This list consists of a head element %A and a tail list %A" head tail
                        patternMatch tail
    
    patternMatch list1
    
    // Mapping elements
    let square x = x*x
    let list2squared = list2
                       |> List.map square
    printfn "%A" list2squared 

## Calculating the total sum of numbers in a list 
By recursion
```
let rec sumTotal list = 
    match list with
    | [] -> 0 // empty list -> return 0
    | head :: tail -> head + sumTotal tail
```
The above example says: "Look at the `list`, is it empty? return 0. Otherwise it is a non-empty list. So it could be `[1]`, `[1; 2]`, [1; 2; 3] etc. If `list`is [1] then bind the variable `head` to `1` and `tail` to `[]` then execute `head + sumTotal tail`. 

Example execution:
```
sumTotal [1; 2; 3]
// head -> 1, tail -> [2; 3]
1 + sumTotal [2; 3]
1 + (2 + sumTotal [3])
1 + (2 + (3 + sumTotal [])) // sumTotal [] is defined to be 0, recursion stops here
1 + (2 + (3 + 0)))
1 + (2 + 3)
1 + 5 
6
```
A more general way to encapsulate the above pattern is by using functional folds! `sumTotal` becomes this:
```
let sumTotal list = List.fold (+) 0 list
```

## Creating lists
A way to create a list is to place elements in two square brackets, separated by semicolons. The elements must have the same type.

Example:

    > let integers = [1; 2; 45; -1];;
    val integers : int list = [1; 2; 45; -1]


    > let floats = [10.7; 2.0; 45.3; -1.05];;
    val floats : float list = [10.7; 2.0; 45.3; -1.05]

When a list has no element, it is empty. An empty list can be declared as follows:

    > let emptyList = [];;
    val emptyList : 'a list

Other example

To create a list of byte, simply to cast the integers:

    > let bytes = [byte(55); byte(10); byte(100)];;
    val bytes : byte list = [55uy; 10uy; 100uy]

It is also possible to define lists of functions, of elements of a type defined previously, of objects of a class, etc.

Example

    > type number = | Real of float | Integer of int;;
    
    type number =
      | Real of float
      | Integer of int

    > let numbers = [Integer(45); Real(0.0); Integer(127)];;
    val numbers : number list = [Integer 45; Real 0.0; Integer 127]

Ranges 

For certain types of elements (int, float, char,...), it is possible to define a list by the start element and the end element, using the following template:

``[start..end]``

Examples:

    > let c=['a' .. 'f'];;
    val c : char list = ['a'; 'b'; 'c'; 'd'; 'e'; 'f']

    let f=[45 .. 60];;
    val f : int list =
      [45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58; 59; 60]

You can also specify a step for certain types, with the following model:

``[start..step..end] ``

Examples:

    > let i=[4 .. 2 .. 11];;
    val i : int list = [4; 6; 8; 10]

    > let r=[0.2 .. 0.05 .. 0.28];;
    val r : float list = [0.2; 0.25]

Generator

Another way to create a list is to generate it automatically by using generator.

We can use one of the following models:

``[for <identifier> in range -> expr]``

or

``[for <identifier> in range do ... yield expr]``

Examples

    > let oddNumbers = [for i in 0..10 -> 2 * i + 1];; // odd numbers from 1 to 21
    val oddNumbers : int list = [1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 21]

    > let multiples3Sqrt = [for i in 1..27 do if i % 3 = 0 then yield sqrt(float(i))];; //sqrt of multiples of 3 from 3 to 27
    val multiples3Sqrt : float list =
      [1.732050808; 2.449489743; 3.0; 3.464101615; 3.872983346; 4.242640687;    4.582575695; 4.898979486; 5.196152423]

Operators

Some operators may be used to construct lists:

 Cons operator ::

This operator :: is used to add a head element to a list:

    > let l=12::[] ;;
    val l : int list = [12]

    > let l1=7::[14; 78; 0] ;;
    val l1 : int list = [7; 14; 78; 0]
    
    > let l2 = 2::3::5::7::11::[13;17] ;;
    val l2 : int list = [2; 3; 5; 7; 11; 13; 17]

 Concatenation

The concatenation of lists is carried out with the operator @.

    > let l1 = [12.5;89.2];;
    val l1 : float list = [12.5; 89.2]

    > let l2 = [1.8;7.2] @ l1;;
    val l2 : float list = [1.8; 7.2; 12.5; 89.2]

