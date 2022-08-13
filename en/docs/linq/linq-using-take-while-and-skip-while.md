---
title: "Linq Using Take while And  Skip While"
slug: "linq-using-take-while-and--skip-while"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Take, Skip, TakeWhile and SkipWhile are all called Partitioning Operators since they obtain a section of an input sequence as determined by a given condition.
Let us discuss these operators

## Take method
The Take Method Takes elements up to a specified position starting from the first element in a sequence.
**Signature of Take:** 

    Public static IEnumerable<TSource> Take<TSource>(this IEnumerable<TSource> source,int count);

**Example:**

    int[] numbers = { 1, 5, 8, 4, 9, 3, 6, 7, 2, 0 };
    var TakeFirstFiveElement = numbers.Take(5);
 **Output:**

The Result is 1,5,8,4 and 9 
To Get Five Element.








 



## Skip Method
Skips elements up to a specified position starting from the first element in a sequence.

**Signature of Skip:**

    Public static IEnumerable Skip(this IEnumerable source,int count);

**Example**
   
    int[] numbers = { 1, 5, 8, 4, 9, 3, 6, 7, 2, 0 };
    var SkipFirstFiveElement = numbers.Take(5);

**Output:**
   The Result is 3,6,7,2 and 0 To Get The Element.
  

## TakeWhile():
Returns elements from the given collection until the specified condition is true. If the first element itself doesn't satisfy the condition then returns an empty collection.

**Signature of TakeWhile():**

    Public static IEnumerable <TSource> TakeWhile<TSource>(this IEnumerable <TSource> source,Func<TSource,bool>,predicate);

Another Over Load Signature:

    Public static IEnumerable <TSource> TakeWhile<TSource>(this IEnumerable <TSource> source,Func<TSource,int,bool>,predicate);

**Example I:**

    int[] numbers = { 1, 5, 8, 4, 9, 3, 6, 7, 2, 0 };
    var SkipFirstFiveElement = numbers.TakeWhile(n => n < 9);
**Output:**

 It Will return Of eleament 1,5,8 and 4 

**Example II :**
 
    int[] numbers = { 1, 2, 3, 4, 9, 3, 6, 7, 2, 0 };
    var SkipFirstFiveElement = numbers.TakeWhile((n,Index) => n < index);

**Output:** 

It Will return Of element 1,2,3 and 4 



## SkipWhile()
Skips elements based on a condition until an element does not satisfy the condition. If the first element itself doesn't satisfy the condition, it then skips 0 elements and returns all the elements in the sequence.

**Signature of SkipWhile():**

    Public static IEnumerable <TSource> SkipWhile<TSource>(this IEnumerable <TSource> source,Func<TSource,bool>,predicate);

**Another Over Load Signature:**

    Public static IEnumerable <TSource> SkipWhile<TSource>(this IEnumerable <TSource> source,Func<TSource,int,bool>,predicate);

**Example I:**

    int[] numbers = { 1, 5, 8, 4, 9, 3, 6, 7, 2, 0 };
    var SkipFirstFiveElement = numbers.SkipWhile(n => n < 9);

**Output:**

  It Will return Of element 9,3,6,7,2 and 0.

**Example II:**

    int[] numbers = { 4, 5, 8, 1, 9, 3, 6, 7, 2, 0 };
    var indexed = numbers.SkipWhile((n, index) => n > index);

**Output:**

  It Will return Of element 1,9,3,6,7,2 and 0.


