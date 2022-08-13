---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Dart is a true object-oriented language, so even functions are objects and have a type, Function. This means that functions can be assigned to variables or passed as arguments to other functions. You can also call an instance of a Dart class as if it were a function. 

## Functions with named parameters
When defining a function, use {param1, param2, …} to specify named parameters:

    void enableFlags({bool bold, bool hidden}) {
      // ...
    }

When calling a function, you can specify named parameters using paramName: value

    enableFlags(bold: true, hidden: false);




## Function scoping
Dart functions may also be declared anonymously or nested.
For example, to create a nested function, just open a new function block within an existing function block

    void outerFunction() {
      
        bool innerFunction() {
            /// Does stuff
        }
    }
The function `innerFunction` may now be used inside, and only inside, `outerFunction`. No other other functions has access to it.

Functions in Dart may also be declared anonymously, which is commonly used as function arguments. A common example is the `sort` method of `List` object. This method takes an optional argument with the following signature:

    int compare(E a, E b)

The documentation states that the function must return `0` if the `a` and `b` are equal. It returns `-1` if `a < b` and `1` if `a > b`.

Knowing this, we can sort a list of integers using an anonymous function.

    List<int> numbers = [4,1,3,5,7];

    numbers.sort((int a, int b) {
       if(a == b) {
          return 0;
       } else if (a < b) {
          return -1;
       } else {
          return 1;
       }
    });

Anonymous function may also be bound to identifiers like so:

    Function intSorter = (int a, int b) {
       if(a == b) {
          return 0;
       } else if (a < b) {
          return -1;
       } else {
          return 1;
       }
    }

and used as an ordinary variable.

    numbers.sort(intSorter);

