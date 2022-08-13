---
title: "Getting started with functional-programming"
slug: "getting-started-with-functional-programming"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Pure functions
Pure functions are self-contained, and have no side effects.
Given the same set of inputs, a pure function will always return the same output value.

The following function is pure:

<!-- language: lang-js -->

    function pure(data) {
        return data.total + 3;
    }

However, this function is not pure as it modifies an external variable:

<!-- language: lang-js -->

    function impure(data) {
        data.total += 3;
        return data.total;
    }

Example:

<!-- language: lang-js -->

    data = {
        total: 6
    };

    pure(data);   // outputs: 9
    impure(data); // outputs: 9 (but now data.total has changed)
    impure(data); // outputs: 12

## Higher-order functions
Higher-order functions take other functions as arguments and/or return them as results. They form the building blocks of functional programming. Most functional languages have some form of filter function, for example. This is a higher-order function, taking a list and a predicate (function that returns true or false) as arguments. 

Functions that do neither of these are often referred to as `first-order functions`.


<!-- language: lang-js -->

    function validate(number,predicate) {
        if (predicate) {    // Is Predicate defined
            return predicate(number);
        }
        return false;
    }

Here "predicate" is a function that will test for some condition involving its arguments and return true or false.

An example call for the above is:

<!-- language: lang-js -->

    validate(someNumber, function(arg) {
        return arg % 10 == 0;
        }
    );

<!-- language: scala -->
A common requirement is to add numbers within a range. By using higher-order functions we can extend this basic capability, applying a transformation function on each number before including it in the sum.

*You want to add all integers within a given range (using Scala)*

    def sumOfInts(a: Int, b: Int): Int = {
      if(a > b) 0
      else a + sumOfInts(a+1, b)
    }
*You want to add squares of all integers within a given range*

    def square(a: Int): Int = a * a

    def sumOfSquares(a: Int, b: Int): Int = {
      if(a > b) 0
      else square(a) + sumOfSquares(a + 1, b)
    }

Notice these things have 1 thing in common, that you want to apply a function on each argument and then add them.

Lets create a higher-order function to do both:

    def sumHOF(f: Int => Int, a: Int, b: Int): Int = {
      if(a > b) 0
      else f(a) + sumHOF(f, a + 1, b)
    }

You can call it like this:

    def identity(a: Int): Int = a

    def square(a: Int): Int = a * a

Notice that `sumOfInts` and `sumOfSquare` can be defined as:

    def sumOfInts(a: Int, b: Int): Int = sumHOF(identity, a, b)

    def sumOfSquares(a: Int, b: Int): Int = sumHOF(square, a, b)

As you can see from this simple example, higher-order functions provide more generalized solutions and reducing code duplication.

> I have used Scala By Example - by Martin Odersky as a reference.

## Currying
Currying is the process of transforming a function that takes multiple arguments into a sequence of functions that each has only a single parameter. Currying is related to, but not the same as, partial application.

Let's consider the following function in JavaScript:
    
    var add = (x, y) => x + y

We can use the definition of currying to rewrite the add function:

    var add = x => y => x + y

This new version takes a single parameter, `x`, and returns a function that takes a single parameter, `y`, which will ultimately return the result of adding `x` and `y`.

    var add5 = add(5)
    var fifteen = add5(10) // fifteen = 15

Another example is when we have the following functions that put brackets around strings:

    var generalBracket = (prefix, str, suffix) => prefix + str + suffix

Now, every time we use `generalBracket` we have to pass in the brackets:

    var bracketedJim = generalBracket("{", "Jim", "}") // "{Jim}"
    var doubleBracketedJim = generalBracket("{{", "Jim", "}}") // "{{Jim}}"

Besides, if we pass in the strings that are not brackets, our function still return a wrong result. Let's fix that:

    var generalBracket = (prefix, suffix) => str => prefix + str + suffix
    var bracket = generalBracket("{", "}")
    var doubleBracket = generalBracket("{{", "}}")

Notice that both `bracket` and `doubleBracket` are now functions waiting for their final parameter:

    var bracketedJim = bracket("Jim") // "{Jim}"
    var doubleBracketedJim = doubleBracket("Jim") // "{{Jim}}"



## Immutability
In traditional object-oriented languages, `x = x + 1` is a simple and legal expression. But in Functional Programming, it's *illegal*.

Variables don't exist in Functional Programming. Stored values are still called variables only because of history. In fact, they are *constants*. Once `x` takes a value, it's that value for life.

So, if a *variable* is a *constant*, then how can we change its value?

Functional Programming deals with changes to values in a record by making a copy of the record with the values changed.

For example, instead of doing:

    var numbers = [1, 2, 3];
    numbers[0] += 1; // numbers = [2, 2, 3];
    

You do:
    
    var numbers = [1, 2, 3];
    var newNumbers = numbers.map(function(number) {
        if (numbers.indexOf(number) == 0)
            return number + 1
        return number
    });
    console.log(newNumbers) // prints [2, 2, 3]

And there are no loops in Functional Programming. We use recursion or higher-order functions like `map`, `filter` and `reduce` to avoid looping.

Let's create a simple loop in JavaScript:

    var acc = 0;
    for (var i = 1; i <= 10; ++i)
        acc += i;
    console.log(acc); // prints 55

We can still do better by changing `acc`'s lifetime from global to local:

    function sumRange(start, end, acc) {
        if (start > end)
            return acc;
        return sumRange(start + 1, end, acc + start)
    }
    console.log(sumRange(1, 10, 0)); // 55

No variables or loops mean simpler, safer and more readable code (especially when debugging or testing - you don't need to worry about the value of `x` after a number of statements, it will never change).



