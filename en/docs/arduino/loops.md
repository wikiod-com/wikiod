---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
 - for (declaration, condition, iteration) { }
 - while (condition) { }
 - do { } while (condition)

`General Remark` If you intend to create a loop to wait for something to happen, you're probably on the wrong track here. Rather remember that all code after setup() is run from a method called loop(). So if you need to wait for something, it's easiest to not do anything (or only other independent stuff) and come back to check for the waiting condition next time. 


`do { } while(condition)` will not evaluate the condition statement until after the first iteration. This is important to keep in mind if the condition statement has side effects.

## Do ... While
A `do while` loop is the same as a `while` loop, except that it is guaranteed to execute at least one time.

The following loop will execute 100 times.

    int i = 0;
    do {
        i++;
    } while (i < 100);

A similar loop, but with a different condition, will execute 1 time.

    int i = 0;
    do {
        i++;
    } while (i < 0);

If the above loop were merely a `while` loop, it would execute 0 times, because the condition would evaluate to `false` before the first iteration. But since it is a `do while` loop, it executes once, then checks its condition before executing again.

## While
A `while` loop will evaluate its condition, and if `true`, it will execute the code inside and start over. That is, as long as its condition evaluates to `true`, the `while` loop will execute over and over.

----------

This loop will execute 100 times, each time adding 1 to the variable `num`:

    int num = 0;
    while (num < 100) {
        // do something
        num++;
    }

The above loop is equivalent to a `for` loop:

    for (int i = 0; i < 100; i++) {
        // do something
    }

----------

This loop will execute forever:

    while (true) {
        // do something
    }

The above loop is equivalent to a `for` loop:

    for (;;) {
        // do something
    }


## For
`for` loops are simplified syntax for a very common loop pattern, which could be accomplished  in more lines with a `while` loop.

The following is a common example of a `for` loop, which will execute 100 times and then stop.

    for (int i = 0; i < 100; i++) {
        // do something
    }

This is equivalent to a `while` loop:

    int num = 0;
    while (num < 100) {
        // do something
        num++;
    }

You can create an endless loop by omitting the condition.

    for (;;) {
        // do something
    }

This is equivalent to a `while` loop:

    while (true) {
        // do something
    }


## Flow Control
There are some ways to break or change a loop's flow.

`break;` will exit the current loop, and will not execute any more lines within that loop.

`continue;` will not execute any more code within the current iteration of the loop, but will remain in the loop.

The following loop will execute 101 times (i =  0, 1, ..., 100 ) instead of 1000, due to the `break` statement:

    for (int i = 0; i < 1000; i++) {
        // execute this repeatedly with i = 0, 1, 2, ...
        if (i >= 100) {
            break;
        }
    }

The following loop will result in `j`'s value being 50 instead of 100, because of the `continue` statement:
   
    int j=0;
    for (int i = 0; i < 100; i++) {
        if (i % 2 == 0) { // if `i` is even
            continue;
        }
        j++;
    }
    // j has the value 50 now.

