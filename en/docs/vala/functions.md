---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Functions are pieces of code that can be executed by other functions of your program. 

Your program always starts with the `main` function.

See also [asynchronous functions](https://www.wikiod.com/vala/async-and-yield).


Methods are exactly the same as function, but they act on an object instance.

## Basic functions
A function is defined by at least its return type and an unique name.

<!-- language: lang-vala -->
    void say_hello () {
        print ("Hello, world!\n");   
    }

Then, to call it just use the name of the function followed by a parenthese.

<!-- language: lang-vala -->
    say_hello ();

---
Functions can also have parameters between the parentheses, defined by their types and names and separated by commas. Then you can just use them as normal variables into your function.

<!-- language: lang-vala -->
    int greet (string name, string family_name) {
        print ("Hello, %s %s!\n", name, family_name);
    }

To call a function with parameters, just put a variable or a value between the parentheses.

<!-- language: lang-vala -->
    string name = "John";
    greet (name, "Doe");
        
---
You can also return a value which can be assigned to a variable with the `return` keyword.

<!-- language: lang-vala -->
    int add (int a, int b) {
        return a + b;
    }

    int sum = add (24, 18);

All code path should end with a `return` statement. For instance, the following code is invalid.

<!-- language: lang-vala -->
    int positive_sub (int a, int b) {
        if (a >= b) {
            return a - b;
        } else {
            // Nothing is returned in this case.
            print ("%d\n", b - a);
        }
    }

## Optional parameters
Parameters can be marked as optional by giving them a default value. Optional parameters can be omitted when calling the function.

<!-- language: lang-vala -->
    string greet (string name, string language = "English") {
        if (language == "English") {
            return @"Hello, $name!";
        } else {
            return @"Sorry $name, I don't speak $language";
        }
    }

    greet ("John");
    greet ("Jane", "Italian");

## Out and Ref parameters
Value types (structures and enumerations) are passed by value to functions: a copy will be given to the function, not a reference to the variable. So the following function won't do anything.

<!-- language: lang-vala -->
    void add_three (int x) {
        x += 3;
    }

    int a = 39;
    add_three (a);
    assert (a == 39); // a is still 39

To change this behavior you can use the `ref` keyword.

<!-- language: lang-vala -->
    // Add it to the function declaration
    void add_three (ref int x) {
        x += 3;
    }

    int a = 39;
    add_three (ref a); // And when you call it
    assert (a == 42); // It works!

`out` works the same way, but you are forced to set a value to this variable before the end of the function.

<!-- language: lang-vala -->
    string content;
    FileUtils.get_contents ("file.txt", out content);

    // OK even if content was not initialized, because
    // we are sure that it got a value in the function above.
    print (content);

## Contract programming
You can assert that parameters have certain values with `requires`.

<!-- language: lang-vala -->
    int fib (int i) requires (i > 0) {
        if (i == 1) {
            return i;
        } else {
            return fib (i - 1) + fib (i - 2);
        }
    }

    fib (-1);

You won't get any error during the compilation, but you'll get an error when running your program and the function won't run.

You can also assert that the return value matches a certain condition with `ensures`

<!-- language: lang-vala -->
    int add (int a, int b) ensures (result >= a && result >= b) {
        return a + b;
    }

You can have as many `requires` and `ensures` as you want.

## Variable arguments
<!-- language: lang-vala -->
    int sum (int x, ...) {
        int result = x;
        va_list list = va_list ();
        for (int? y = list.arg<int?> (); y != null; y = list.arg<int?> ()) {
            result += y;
        }
        return result;
    }

    int a = sum (1, 2, 3, 36);

With this function, you can pass as many int as you want. If you pass something else, you'll either get an unexpected value or a segmentation fault.

