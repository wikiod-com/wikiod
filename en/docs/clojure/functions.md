---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Defining Functions
# Functions are defined with five components:


The header, which includes the `defn` keyword, the name of the function.
    
    (defn welcome ....)


An optional Docstring that explains and document what the function does.

    (defn welcome 
        "Return a welcome message to the world"
         ...)

Parameters listed in brackets.

    (defn welcome 
        "Return a welcome message"
        [name]
        ...)


The body, which describes the procedures the function carries out.

    (defn welcome 
        "Return a welcome message"
        [name]
        (str "Hello, " name "!"))

Calling it:

    => (welcome "World")

    "Hello, World!"


## Parameters and Arity
Clojure functions can be defined with zero or more parameters.

    (defn welcome
        "Without parameters"
        []
        "Hello!")

    (defn square
        "Take one parameter"
        [x]
        (* x x))

    (defn multiplier
        "Two parameters"
        [x y]
        (* x y))

# Arity

The number of arguments a function takes. Functions support *arity overloading*, which means that functions in Clojure allow for more than one "set" of arguments. 

    (defn sum-args
      ;; 3 arguments
      ([x y z]
         (+ x y z))
      ;; 2 arguments
      ([x y]
         (+ x y))
      ;; 1 argument 
      ([x]
         (+ x 1)))

The arities don't have to do the same job, each arity can do something unrelated:

    (defn do-something
      ;; 2 arguments
      ([first second]
         (str first " " second))
      ;; 1 argument 
      ([x]
         (* x x x)))

## Defining Variadic Functions
A Clojure function can be defined to take an arbitrary number of arguments, using the symbol **&** in its argument list. All remaining arguments are collected as a sequence.

```
(defn sum [& args]
  (apply + args))

(defn sum-and-multiply [x & args]
  (* x (apply + args)))
```

Calling:
```
=> (sum 1 11 23 42)
77

=> (sum-and-multiply 2 1 2 3)  ;; 2*(1+2+3)
12
```

## Defining anonymous functions
There are two ways to define an anonymous function: the full syntax and a shorthand.

## Full Anonymous Function Syntax ##

    (fn [x y] (+ x y))

This expression evaluates to a function. Any syntax you can use with a function defined with `defn` (`&`, argument destructuring, etc.), you can also do with with the `fn` form. `defn` is actually a macro that just does `(def (fn ...))`.
    
## Shorthand Anonymous Function Syntax ##

    #(+ %1 %2)

This is the shorthand notation. Using the shorthand notation, you don't have to name arguments explicitly; they'll be assigned the names `%1`, `%2`, `%3` and so on according to the order they're passed in. If the function only has one argument, its argument is called just `%`.

## When To Use Each ##

The shorthand notation has some limitations. You can't destructure an argument, and you can't nest shorthand anonymous functions. The following code throws an error:

    (def f #(map #(+ %1 2) %1))

## Supported Syntax ##

You _can_ use varargs with shorthand anonymous functions. This is completely legal:

    #(every? even? %&)

It takes a variable number of arguments and returns true if every one of them is even:

    (#(every? even? %&) 2 4 6 8)
    ;; true
    (#(every? even? %&) 1 2 4 6)
    ;; false

Despite the apparent contradiction, it is possible to write a named anonymous function by including a name, as in the following example. This is especially useful if the function needs to call itself but also in stack traces.

    (fn addition [& addends] (apply + addends))


