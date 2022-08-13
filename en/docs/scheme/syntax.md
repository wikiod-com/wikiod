---
title: "Syntax"
slug: "syntax"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## S-Expression
An expression in Scheme is what is going to get executed. A S-expression, as it's usually called starts with a `(` and end with a `)`. The first member of the expression is what is going to get executed. The following member of the expression are the parameters that will be sent to the expression during the evaluation of the expression.

For example adding numbers:

    (+ 1 2 3)

In this case, `+` is a symbol to a *add* function that takes multiple parameters. `1`, `2` and `3` are sent to the `+` function.

S-Expression may contain S-Expressions as parameters as shown in the following example:

    (if (< x y)
      x
      y)
    
Which can be read as if `x` is less than `y` return `x` else return `y`. In this example we evaluate the condition expression, depending on the resolved value, either x or y will be returned. It could be evaluated to this

    (if #t x y)
    x
    (if #f x y)
    y

A less obvious example for beginners is to have a S-Expression as part of the first member of a S-Expression. This way, we can change the behaviour of a method by changing the function that will be called without having to create branches with the same parameters. Here's a quick example of an expression that either add or substract numbers if x is below y.

    ((if (< x y) + -) 
      1 2 3)

If `x` is below `y`, the expression will be evaluated as:

    (+ 1 2 3)
    6

otherwise

    (- 1 2 3)
    -4

As you can see, Scheme allow the programmer to build up complex piece of code while giving the programmer the tools to prevent duplicating code. In other languages we could see the same example written as such:

   (if (< x y)
     (+ 1 2 3)
     (- 1 2 3))

The problem with this method is that we duplicate a lot of code while the only thing that change is the method being called. This example is fairly simple but with more condition we could see a lot of similar lines duplicated.

## Simple let macro
The let expressions in scheme are in fact macros. They can be expressed with lambdas. A simple let might look like this:

    (let ((x 1) (y 2))
      (+ x y))

It will return 3 as the value of the last expression of the let body is returned. As you can see, a let-expression is actually executing something. If we translate this part of code with lambdas, we'd get something like this:

    ((lambda (x y) (+ x y)) 1 2)

Here we can see that we're calling the anonymous lambda with 1 and 2 directly. So the result in this case is also 3.

With that in mind, we understand that a let expression is composed of 2 parts. It has parameters and a body like a lambda has, but the difference is that let expression are called after right after their evaluation.

To explain how a let expression work from an abstract to concrete view, it would look like this.

    (let params body ...)
    (let (param1 param2 ...) body ...)
    (let ((p1 val1) (p2 val2) ...) body ...)

The parameters are a list of pair of `(name value)` to be used in the body of the `let`.

**Why use let expression?**

Let expressions are particularly useful to store variables in a method just like initializations of variable in c like languages. It is favorable to the use of `define` because out of the let expression, the variables are gone... Using a define is actually adding a variable to the current execution environment. Variables that are added to the global environment cannot be removed. Let expression are safe to use anywhere. It can also be used to ghost variables without touching the parent scopes.

For example:

    (let ((x 1))
      (let ((x 2) (y x))
        (display x)
        (display y))
      (display x))

It will print:

    2
    1
    1

In this case, `x` is defined with 1, then ghosted by the `x` in the second `let` with the value `2`. The variable `y` is initiated with the value `x` of the parent scope. After the inner `let` expression is executed, it display the initial value of `x` with 1. The inner `let` expression didn't change the value of the parent scope.

Whenever you need to initialize variables, you should be using let expressions like this:

    (let (
      (user (get-current-user session))
      (db (get-current-db session))
      (ids (get-active-ids session))
      )
      (mark-task-completed db user ids)
      (change-last-logged db user)
      (db-commit db))

Here in this example, the variables are initialized and used multiple time in the code block. And when the let expression is finished, the variables are automatically freed as they are not necessary anymore.



## Dotted syntax for pairs
There is a particular syntax that allow us to write `cons` cell in a more compact way than using the `cons` constructor.

A pair can be written as such:

    '(1 . 2) == (cons 1 2)

The big difference is that we can create `pairs` using quote. Otherwise, Scheme would create a proper list `(1 . (2 . '()))`. 

The dot syntax force the expression to have only 2 members. Each member can be of any type including pairs.

    '(1 . (2 . (3 . 4)))
    > (1 2 3 . 4)

Note that the improper list should be displayed with a dot at the end to show that the `cdr` of the last pair of the list isn't the empty list `'()`.

This way of showing lists is sometime confusing as the following expression would be expressed not like one would expect it.

    '((1 . 2) . ( 3 . 4))
    > ((1 . 2) 3 . 4)

Since list usually skip the `.`, the first argument of the list would be `(1 . 2)`, the second argument would be `3` but since the list is improper, the last `.` is shown to show that the last element of the list isn't `'()`.
Even thought, the data is shown in a different way, the internal data is as it was created.

