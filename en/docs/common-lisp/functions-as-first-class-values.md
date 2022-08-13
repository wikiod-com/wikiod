---
title: "Functions as first class values"
slug: "functions-as-first-class-values"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Syntax
 - (function name) ; retrieves the function object of that name
 - #'name ; syntactic sugar for (function name)
 - (symbol-function symbol) ; returns the function bound to symbol
 - (funcall function args...) ; call function with args
 - (apply function arglist) ; call function with arguments given in a list
 - (apply function arg1 arg2 ... argn arglist) ; call function with arguments given by arg1, arg2, ..., argn, and the rest in the list arglist

## Parameters
| Parameter | Details |
| ------ | ------ |
| name   | some (unevaluated) symbol which names a function |
| symbol | a symbol |
| function | a function which is to be called |
| args... | zero or more arguments (*not* a list of arguments)
| arglist | a list containing arguments to be passed to a function
| arg1, arg2, ..., argn | each is a single argument to be passed to a function

When talking about Lisp-like languages there is a common distinction between what is known as a Lisp-1 and a Lisp-2. In a Lisp-1, symbols only have a value and if a symbol refers to a function then the value of that symbol will be that function.
In a Lisp-2, symbols can have separate associated values and functions and so a special form is required to refer to the function stored in a symbol instead of the value.

Common Lisp is basically a Lisp-2 however there are in fact more than 2 namespaces (things that symbols can refer to) -- symbols can refer to values, functions, types and tags, for example.

## Closures
Functions remember the lexical scope they where defined in. Because of this, we can enclose a lambda in a let to define closures.

    (defvar *counter* (let ((count 0))
                        (lambda () (incf count))))
    
    (funcall *counter*) ;; => 1
    (funcall *counter*) ;; = 2

In the example above, the counter variable is only accessible to the anonymous function. This is more clearly seen in the following example

    (defvar *counter-1* (make-counter))
    (defvar *counter-2* (make-counter))
    
    (funcall *counter-1*) ;; => 1
    (funcall *counter-1*) ;; => 2
    (funcall *counter-2*) ;; => 1
    (funcall *counter-1*) ;; => 3

## Defining anonymous functions
Functions in Common Lisp are *first class values*. An anonymous function can be created by using `lambda`. For example, here is a function of 3 arguments which we then call using `funcall`
```
CL-USER> (lambda (a b c) (+ a (* b c)))
#<FUNCTION (LAMBDA (A B C)) {10034F484B}>
CL-USER> (defvar *foo* (lambda (a b c) (+ a (* b c))))
*FOO*
CL-USER> (funcall *foo* 1 2 3)
7
```
Anonymous functions can also be used directly. Common Lisp provides a syntax for it.
```
((lambda (a b c) (+ a (* b c)))    ; the lambda expression as the first
                                   ; element in a form
  1 2 3)                           ; followed by the arguments
```
Anonymous functions can also be stored as global functions:
```
(let ((a-function (lambda (a b c) (+ a (* b c)))))      ; our anonymous function
  (setf (symbol-function 'some-function) a-function))   ; storing it

(some-function 1 2 3)                                   ; calling it with the name
```

**Quoted lambda expressions are not functions**

Note that quoted lambda expressions are not functions in Common Lisp. This does **not** work:

    (funcall '(lambda (x) x)
             42)

To convert a quoted lambda expression to a function use `coerce`, `eval` or `funcall`:

    CL-USER > (coerce '(lambda (x) x) 'function)
    #<anonymous interpreted function 4060000A7C>

    CL-USER > (eval '(lambda (x) x))
    #<anonymous interpreted function 4060000B9C>

    CL-USER > (compile nil '(lambda (x) x))
    #<Function 17 4060000CCC>


## Higher order functions
Common Lisp contains many higher order functions which are passed functions for arguments and call them. Perhaps the most fundamental are [`funcall`][1] and [`apply`][2]:

<!-- language: lisp -->

    CL-USER> (list 1 2 3)
    (1 2 3)
    CL-USER> (funcall #'list 1 2 3)
    (1 2 3)
    CL-USER> (funcall #'list 1 2 3 4 5)
    (1 2 3 4 5)
    CL-USER> (apply #'list '(1 2 3))
    (1 2 3)
    CL-USER> (apply #'list 1 2 '(4 5))
    (1 2 3 4 5)
    CL-USER> (apply #'+ 1 (list 2 3))
    6
    CL-USER> (defun my-funcall (function &rest args)
               (apply function args))
    MY-FUNCALL
    CL-USER> (my-funcall #'list 1 2 3)
    (1 2 3)


There are many other higher order-function which, for example, apply a function many times to elements of a list.
```
CL-USER> (map 'list #'/ '(1 2 3 4))
(1 1/2 1/3 1/4)
CL-USER> (map 'vector #'+ '(1 2 3 4 5) #(5 4 3 2 10))
#(6 6 6 6 15)
CL-USER> (reduce #'+ '(1 2 3 4 5))
15
CL-USER> (remove-if #'evenp '(1 2 3 4 5))
(1 3 5)
```


  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm
  [2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_apply.htm#apply

## Summing a list


## Referring to Existing Functions
Any symbol in Common Lisp has a slot for a variable to be bound and a separate slot for a function to be bound.

Note that the naming in this example is only for illustration. Global variables should not be named `foo`, but `*foo*`. The latter notation is a convention to make it clear that the variable is a *special* variable using *dynamic binding*.

```
CL-USER> (boundp 'foo) ;is FOO defined as a variable?
NIL
CL-USER> (defvar foo 7)
FOO
CL-USER> (boundp 'foo)
T
CL-USER> foo
7
CL-USER> (symbol-value 'foo)
7
CL-USER> (fboundp 'foo) ;is FOO defined as a function?
NIL
CL-USER> (defun foo (x y) (+ (* x x) (* y y)))
FOO
CL-USER> (fboundp 'foo)
T
CL-USER> foo
7
CL-USER> (symbol-function 'foo)
#<FUNCTION FOO>
CL-USER> (function foo)
#<FUNCTION FOO>
CL-USER> (equalp (quote #'foo) (quote (function foo)))
T
CL-USER> (eq (symbol-function 'foo) #'foo)
T
CL-USER> (foo 4 3)
25
CL-USER> (funcall foo 4 3)
;get an error: 7 is not a function
CL-USER> (funcall #'foo 4 3)
25
CL-USER> (defvar bar #'foo)
BAR
CL-USER> bar
#<FUNCTION FOO>
CL-USER> (funcall bar 4 3)
25
CL-USER> #'+
#<FUNCTION +>
CL-USER> (funcall #'+ 2 3)
5
```

## Implementing reverse and revappend


## Defining functions that take functions and return functions
A simple example:

    CL-USER> (defun make-apply-twice (fun)
               "return a new function that applies twice the function`fun' to its argument"
               (lambda (x)
                 (funcall fun (funcall fun x))))
    MAKE-APPLY-TWICE
    CL-USER> (funcall (make-apply-twice #'1+) 3)
    5
    CL-USER> (let ((pow4 (make-apply-twice (lambda (x) (* x x)))))
               (funcall pow4 3))
    81

The classical example of [function composition](https://en.wikipedia.org/wiki/Function_composition): (*f* ∘ *g* ∘ *h*)(*x*) = *f* (*g* (*h* (*x*)):

    CL-USER> (defun compose (&rest funs)
               "return a new function obtained by the functional compositions of the parameters"
               (if (null funs) 
                   #'identity
                   (let ((rest-funs (apply #'compose (rest funs))))
                     (lambda (x) (funcall (first funs) (funcall rest-funs x))))))
    COMPOSE
    CL-USER> (defun square (x) (* x x))
    SQUARE
    CL-USER> (funcall (compose #'square #'1+ #'square) 3)
    100  ;; => equivalent to (square (1+ (square 3)))



