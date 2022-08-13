---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
- (define (name arguments ...) body)

- (function arguments ...)

## Simple Function Calls
You can call a function in Racket by wrapping it in parentheses with the arguments after it. This looks like `(function argument ...)`.

    > (define (f x) x)
    > (f 1)
    1
    > (f "salmon")
    "salmon"
    > (define (g x y) (string-append x y))
    > (g "large" "salmon")
    "largesalmon"
    > (g "large " "salmon")
    "large salmon"

Operations like `+` and `*` are functions as well, and they use the same syntax as calling `f` or `g`.

    > (+ 1 2)
    3
    > (* 3 4)
    12
    > (+ (* 3 3) (* 4 4))
    25

For more information and examples, see [Function Calls][guide: Function Calls] in the Racket Guide.

  [guide: Function Calls]: http://docs.racket-lang.org/guide/syntax-overview.html#%28part._.Function_.Calls__.Procedure_.Applications_%29

## The `apply` function
If you have a list, and you want to use the elements of that list as the arguments to a function, what you want is `apply`:

    > (apply string-append (list "hello" " " "and hi" " " "are both words"))
    "hello and hi are both words"
    > (apply + (list 1 2 3 4))
    10
    > (apply append (list (list "a" "b" "c") (list 1 2 3) (list "do" "re" "mi")))
    (list "a" "b" "c" 1 2 3 "do" "re" "me")

`apply` takes two arguments. The first argument is the function to apply, and the second argument is the list containing the arguments.

An `apply` call like

    (apply + (list 1 2 3 4))

Is equivalent to

    (+ 1 2 3 4)

The major advantage of `apply` is that it works on arbitrary computed lists, including appended lists and lists that come from function arguments.

    > (apply + (append (list 1 2 3 4) (list 2 3 4)))
    19
    > (define (sum lst)
        (apply + lst))
    > (sum (list 1 2 3 4))
    10
    > (sum (append (list 1 2 3 4) (list 2 3 4)))
    19

For more information and examples, see [The `apply` function][guide: The apply function] in the Racket Guide.

  [guide: The apply function]: http://docs.racket-lang.org/guide/application.html#%28part._apply%29

## Keyword arguments
Racket functions can also have *keyword arguments*, which are specified with a keyword followed by the argument expression. A keyword begins with the characters `#:`, so a keyword argument looks like `#:keyword arg-expr`. Within a function call this looks like `(function #:keyword arg-expr)`.

    > (define (hello #:name n)
        (string-append "Hello " n))
    > (hello #:name "John")
    "Hello John"
    > (hello #:name "Sarah")
    "Hello Sarah"
    > (define (kinetic-energy #:mass m #:velocity v)
        (* 1/2 m (sqr v)))
    > (kinetic-energy #:mass 2 #:velocity 1)
    1
    > (kinetic-energy #:mass 6 #:velocity 2)
    12

For more information and examples, see [Keyword Arguments][guide: Keyword Arguments] in the Racket Guide.

  [guide: Keyword Arguments]: http://docs.racket-lang.org/guide/application.html#%28part._keyword-args%29

## Function Definitions
Functions in Racket can be created with the [`lambda`][1] form. The form takes a list of arguments and a body.

    (lambda (x y) (* x y))

In the example above, the function takes in two arguments and returns the result of multiplying them.

    > ((lambda (x y) (* x y)) 4 4)
    16
    > ((lambda (x y) (* x y)) 3 2)
    6

It's tedious to re-write the function and its body every time we want to multiply two numbers, so let's give it a name. To give it a name, use the [`define`][2] form. This will bind functions to a name.

    (define multiply (lambda (x y) (* x y)))

Now we can refer to our function by calling `multiply`

    > (multiply 5 2)
    10

Since it is very common to bind procedures to names, Racket provides a shorthand to define functions using the define form.

    (define (multiply x y) (* x y))

For more information and examples, see [Functions: lambda][guide: Functions: lambda] in the Racket Guide.

  [1]: http://docs.racket-lang.org/guide/lambda.html
  [2]: http://docs.racket-lang.org/guide/define.html
  [guide: Functions: lambda]: http://docs.racket-lang.org/guide/lambda.html

