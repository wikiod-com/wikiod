---
title: "Getting started with racket"
slug: "getting-started-with-racket"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Racket
Racket is a full-spectrum programming language. It goes beyond Lisp and Scheme with dialects that support objects, types, laziness, and more. Racket enables programmers to link components written in different dialects, and it empowers programmers to create new, project-specific dialects. Racket's libraries support applications from web servers and databases to GUIs and charts.

The offical, comprehensive and very well-written documentation can be found at [http://docs.racket-lang.org/][1]. On this site (Stack Overflow Documentation) you can find user-contributed examples. 

Installation
------------

Go to http://racket-lang.org and click the download button.


  [1]: http://docs.racket-lang.org

## Hello, World!
The following example declares a piece of code to be written in Racket, and then prints the string `Hello, world`. 

    #lang racket
    "Hello, world!"

Racket code can either be run directly from the command line or on the DrRacket IDE. Typing `racket` on the command line will start a REPL, and typing `racket` followed by a file name will evaluate the contents of the file. For example, suppose the file `hello.rkt` contains the above code. Here is an example of running Racket on the command line.

    $ racket
    Welcome to Racket v6.5.
    > "Hello, world!"
    "Hello, world!"
    > (exit)
    $ racket hello.rkt
    "Hello, world!"

## Simple Recursive Function Definition
In Racket, we use recursion very frequently. Here is an example of a function that sums all of the numbers from zero to the parameter, `n`.

    (define (sum n)
        (if (zero? n)
            0
            (+ n (sum (sub1 n)))))

Note that there are many helpful convenience based functions used here, such as `zero?` and `sub1`. Each respectively does just what you might expect: `zero?` returns a boolean which says whether the given number was equal to zero, and `sub1` subtracts one from its argument.

## Installation or Setup
The installation is very simple. If you are used to this kind of thing, just go to https://download.racket-lang.org. If you prefer, there are more detailed step-by-step installation instructions for the following systems:

* [Installation steps (Windows)][1]
* [Installation steps (Linux)][2]
* [Installation steps (macOS)][3]


  [1]: https://www.wikiod.com/racket/installation-steps-windows
  [2]: https://www.wikiod.com/racket/installation-steps-linux
  [3]: https://www.wikiod.com/racket/installation-steps-macos

## Find Racket sources in all subdirs
    #lang racket 
    (for ([path (in-directory)]
      #:when (regexp-match? #rx"[.]rkt$" path))
      (printf "source file: ~a\n" path))

The `#lang` line specifies the programming language of this file. `#lang racket` we are using the baseline, battery-included Racket programming language. Other languages ranen from  Racket flavors such as Type Racket (`#lang typed/racket`) or the documentation language Scribble (`#lang scribble`), to small convenience languages such as the language for defining packages (`#lang info`).

The `in-directory` function constructs a sequence that walks a directory tree (starting with the current directory, by default) and generates paths in the tree. The `for` form binds `path` to each path in the sequence, and `regexp-match?` applies a pattern to the path.

To run the example, install Racket, start DrRacket, paste the example program into the top area in DrRacket, and click the Run button. Alternatively, save the program to a file and run `racket` from the command line on the file.





