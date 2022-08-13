---
title: "Closures"
slug: "closures"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

From the StackOverflow [closure](http://stackoverflow.com/tags/closures/info) tag:

*A closure is a first-class function that refers to (closes over) variables from the scope in which it was defined. If the closure still exists after its defining scope ends, the variables it closes over will continue to exist as well.*

**It is sometimes useful to consider closures and objects as similar.**

*The venerable master Qc Na was walking with his student, Anton. Hoping to prompt the master into a discussion, Anton said "Master, I have heard that objects are a very good thing - is this true?" Qc Na looked pityingly at his student and replied, "Foolish pupil - objects are merely a poor man's closures."
Chastised, Anton took his leave from his master and returned to his cell, intent on studying closures. He carefully read the entire "Lambda: The Ultimate..." series of papers and its cousins, and implemented a small Scheme interpreter with a closure-based object system. He learned much, and looked forward to informing his master of his progress.*

*On his next walk with Qc Na, Anton attempted to impress his master by saying "Master, I have diligently studied the matter, and now understand that objects are truly a poor man's closures." Qc Na responded by hitting Anton with his stick, saying "When will you learn? Closures are a poor man's object." At that moment, Anton became enlightened.*

Source: http://c2.com/cgi/wiki?ClosuresAndObjectsAreEquivalent

## Closure with static environment
A closure is a procedure that holds internal state:

**Define a procedure that returns a closure**

The procedure `make-an-adder` takes one argument `x` and returns a function that closes over the value. Or to put it another way, `x` is within the lexical scope of the returned function.

    #lang racket
    (define (make-an-adder x)
      (lambda (y)
        (+ y x)))
    
**Usage**

Calling the procedure `make-an-adder` returns a procedure that is a closure.

    Welcome to DrRacket, version 6.6 [3m].
    Language: racket, with debugging; memory limit: 128 MB.
    > (define 3adder (make-an-adder 3))
    > (3adder 4)
    7
    > (define 8adder (make-an-adder 8))
    > (8adder 4)
    12

