---
title: "Pattern matching"
slug: "pattern-matching"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Overview
The two main libraries providing pattern matching in Common Lisp are [Optima][1] and [Trivia][2]. Both provide a similar matching API and syntax. However trivia provides a unified interface to extend matching, `defpattern`.


  [1]: https://github.com/m2ym/optima
  [2]: https://github.com/guicho271828/trivia

## Dispatching Clack requests
Because a clack request is represented as a plist, we can use pattern matching as the entry point to the clack app as a way to route request to their appropriate controllers  

    (defvar *app*
      (lambda (env)
        (match env
          ((plist :request-method :get
                  :request-uri uri)
           (match uri
             ("/" (top-level))
             ((ppcre "/tag/(\\w+)/$" name) (tag-page name)))))))

Note: To start `*app*` we pass it to clackup. ej `(clack:clackup *app*)` 

## defun-match
Using pattern matching one can intertwine function definition and pattern matching, similar to SML.

    (trivia:defun-match fib (index)
      "Return the corresponding term for INDEX."
      (0 1)
      (1 1)
      (index (+ (fib (1- index)) (fib (- index 2)))))
    
    (fib 5)
    ;; => 8

## Constructor patterns
Cons-cells, structures, vectors, lists and such can be matched with constructor patterns.

    (loop for i from 1 to 30
          do (format t "~5<~a~;~>"
                     (match (cons (mod i 3)
                                  (mod i 5))
                       ((cons 0 0) "Fizzbuzz")
                       ((cons 0 _) "Fizz")
                       ((cons _ 0) "Buzz")
                       (_ i)))
          when (zerop (mod i 5)) do (terpri))
    ; 1    2    Fizz 4    Buzz 
    ; Fizz 7    8    Fizz Buzz 
    ; 11   Fizz 13   14   Fizzbuzz
    ; 16   17   Fizz 19   Buzz 
    ; Fizz 22   23   Fizz Buzz 
    ; 26   Fizz 28   29   Fizzbuzz


## Guard-pattern
Guard patterns can be used to check that a value satisfies an arbitrary test-form.

    (dotimes (i 5)
      (format t "~d: ~a~%"
              i (match i
                  ((guard x (oddp x)) "Odd!")
                  (_ "Even!"))))
    ; 0: Even!
    ; 1: Odd!
    ; 2: Even!
    ; 3: Odd!
    ; 4: Even!

