---
title: "Recursion"
slug: "recursion"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Using define
    #lang racket
    (define (sum-of-list l)
      (if (null? l)
          0
          (+ (car l)
             (sum-of-list (cdr l)))))
    (sum-of-list '(1 2 3 4 5)) ;; => 15
    

## Using let-rec
    #lang racket
    (letrec ([sum-of-list (λ (l)
                            (if (null? l)
                                0
                                (+ (car l) (sum-of-list (cdr l)))))])
      (sum-of-list '(1 2 3 4 5)))
    ;; => 15

It is possible to write mutually recursive functions with `letrec`:

    #lang racket
    (letrec ([even? (λ (n) (if (= n 0) #t (odd?  (sub1 n))))]
             [odd?  (λ (n) (if (= n 0) #f (even? (sub1 n))))])
      (list (even? 3)
            (odd? 5)))
    ;; => '(#f #t)

## Using a named let
A normal `let` form binds each value to its corresponding identifier, before executing the body. With a "named `let`", the body can then recursively be re-executed, passing a new value for each identifier.

    #lang racket
    (let sum-of-list ([l '(1 2 3)])
      (if (null? l)
          0
          (+ (car l) (sum-of-list (cdr l)))))
    ;; => 15

It is common to use `rec` as the name for the let, which gives:

    #lang racket
    (let rec ([l '(1 2 3 4 5)])
      (if (null? l)
          0
          (+ (car l) (rec (cdr l)))))
    ;; => 15


## Using rec
    #lang racket
    (require mzlib/etc)
    ((rec sum-of-list
       (λ (l)
         (if (null? l)
             0
             (+ (car l) (sum-of-list (cdr l))))))
     '(1 2 3 4 5))
    ;; => 15
    
    ;; Outside of the rec form, sum-of-list gives an error:
    ;; sum-of-list: undefined;
    ;;  cannot reference an identifier before its definition


This is similar to `define`, but the `sum-of-list` identifier is not visible outside of the `rec` form.

To avoid using an explicit `λ`, it is possible to replace `sum-of-list` with `(sum-of-list args ...)`:

    #lang racket
    (require mzlib/etc)
    ((rec (sum-of-list l)
       (if (null? l)
           0
           (+ (car l) (sum-of-list (cdr l)))))
     '(1 2 3 4 5))
    ;; => 15

## Using higher-order functions instead of recursion
It is common practice to use [higher order functions][1] instead of recursion, if there is a higher order function which expresses the right recursion pattern. In our case, `sum-of-numbers` can be defined using `foldl`:

    #lang racket
    (define (sum-of-numbers l)
      (foldl + 0 l))
    (sum-of-numbers '(1 2 3 4 5)) ;; => 15

It is possible to call `foldl` directly on the list:

    #lang racket
    (foldl + 0 '(1 2 3 4 5)) ;; => 15

  [1]: https://www.wikiod.com/racket/higher-order-functions

