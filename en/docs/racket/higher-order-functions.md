---
title: "Higher Order Functions"
slug: "higher-order-functions"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Map
Map applies a function to every element of a list:

    map: (a -> b) (listof a) -> (listof b)

    > (map (lambda (x) (* x 2)) (list 1 2 3 4 5)
    (list 2 4 6 8 10)

    > (map sqrt (list 1 4 9))
    (list 1 2 3)

    > (map (lambda (x) (if (even? x) "even" "odd")) (list 1 2 3))
    (list "odd" "even" "odd")

## Fold
**Fold Right** successively applies a two-argument function to every element in a list from left to right starting with a base value:
  
    foldr: (a b -> b) b (listof a) -> b

    > (foldr + 0 (list 1 2 3 4))
    10

    > (foldr string-append "" (list "h" "e" "l" "l" "o"))
    "hello"

    > (foldr cons empty (list 1 2 3 4))
    (list 1 2 3 4)

**Fold Left** performs the same action in the opposite direction:

    foldl: (a b -> b) b (listof a) -> b

    > (foldl + 0 (list 1 2 3 4)
    10

    > (foldl string-append "" (list "h" "e" "l" "l" "o"))
    "olleh"

    > (foldl cons empty (list 1 2 3 4))
    (list 4 3 2 1)


## Filter
`filter` returns a list of each item in the given list for which the given predicate returns a non-`#f` value.

    ;; Get only even numbers in a list
    > (filter even? '(1 2 3 4))
    '(2 4)

    ;; Get all square numbers from 1 to 100
    > (filter (lambda (n) (integer? (sqrt n))) (range 1 100))
    '(1 4 9 16 25 36 49 64 81) 

## Compose
Lets you compose several functions `f₀ f₁ … fₙ`. It returns a function that will successively apply `fₙ` to its arguments, then `fₙ₋₁` to the result of `fₙ` and so on. Function are applied from right to left, like for mathematical function composition: `(f ∘ g ∘ h)(x) = f(g(h(x)))`.

    > ((compose sqrt +) 16 9)
    5
    > ((compose - sqrt) 16)
    -4

The arity of each function should include the the number of returned values of the function immediately to its right. The rightmost function determines the arity of the whole composition. The compose1 function imposes that the functions return 1 value and expect 1 argument. However, compose1 does not restrict the input arity of the last function, nor the output arity of the first function.

    [n input]--> first-function -->[1 output]--> ... last function -->[m output].

    ((compose + values) 1 2 3 4)
    10
    > ((compose1 + values) 1 2 3 4)
    XX result arity mismatch;
     expected number of values not received
      expected: 1
      received: 4
      values...:

## Curry
Returns a partially applied function.

    > ((curry + 10) 20)
    30

`curryr` can be used when the arguments need to be inserted at the end. In other words, `(curryr list 1 2)` will produce a function expecting some `new-arguments ...`. When called, that new function will in turn call `(list new-arguments ... 1 2)`.

    > (((curryr list) 1 2) 3 4)
    '(3 4 1 2)
    > ((curryr list 1 2) 3 4)
    '(3 4 1 2) 
    > ((curryr - 30) 40)
    10
    > (((curryr -) 30 40))
    10

