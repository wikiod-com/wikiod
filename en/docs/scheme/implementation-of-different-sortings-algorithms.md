---
title: "Implementation of different sortings algorithms"
slug: "implementation-of-different-sortings-algorithms"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Quicksort
Quicksort is a common sorting algorithm with an average case complexity of `O(n log n)` and a worst case complexity of `O(n^2)`. Its advantage over other `O(n log n)` methods is that it can be executed in-place.

Quicksort splits the input on a chosen pivot value, separating the list into those values that are less than and those values that are greater than (or equal to) the pivot. Splitting the list is easily done with `filter`.

Using this, a Scheme implementation of Quicksort may look like the following:

    (define (quicksort lst)
      (cond
        ((or (null? lst) ; empty list is sorted
             (null? (cdr lst))) ; single-element list is sorted
         lst)
        (else
          (let ((pivot (car lst)) ; Select the first element as the pivot
                (rest (cdr lst)))
            (append
              (quicksort ; Recursively sort the list of smaller values
                (filter (lambda (x) (< x pivot)) rest)) ; Select the smaller values
              (list pivot) ; Add the pivot in the middle
              (quicksort ; Recursively sort the list of larger values
                (filter (lambda (x) (>= x pivot)) rest))))))) ; Select the larger and equal values


## Merge Sort
Merge Sort is a common sorting algorithm with an average case complexity of `O(n log n)` and a worst case complexity of `O(n log n)`. Although it cannot be executed in-place, it guarantees `O(n log n)` complexity in all cases.

Merge Sort repeatedly splits the input in two, until an empty list or single-element list is reached. Having reached the bottom of the splitting tree, it then works its way back up, merging the two sorted splits into each other, until a single sorted list is left.

Using this, a Scheme implementation of Merge Sort may look like the following:

    ;; Merge two sorted lists into a single sorted list
    (define (merge list1 list2)
      (cond
        ((null? list1)
         list2)
        ((null? list2)
         list1)
        (else
          (let ((head1 (car list1))
                (head2 (car list2)))
            ; Add the smaller element to the front of the merge list
            (if (<= head1 head2)
              (cons
                head1
                ; Recursively merge
                (merge (cdr list1) list2))
              (cons
                head2
                ; Recursively merge
                (merge list1 (cdr list2))))))))
    
    (define (split-list lst)
      (let ((half (quotient (length lst) 2)))
        ; Create a pair of the first and second halves of the list
        (cons
          (take lst half)
          (drop lst half))))
    
    (define (merge-sort lst)
      (cond
        ((or (null? lst) ; empty list is sorted, so merge up
             (null? (cdr lst))) ; single-element list is sorted, so merge up
         lst)
        (else
          (let ((halves (split-list lst)))
            ; Recursively split until the bottom, then merge back up to sort
            (merge (merge-sort (car halves))
                   (merge-sort (cdr halves)))))))

