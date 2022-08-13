---
title: "Recursion"
slug: "recursion"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Lisp is often used in educational contexts, where students learn to understand and implement recursive algorithms.

Production code written in Common Lisp or portable code has several issues with recursion: They do not make use of implementation-specific features like *tail call optimization*, often making it necessary to avoid recursion altogether. In these cases, implementations:

* Usually have a *recursion depth limit* due to limits in stack sizes. Thus recursive algorithms will only work for data of limited size.
* Do not always provide optimization of tail calls, especially in combination with dynamically scoped operations.
* Only provide optimization of tail calls at certain optimization levels.
* Do not usually provide *tail call optimization*.
* Usually do not provide *tail call optimization* on certain platforms. For example, implementations on JVM may not do so, since the JVM itself does not support *tail call optimization*.

Replacing tail calls with jumps usually makes debugging more difficult; Adding jumps will cause stack frames to become unavailable in a debugger. As alternatives Common Lisp provides:

* Iteration constructs, like `DO`, `DOTIMES`, `LOOP`, and others
* Higher-order functions, like `MAP`, `REDUCE`, and others
* Various control structures, including low-level `go to`



## Recursion template 1 single condition  single tail recursion
    (defun fn (x)
      (cond (test-condition the-value)
            (t (fn reduced-argument-x))))


## Recursively print the elements of a list
    ;;Recursively print the elements of a list
    (defun print-list (elements)
        (cond
            ((null elements) '()) ;; Base case: There are no elements that have yet to be printed. Don't do anything and return a null list.
            (t
                ;; Recursive case
                ;; Print the next element.
                (write-line (write-to-string (car elements)))
                ;; Recurse on the rest of the list.
                (print-list (cdr elements))
            )
        )
    )

To test this, run:

    (setq test-list '(1 2 3 4))
    (print-list test-list)

The result will be:

    1
    2
    3
    4



## Compute the factorial of a whole number
One easy algorithm to implement as a recursive function is factorial.

    ;;Compute the factorial for any n >= 0. Precondition: n >= 0, n is an integer.
    (defun factorial (n)
        (cond
            ((= n 0) 1) ;; Special case, 0! = 1
            ((= n 1) 1) ;; Base case, 1! = 1
            (t
                ;; Recursive case
                ;; Multiply n by the factorial of n - 1.
                (* n (factorial (- n 1)))
            )
        )
    )

## Recursion template 2 multi-condition
     (defun fn (x)
          (cond (test-condition1 the-value1)
                (test-condition2 the-value2)
                ...
                ...
                ...
                (t (fn reduced-argument-x))))
   

     CL-USER 2788 > (defun my-fib (n)
                     (cond ((= n 1) 1)
                           ((= n 2) 1)
                           (t (+
                               (my-fib (- n 1))
                               (my-fib (- n 2))))))
    MY-FIB
    
    CL-USER 2789 > (my-fib 1)
    1
    
    CL-USER 2790 > (my-fib 2)
    1
    
    CL-USER 2791 > (my-fib 3)
    2
    
    CL-USER 2792 > (my-fib 4)
    3
    
    CL-USER 2793 > (my-fib 5)
    5
    
    CL-USER 2794 > (my-fib 6)
    8
    
    CL-USER 2795 > (my-fib 7)
    13

## Compute nth Fibonacci number
    ;;Find the nth Fibonacci number for any n > 0.
    ;; Precondition: n > 0, n is an integer. Behavior undefined otherwise.
    (defun fibonacci (n)
        (cond
            (                                     ;; Base case.
                 ;; The first two Fibonacci numbers (indices 1 and 2) are 1 by definition.
                (<= n 2)                          ;; If n <= 2
                1                                 ;; then return 1.
            )
            (t                                    ;; else
                (+                                ;; return the sum of
                                                  ;; the results of calling 
                    (fibonacci (- n 1))           ;; fibonacci(n-1) and
                    (fibonacci (- n 2))           ;; fibonacci(n-2).
                                                  ;; This is the recursive case.
                )
            )
        )
    )

