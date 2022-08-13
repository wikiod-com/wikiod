---
title: "Lexical vs special variables"
slug: "lexical-vs-special-variables"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Global special variables are special everywhere
Thus these variables will use dynamic binding.

    (defparameter count 0)
    ;; All uses of count will refer to this one 
    
    (defun handle-number (number)
      (incf count)
      (format t "~&~d~%" number))
      
    (dotimes (count 4)
      ;; count is shadowed, but still special
      (handle-number count))
      
    (format t "~&Calls: ~d~%" count)
    ==>
    0
    2
    Calls: 0

Give special variables distinct names to avoid this problem:

    (defparameter *count* 0)
    
    (defun handle-number (number)
      (incf *count*)
      (format t "~&~d~%" number))
      
    (dotimes (count 4)
      (handle-number count))
      
    (format t "~&Calls: ~d~%" *count*)
    ==>
    0
    1
    2
    3
    Calls: 4

Note 1: it is not possible to make a global variable non-special in a certain scope. There is no declaration to make a variable *lexical*.

Note 2: it is possible to declare a variable *special* in a local context using the `special` declaration. If there is no global special declaration for that variable, the declaration is only locally and can be shadowed.

    (defun bar ()
      (declare (special a))
      a)                       ; value of A is looked up from the dynamic binding
    
    (defun foo ()
      (let ((a 42))            ; <- this variable A is special and
                               ;    dynamically bound
        (declare (special a))
        (list (bar)
              (let ((a 0))     ; <- this variable A is lexical
                (bar)))))

    
    > (foo)
    (42 42)




