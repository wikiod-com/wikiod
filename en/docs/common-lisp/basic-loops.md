---
title: "Basic loops"
slug: "basic-loops"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
- (do ({var | (var [init-form [step-form]])}\*) (end-test-form result-form\*) declaration\* {tag | statement}\*)
- (do\* ({var | (var [init-form [step-form]])}\*) (end-test-form result-form\*) declaration\* {tag | statement}\*)
- (dolist (var list-form [result-form]) declaration\* {tag | statement}\*)
- (dotimes (var count-form [result-form]) declaration\* {tag | statement}\*)

## dotimes
`dotimes` is a macro for integer iteration over a single variable from 0 below some parameter value. One of the simples examples would be:

    CL-USER> (dotimes (i 5)
               (print i))
    
    0 
    1 
    2 
    3 
    4 
    NIL

Note that `NIL` is the returned value, since we did not provide one ourselves; the variable starts from 0 and throughout the loop becomes values from 0 to N-1. After the loop, the variable becomes the N:

    CL-USER> (dotimes (i 5 i))
    5

    CL-USER> (defun 0-to-n (n)
               (let ((list ()))
                 (dotimes (i n (nreverse list))
                   (push i list))))
    0-TO-N
    CL-USER> (0-to-n 5)
    (0 1 2 3 4)

## dolist
`dolist` is a looping macro created to easily loop through the lists. One of the simplest uses would be:

    CL-USER> (dolist (item '(a b c d))
               (print item))
    
    A 
    B 
    C 
    D 
    NIL ; returned value is NIL

Note that since we did not provide return value, `NIL` is returned (and A,B,C,D are printed to `*standard-output*`).

`dolist` can also return values:

    ;;This may not be the most readable summing function.
    (defun sum-list (list)
      (let ((sum 0))
        (dolist (var list sum)
          (incf sum var))))
    
    CL-USER> (sum-list (list 2 3 4))
    9

## Simple loop
The [**loop**][1] macro has two forms: the "simple" form and the "extended" form.  The extended form is covered in another documentation topic, but the simple loop is useful for very basic loop.

The simple **loop** form takes a number of forms and repeats them until the loop is exited using **return** or some other exit (e.g., **throw**).

    (let ((x 0))
      (loop
         (print x)
         (incf x)
         (unless (< x 5)
           (return))))
    
    0 
    1 
    2 
    3 
    4 
    NIL


  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm

