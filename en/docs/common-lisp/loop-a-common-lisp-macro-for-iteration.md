---
title: "LOOP, a Common Lisp macro for iteration"
slug: "loop-a-common-lisp-macro-for-iteration"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Parallel Iteration
Multiple `FOR` clauses are allowed in a `LOOP`. The loop finishes when the first of these clauses finishes:

    (loop for a in '(1 2 3 4 5)
          for b in '(a b c)
          collect (list a b))
    ;; Evaluates to: ((1 a) (2 b) (3 c))

Other clauses that determine if the loop should continue can be combined:

    (loop for a in '(1 2 3 4 5 6 7)
          while (< a 4)
          collect a)
    ;; Evaluates to: (1 2 3)

    (loop for a in '(1 2 3 4 5 6 7)
          while (< a 4)
          repeat 1
          collect a)
    ;; Evaluates to: (1)

Determine which list is longer, cutting off iteration as soon as the answer is known:

    (defun longerp (list-1 list-2)
        (loop for cdr1 on list-1
              for cdr2 on list-2
              if (null cdr1) return nil
              else if (null cdr2) return t
              finally (return nil)))

Numbering the elements of a list:

    (loop for item in '(a b c d e f g)
          for x from 1
          collect (cons x item))
    ;; Returns ((1 . a) (2 . b) (3 . c) (4 . d) (5 . e) (6 . f) (7 . g))

Ensure that all the numbers in a list are even, but only for the first
100 items:

    (assert
       (loop for number in list
             repeat 100
             always (evenp number)))


## Bounded Loops
We can repeat an action some number of times using `repeat`.

```lisp
CL-USER> (loop repeat 10 do (format t "Hello!~%"))
Hello!
Hello!
Hello!
Hello!
Hello!
Hello!
Hello!
Hello!
Hello!
Hello!
NIL
CL-USER> (loop repeat 10 collect (random 50))
(28 46 44 31 5 33 43 35 37 4)
```

## Simple LOOP form
Simple LOOP form without special keywords:

```
(loop forms...)
```

To break out of the loop we can use `(return <return value>)` `

Some examples:

```
(loop (format t "Hello~%"))  ; prints "Hello" forever
(loop (print (eval (read)))) ; your very own REPL
(loop (let ((r (read)))
        (typecase r
         (number (return (print (* r r))))
         (otherwise (format t "Not a number!~%")))))
```

## Arithmetic Loops
```
(loop for i from 0 to 10
      do (print i)) ; prints 0 1 2 3 4 5 6 7 8 9 10
(loop for i from 0 below 10
      do (print i)) ; prints 0 1 2 3 4 5 6 7 8 9 10
(loop for i from 10 above 0
      do (print i)) ; prints 10 9 8 7 6 5 4 3 2 1
(loop for i from 10 to 0
      do (print i)) ; prints nothing
(loop for i from 10 downto 0
      do (print i)) ; prints 10 9 8 7 6 5 4 3 2 1 0
(loop for i downfrom 10 to 0
      do (print i)) ; same as above
(loop for i from 1 to 100 by 10
      do (print i)) ; prints 1 11 21 31 41 51 61 71 81 91
(loop for i from 100 downto 0 by 10
      do (print i)) ; prints 100 90 80 70 60 50 40 30 20 10 0
(loop for i from 1 to 10 by (1+ (random 3))
      do (print i)) ; note that (random 3) is evaluated only once
(let ((step (random 3)))
  (loop for i from 1 to 10 by (+ step 1)
        do (print i))) ; equivalent to the above
(loop for i from 1 to 10
      for j from 11 by 11
      do (format t "~2d ~3d~%" i j)) ;prints 1 11\n2 22\n...10 110
```

## Looping over Sequences
```
(loop for i in '(one two three four five six)
      do (print i))
(loop for i in '(one two three four five six) by #'cddr
      do (print i)) ;prints ONE THREE FIVE

(loop for i on '(a b c d e f g)
      do (print (length i))) ;prints 7 6 5 4 3 2 1
(loop for i on '(a b c d e f g) by #'cddr
      do (print (length i))) ;prints 7 5 3 1
(loop for i on '(a b c)
      do (print i)) ;prints (a b c) (b c) (c)

(loop for i across #(1 2 3 4 5 6)
      do (print i)) ; prints 1 2 3 4 5 6
(loop for i across "foo"
      do (print i)) ; prints #\f #\o #\o
(loop for element across "foo"
      for i from 0
      do (format t "~a ~a~%" i element)) ; prints 0 f\n1 o\n1 o
```
Here is a summary of the keywords

| Keyword | Sequence type | Variable type |
| ------ | ------ | ----- |
| in   | list   | element of list
| on | list | some cdr of list
| across | vector | element of vector

## Looping over Hash Tables
```
(defvar *ht* (make-hash-table))
(loop for (sym num) on 
        '(one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9 ten 10)
        by #'cddr
      do (setf (gethash sym *ht*) num))

(loop for k being each hash-key of *ht*
      do (print k)) ; iterate over the keys
(loop for k being the hash-keys in *ht* using (hash-value v)
      do (format t "~a=>~a~%" k v))
(loop for v being the hash-value in *ht*
      do (print v))
(loop for v being each hash-values of *ht* using (hash-key k)
      do (format t "~a=>~a~%" k v))
```

## Looping over Packages
```
(loop for s being the symbols in 'cl
      do (print s))
(loop for s being the present-symbols in :cl
      do (print s))
(loop for s being the external-symbols in (find-package "COMMON LISP")
      do (print s))
(loop for s being each external-symbols of "COMMON LISP"
      do (print s))
(loop for s being each external-symbol in pack ;pack is a variable containing a package
      do (print s))
```

## Destructuring in FOR statements
We can destructure lists of compound objects

    CL-USER> (loop for (a . b) in '((1 . 2) (3 . 4) (5 . 6)) collect a)
    (1 3 5)
    CL-USER> (loop for (a . b) in '((1 . 2) (3 . 4) (5 . 6)) collect b)
    (2 4 6)
    CL-USER> (loop for (a b c) in '((1 2 3) (4 5 6) (7 8 9) (10 11 12)) collect b)
    (2 5 8 11)

We can also destructure a list itself

    CL-USER> (loop for (a . b) on '(1 2 3 4 5 6) collect a)
    (1 2 3 4 5 6)
    CL-USER> (loop for (a . b) on '(1 2 3 4 5 6) collect b)
    ((2 3 4 5 6) (3 4 5 6) (4 5 6) (5 6) (6) NIL)

This is useful when we want to iterate through only certain elements

    CL-USER> (loop for (a . b) on '(1 2 3 4 5 6) by #'cddr collect a)
    (1 3 5)
    CL-USER> (loop for (a . b) on '(1 2 3 4 5 6) by #'cdddr collect a)
    (1 4)

Using `NIL` to ignore a term:
```
(loop for (a nil . b) in '((1 2 . 3) (4 5 . 6) (7 8 . 9))
      collect (list a b)) ;=> ((1 3) (4 6) (7 9))
(loop for (a b) in '((1 2) (3 4) (5 6)) ;(a b) == (a b . nil)
      collect (+ a b)) ;=> (3 7 11)

; iterating over a window in a list
(loop for (pre x post) on '(1 2 3 4 5 3 2 1 2 3 4)
      for nth from 1
      while (and x post) ; checks that we have three elements of the list
      if (and (<= post x) (<= pre x)) collect (list :max x nth)
      if (and (>= post x) (>= pre x)) collect (list :min x nth))
; The above collects local minima/maxima
```

## LOOP as an Expression
Unlike the loops in nearly every other programming language in use today, the `LOOP` in Common Lisp can be used as an expression:

    (let ((doubled (loop for x from 1 to 10
                         collect (* 2 x))))
        doubled) ;; ==> (2 4 6 8 10 12 14 16 18 20)

    (loop for x from 1 to 10 sum x)

`MAXIMIZE` causes the `LOOP` to return the largest value
that was evaluated. `MINIMIZE` is the opposite of
`MAXIMIZE`.

    (loop repeat 100
          for x = (random 1000)
          maximize x)

`COUNT` tells you how many times an expression evaluated to non-`NIL` during the loop:

    (loop repeat 100
          for x = (random 1000)
          count (evenp x))

`LOOP` also has equivalents of the `some`, `every`, and `notany` functions:

    (loop for ch across "foobar"
         thereis (eq ch #\a))

    (loop for x in '(a b c d e f 1)
        always (symbolp x))

    (loop for x in '(1 3 5 7)
        never (evenp x))

...except they're not limited to iterating over sequences:

    (loop for value = (read *standard-input* nil :eof)
       until (eq value :eof)
       never (stringp value))

`LOOP` value-generating verbs can also be written
with an -ing suffix:

    (loop repeat 100
          for x = (random 1000)
          minimizing x)

It is also possible to capture the value generated by these verbs into variables (which are created implicitly by the `LOOP` macro),
so you can generate more than one value at a time:

     (loop repeat 100
         for x = (random 1000)
         maximizing x into biggest
         minimizing x into smallest
         summing x into total
         collecting x into xs
         finally (return (values biggest smallest total xs)))

You can have more than one `collect`, `count`, etc. clause that collects
into the same output value. They will be executed in sequence.

The following converts an association list (which you can use with `assoc`) into a property list (which you can use with `getf`):

    (loop for (key . value) in assoc-list
          collect key
          collect value)

Although this is better style:

    (loop for (key . value) in assoc-list
          append (list key value))


## Conditionally executing LOOP clauses
`LOOP` has its own `IF` statement that can control how the clauses are
executed:

    (loop repeat 1000
          for x = (random 100)
          if (evenp x)
            collect x into evens
          else
            collect x into odds
          finally (return (values evens odds)))

Combining multiple clauses in an IF body requires special syntax:

     (loop repeat 1000
           for x = (random 100)
           if (evenp x)
              collect x into evens
              and do (format t "~a is even!~%" x)
           else
              collect x into odds
              and count t into n-odds
           finally (return (values evens odds n-odds)))


## Nested Iteration
The special `LOOP NAMED foo` syntax allows you to create a loop that you can exit early from. The exit is performed using `return-from`, and can be used from within nested loops. 

The following uses a nested loop to look for a complex number in a 2D array:

    (loop named top
          for x from 0 below (array-dimension *array* 1)
          do (loop for y from 0 below (array-dimension *array* 0))
                   for n = (aref *array* y x)
                 when (complexp n)
                   do (return-from top (values n x y))))


## RETURN clause versus RETURN form.
Within a `LOOP`, you can use the Common Lisp `(return)` form in any expression, which will cause the `LOOP` form to immediately evaluate to the value given to `return`.

`LOOP` also has a `return` clause which works almost identically, the only difference being that you don't surround it with parentheses. The clause is used within `LOOP`'s DSL, while the form is used within expressions.

    (loop for x in list
          do (if (listp x) ;; Non-barewords after DO are expressions
                 (return :x-has-a-list)))

    ;; Here, both the IF and the RETURN are clauses
    (loop for x in list
         if (listp x) return :x-has-a-list)

    ;; Evaluate the RETURN expression and assign it to X...
    ;; except RETURN jumps out of the loop before the assignment
    ;; happens.
    (loop for x = (return :nothing-else-happens)
          do (print :this-doesnt-print))

The thing after `finally` must be an expression, so the `(return)` form must be used and not the `return` clause:

     (loop for n from 1 to 100
           when (evenp n) collect n into evens
           else collect n into odds
          finally return (values evens odds)) ;; ERROR!

     (loop for n from 1 to 100
           when (evenp n) collect n into evens
           else collect n into odds
          finally (return (values evens odds))) ;; Correct usage.


## Looping over a window of a list
Some examples for a window of size 3:
```
;; Naïve attempt:
(loop for (first second third) on '(1 2 3 4 5)
      do (print (* first second third)))
;; prints 6 24 60 then Errors on (* 4 5 NIL)

;; We will try again and put our attempt into a function
(defun loop-3-window1 (function list)
  (loop for (first second third) on list
        while (and second third)
        do (funcall function first second third)))
(loop-3-window1 (lambda (a b c) (print (* a b c))) '(1 2 3 4 5))
;; prints 6 24 60 and returns NIL
(loop-3-window1 (lambda (a b c) (print (list a b c))) '(a b c d nil nil e f))
;; prints (a b c) (b c d) then returns NIL

;; A second attempt
(defun loop-3-window2 (function list)
  (loop for x on list
        while (nthcdr 2 x) ;checks if there are at least 3 elements
        for (first second third) = x
        do (funcall function first second third)))
(loop-3-window2 (lambda (a b c) (print (list a b c))) '(a b c d nil nil e f))
;; prints (a b c) (b c d) (c d nil) (c nil nil) (nil nil e) (nil e f)

;; A (possibly) more efficient function:
(defun loop-3-window2 (function list)
  (let ((f0 (pop list))
        (s0 (pop list)))
    (loop for first = f0 then second
          and second = s0 then third
          and third in list
          do (funcall function first second third))))

;; A more general function:
(defun loop-n-window (n function list)
  (loop for x on list
        while (nthcdr (1- n) x)
        do (apply function (subseq x 0 n))))
;; With potentially efficient implementation:
(define-compiler-macro loop-n-window (n function list &whole w)
  (if (typep n '(integer 1 #.call-arguments-limit))
     (let ((vars (loop repeat n collect (gensym)))
           (vars0 (loop repeat (1- n) collect (gensym)))
           (lst (gensym)))
       `(let ((,lst ,list))
          (let ,(loop for v in vars0 collect `(,v (pop ,lst)))
            (loop for
                  ,@(loop for v0 in vars0 for (v vn) on vars
                     collect v collect '= collect v0 collect 'then collect vn
                     collect 'and)
                  ,(car (last vars)) in ,lst
                  do ,(if (and (consp function) (eq 'function (car function))
     w
```

