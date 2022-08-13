---
title: "Control Structures"
slug: "control-structures"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Conditional Constructs
In Common Lisp, `if` is the simplest conditional construct. It has the form `(if test then [else])` and is evaluated to `then` if `test` is true and `else` otherwise. The else part can be omitted.

    (if (> 3 2)
        "Three is bigger!"
        "Two is bigger!")
    ;;=> "Three is bigger!"

One very important difference between `if` in Common Lisp and `if` in many other programming languages is that CL's `if` is an expression, not a statement. As such, `if` forms return values, which can be assigned to variables, used in argument lists, etc:

    ;; Use a different format string depending on the type of x
    (format t (if (numberp x)
                  "~x~%"
                  "~a~%")
               x)

Common Lisp's `if` can be considered equivalent to the [ternary operator ?:](https://www.wikiod.com/docs/c%23/18/operators/6029/ternary-operator) in C# and other "curly brace" languages.

For example, the following C# expression:

    year == 1990 ? "Hammertime" : "Not Hammertime"

Is equivalent to the following Common Lisp code, assuming that `year` holds an integer:

    (if (eql year 1990) "Hammertime" "Not Hammertime")

`cond` is another conditional construct. It is somewhat similar to a chain of `if` statements, and has the form:

    (cond (test-1 consequent-1-1 consequent-2-1 ...)
          (test-2)
          (test-3 consequent-3-1 ...)
          ... )

More precisely, `cond` has zero or more *clauses*, and each clause has one test followed by zero or more consequents. The entire `cond` construct selects the first clause whose test does not evaluate to `nil` and evaluates its consequents in order. It returns the value of the last form in the consequents.

    (cond ((> 3 4) "Three is bigger than four!")
          ((> 3 3) "Three is bigger than three!")
          ((> 3 2) "Three is bigger than two!")
          ((> 3 1) "Three is bigger than one!"))
    ;;=> "Three is bigger than two!"

To provide a default clause to evaluate if no other clause evaluates to `t`, you can add a clause that is true by default using `t`. This is very similar in concept to SQL's `CASE...ELSE`, but it uses a literal boolean true rather than a keyword to accomplish the task.

    (cond
        ((= n 1) "N equals 1")
        (t "N doesn't equal 1")
    )

An `if` construct can be written as a `cond` construct. `(if test then else)` and `(cond (test then) (t else))` are equivalent.

If you only need one clause, use `when` or `unless`:

    (when (> 3 4)
      "Three is bigger than four.")
    ;;=> NIL

    (when (< 2 5)
      "Two is smaller than five.")
    ;;=> "Two is smaller than five."

    (unless (> 3 4)
      "Three is bigger than four.")
    ;;=> "Three is bigger than four."

    (unless (< 2 5)
      "Two is smaller than five.")
    ;;=> NIL

## The do loop
Most looping and conditional constructs in Common Lisp are actually [macros][1] that hide away more basic constructs. For example, `dotimes` and `dolist` are built upon the `do` macro. The form for `do` looks like this:
   

    (do (varlist)
        (endlist)
       &body)

 - `varlist` is composed of the variables defined in the loop, their
   initial values, and how they change after each iteration. The 'change' portion is evaluated at the end of the loop.
 - `endlist` contains the end conditions and the values returned at the end of the loop. The end condition is evaluated at the beginning of the loop.

Here's one that starts at 0 and goes upto (not including) 10.

    ;;same as (dotimes (i 10)) 
    (do (( i (+ 1 i))
        ((< i 10) i)
       (print i))

And here's one that moves through a list:

    ;;same as (dolist (item given-list)
    (do ((item (car given-list))
         (temp list (cdr temp))
       (print item))

The `varlist` portion is similar the one in a `let` statement. You can bind more than one variable, and they only exist inside the loop. Each variable declared is in its own set of parenthesis. Here's one that counts how many 1's and 2's are in a list.

    (let ((vars (list 1 2 3 2 2 1)))
      (do ((ones 0)
           (twos 0)
           (temp vars (cdr temp)))
          ((not temp) (list ones twos))
        (when (= (car temp) 1)
          (setf ones (+ 1 ones)))
        (when (= (car temp) 2)
          (setf twos (+ 1 twos)))))
    -> (2 3)

And if a while loop macro hasn't been implemented:

    (do ()
        (t)
      (when task-done
        (break)))
For the most common applications, the more specific `dotimes` and `doloop` macros are much more succinct. 

  [1]: https://www.wikiod.com/docs/common-lisp/1257/macros#t=201609030332007042893

