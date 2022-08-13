---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Anonymous functions can be created using [`LAMBDA`](http://www.lispworks.com/documentation/HyperSpec/Body/m_lambda.htm). Local functions can be defined using [`LABELS` or `FLET`](http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm). Their parameters are defined the same was as in global named functions.

## Required Parameters
    (defun foobar (x y)
      (format t "X: ~s~@
                 Y: ~s~%"
              x y))
    
    (foobar 10 20)
    ; X: 10
    ; Y: 20
    ;=> NIL

## Optional Parameters
Optional parameters can be specified after required parameters, by using `&OPTIONAL` keyword. There may be multiple optional parameters after it.

    (defun foobar (x y &optional z)
      (format t "X (~s) and Y (~s) are required.~@
                 Z (~s) is optional.~%"
              x y z))
    
    (foobar 10 20)
    ; X (10) and Y (20) are required.
    ; Z (NIL) is optional.
    ;=> NIL
    (foobar 10 20 30)
    ; X (10) and Y (20) are required.
    ; Z (30) is optional.
    ;=> NIL

# Default alue

A default value can be given for optional parameters by specifying the parameter with a list; the second value is the default. The default value form will only be evaluated if the argument was given, so it can be used for side-effects, such as signalling an error.

    (defun foobar (x y &optional (z "Default"))
      (format t "X (~s) and Y (~s) are required.~@
                 Z (~s) is optional.~%"
              x y z))
    
    (foobar 10 20)
    ; X (10) and Y (20) are required.
    ; Z ("Default") is optional.
    ;=> NIL
    (foobar 10 20 30)
    ; X (10) and Y (20) are required.
    ; Z (30) is optional.
    ;=> NIL

# Check if optional argument was given

A third member can be added to the list after the default value; a variable name that is true if the argument was given, or `NIL` if it wasn't given (and the default is used).

    (defun foobar (x y &optional (z "Default" zp))
      (format t "X (~s) and Y (~s) are required.~@
                 Z (~s) is optional. It ~:[wasn't~;was~] given.~%"
              x y z zp))
    
    (foobar 10 20)
    ; X (10) and Y (20) are required.
    ; Z ("Default") is optional. It wasn't given.
    ;=> NIL
    (foobar 10 20 30)
    ; X (10) and Y (20) are required.
    ; Z (30) is optional. It was given.
    ;=> NIL

## Function without Parameters
Global named functions are defined with [`DEFUN`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm).

    (defun foobar ()
      "Optional documentation string. Can contain line breaks.
    
    Must be at the beginning of the function body. Some will format the
    docstring so that lines are indented to match the first line, although
    the built-in DESCRIBE-function will print it badly indented that way.

    Ensure no line starts with an opening parenthesis by escaping them
    \(like this), otherwise your editor may have problems identifying
    toplevel forms."
      (format t "No parameters.~%"))

    (foobar)
    ; No parameters.
    ;=> NIL

    (describe #'foobar) ; The output is implementation dependant.
    ; #<FUNCTION FOOBAR>
    ;   [compiled function]
    ;
    ; Lambda-list: ()
    ; Derived type: (FUNCTION NIL (VALUES NULL &OPTIONAL))
    ; Documentation:
    ;   Optional documentation string. Can contain line breaks.
    ;   
    ;   Must be at the beginning of the function body. Some will format the
    ;   docstring so that lines are indented to match the first line, although
    ;   the built-in DESCRIBE-function will print it badly indented that way.
    ; Source file: /tmp/fileInaZ1P
    ;=> No values

The function body may contain any number of forms. The values from the last form will be returned from the function.

## Rest Parameter
A single rest-parameter can be given with the keyword `&REST` after the required arguments. If such a parameter exists, the function can take a number of arguments, which will be grouped into a list in the rest-parameter. Note that the variable `CALL-ARGUMENTS-LIMIT` determines the maximum number of arguments which can be used in a function call, thus the number of arguments is limited to an implementation specific value of minimum 50 or more arguments.

    (defun foobar (x y &rest rest)
      (format t "X (~s) and Y (~s) are required.~@
                 The function was also given following arguments: ~s~%"
              x y rest))
    
    (foobar 10 20)
    ; X (10) and Y (20) are required.
    ; The function was also given following arguments: NIL
    ;=> NIL
    (foobar 10 20 30 40 50 60 70 80)
    ; X (10) and Y (20) are required.
    ; The function was also given following arguments: (30 40 50 60 70 80)
    ;=> NIL

# Rest and Keyword Parameters together

The rest-parameter may be before keyword parameters. In that case it will contain the property list given by the user. The keyword values will still be bound to the corresponding keyword parameter.

    (defun foobar (x y &rest rest &key (z 10 zp))
      (format t "X (~s) and Y (~s) are required.~@
                 Z (~s) is a keyword argument. It ~:[wasn't~;was~] given.~@
                 The function was also given following arguments: ~s~%"
              x y z zp rest))
    
    (foobar 10 20)
    ; X (10) and Y (20) are required.
    ; Z (10) is a keyword argument. It wasn't given.
    ; The function was also given following arguments: NIL
    ;=> NIL
    (foobar 10 20 :z 30)
    ; X (10) and Y (20) are required.
    ; Z (30) is a keyword argument. It was given.
    ; The function was also given following arguments: (:Z 30)
    ;=> NIL

Keyword `&ALLOW-OTHER-KEYS` can be added at the end of the lambda-list to allow the user to give keyword arguments not defined as parameters. They will go in the rest-list.

    (defun foobar (x y &rest rest &key (z 10 zp) &allow-other-keys)
      (format t "X (~s) and Y (~s) are required.~@
                 Z (~s) is a keyword argument. It ~:[wasn't~;was~] given.~@
                 The function was also given following arguments: ~s~%"
              x y z zp rest))
    
    (foobar 10 20 :z 30 :q 40)
    ; X (10) and Y (20) are required.
    ; Z (30) is a keyword argument. It was given.
    ; The function was also given following arguments: (:Z 30 :Q 40)
    ;=> NIL

## Auxiliary Variables
The `&AUX` keyword can be used to define local variables for the function. They are not parameters; the user cannot supply them. 

`&AUX` variables are seldomly used. You can always use `LET` instead, or some other way of defining local variables in the function body.

`&AUX` variables have the advantages that local variables of the whole function body move to the top and it makes one indentation level (for example introduced by a LET) unnecessary.

    (defun foobar (x y &aux (z (+ x y)))
      (format t "X (~d) and Y (~d) are required.~@
                 Their sum is ~d."
              x y z))
    
    (foobar 10 20)
    ; X (10) and Y (20) are required.
    ; Their sum is 30.
    ;=> NIL

One typical usage may be resolving "designator" parameters. Again, you need not do it this way; using `let` is just as idiomatic.

    (defun foo (a b &aux (as (string a)))
      "Combines A and B in a funny way.  A is a string designator, B a string."
      (concatenate 'string as " is funnier than " b))

## RETURN-FROM, exit from a block or a function
Functions always establish a block around the body. This block has the same name as the function name. This means you can use `RETURN-FROM` with this block name to return from the function and return values.

You should avoid returning early whenever possible.

    (defun foobar (x y)
      (when (oddp x)
        (format t "X (~d) is odd. Returning immediately.~%" x)
        (return-from foobar "return value"))
      (format t "X: ~s~@
                 Y: ~s~%"
              x y))
    
    (foobar 10 20)
    ; X: 10
    ; Y: 20
    ;=> NIL
    (foobar 9 20)
    ; X (9) is odd. Returning immediately.
    ;=> "return value"

## Keyword Parameters
Keyword parameters can be defined with the `&KEY` keyword. They are always optional (see the Optional Parameters example for details of the definition). There may be multiple keyword parameters.

    (defun foobar (x y &key (z "Default" zp))
      (format t "X (~s) and Y (~s) are required.~@
                 Z (~s) is a keyword argument. It ~:[wasn't~;was~] given.~%"
              x y z zp))
    
    (foobar 10 20)
    ; X (10) and Y (20) are required.
    ; Z ("Default") is a keyword argument. It wasn't given.
    ;=> NIL
    (foobar 10 20 :z 30)
    ; X (10) and Y (20) are required.
    ; Z (30) is a keyword argument. It was given.
    ;=> NIL

