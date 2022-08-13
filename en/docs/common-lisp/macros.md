---
title: "macros"
slug: "macros"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

# The Purpose of Macros

Macros are intended for generating code, transforming code and providing new notations. These new notations can be more suited to better express the program, for example by providing domain-level constructs or entire new embedded languages.

Macros can make source code more self-explanatory, but debugging can be made more difficult. As a rule of thumb, one should not use macros when a regular function will do. When you do use them, avoid the usual pitfalls, try to stick to the commonly used patterns and naming conventions.

# Macroexpansion Order

Compared to functions, macros are expanded in a reverse order; outmost first, inmost last. This means that by default one cannot use an inner macro to generate syntax required for an outer macro.

# Evaluation Order

Sometimes macros need to move user-supplied forms around. One must make sure not to change the order in which they are evaluated. The user may be relying on side effects happening in order.

# Evaluate Once Only

The expansion of a macro often needs to use the value of the same user-supplied form more than once. It is possible that the form happens to have side-effects, or it might be calling an expensive function. Thus the macro must make sure to only evaluate such forms once. Usually this will be done by assigning the value to a local variable (whose name is `GENSYM`ed).

# Functions used by Macros, using EVAL-WHEN

Complex macros often have parts of their logic implemented in separate functions. One must remember, however, that macros are expanded before the actual code is compiled. When compiling a file, then by default, functions and variables defined in the same file will not be available during macro execution. All function and variable definitions, in the same file, used by a macro must be wrapped inside an [`EVAL-WHEN`](http://www.lispworks.com/documentation/HyperSpec/Body/s_eval_w.htm)-form. The `EVAL-WHEN` should have all three times specified, when the enclosed code also should be evaluated during load and runtime.

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (defun foobar () ...))

This does not apply to functions called from the expansion of the macro, only the ones called by the macro itself.

## Anaphoric Macros
An [Anaphoric Macro](https://en.wikipedia.org/wiki/Anaphoric_macro) is a macro that introduces a variable (often `IT`) that captures the result of a user-supplied form. A common example is the Anaphoric If, which is like a regular `IF`, but also defines the variable `IT` to refer to the result of the test-form.

    (defmacro aif (test-form then-form &optional else-form)
      `(let ((it ,test-form))
         (if it ,then-form ,else-form)))

    (defun test (property plist)
      (aif (getf plist property)
           (format t "The value of ~s is ~a.~%" property it)
           (format t "~s wasn't in ~s!~%" property plist)))

    (test :a '(:a 10 :b 20 :c 30))
    ; The value of :A is 10.
    (test :d '(:a 10 :b 20 :c 30))
    ; :D wasn't in (:A 10 :B 20 :C 30)!

## Backquote - writing code templates for macros
Macros return code. Since code in Lisp consists of lists, one can use the regular list manipulation functions to generate it.

    ;; A pointless macro
    (defmacro echo (form)
      (list 'progn
            (list 'format t "Form: ~a~%" (list 'quote form))
            form))
    
This is often very hard to read, especially in longer macros. The [Backquote](http://www.lispworks.com/documentation/HyperSpec/Body/02_df.htm) reader macro allows one to write quoted templates that are filled in by selectively evaluating elements. 

    (defmacro echo (form)
      `(progn
         (format t "Form: ~a~%" ',form)
         ,form))

    (macroexpand '(echo (+ 3 4)))
    ;=> (PROGN (FORMAT T "Form: ~a~%" '(+ 3 4)) (+ 3 4))

This version looks almost like regular code. The commas are used to evaluate `FORM`; everything else is returned as is. Notice that in `',form` the single quote is outside the comma, so it will be returned.

One can also use `,@` to splice a list in the position.

    (defmacro echo (&rest forms)
      `(progn
         ,@(loop for form in forms collect `(format t "Form: ~a~%" ,form))
         ,@forms))

    (macroexpand '(echo (+ 3 4) 
                        (print "foo")
                        (random 10)))
    ;=> (PROGN
    ;    (FORMAT T "Form: ~a~%" (+ 3 4))
    ;    (FORMAT T "Form: ~a~%" (PRINT "foo"))
    ;    (FORMAT T "Form: ~a~%" (RANDOM 10))
    ;    (+ 3 4)
    ;    (PRINT "foo")
    ;    (RANDOM 10))

Backquote can be used outside macros too.

## Common Macro Patterns
**TODO: Maybe move the explanations to remarks and add examples separately**

# FOOF

In Common Lisp, there is a concept of [Generalized References](http://www.lispworks.com/documentation/HyperSpec/Body/05_a.htm). They allow a programmer to setf values to various "places" as if they were variables. Macros that make use of this ability often have a `F`-postfix in the name. The place is usually the first argument to the macro.

Examples from the standard: [`INCF`, `DECF`](http://www.lispworks.com/documentation/HyperSpec/Body/m_incf_.htm#decf), [`ROTATEF`](http://www.lispworks.com/documentation/HyperSpec/Body/m_rotate.htm#rotatef), [`SHIFTF`](http://www.lispworks.com/documentation/HyperSpec/Body/m_shiftf.htm#shiftf), [`REMF`](http://www.lispworks.com/documentation/HyperSpec/Body/m_remf.htm#remf).

A silly example, a macro that flips the sign of a number store in a place:

    (defmacro flipf (place)
      `(setf ,place (- ,place)))

# WITH-FOO

Macros that acquire and safely release a resource are usually named with a `WITH-`-prefix. The macro should usually use syntax like:

    (with-foo (variable details-of-the-foo...)
      body...)

Examples from the standard: [`WITH-OPEN-FILE`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_open.htm), [`WITH-OPEN-STREAM`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_op_1.htm), [`WITH-INPUT-FROM-STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_in_f.htm), [`WITH-OUTPUT-TO-STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_out_.htm).

One approach to implementing this type of macro that can avoid some of the pitfalls of name pollution and unintended multiple evaluation is by implementing a functional version first.  For instance, the first step in implementing a `with-widget` macro that safely creates a widget and cleans up afterward might be a function:

    (defun call-with-widget (args function)
      (let ((widget (apply #'make-widget args))) ; obtain WIDGET
        (unwind-protect (funcall function widget)  ; call FUNCTION with WIDGET
          (cleanup widget)                         ; cleanup

Because this is a function, there are no concerns about the scope of names within **function** or **supplier**, and it makes it easy to write a corresponding macro:

    (defmacro with-widget ((var &rest args) &body body)
      `(call-with-widget (list ,@args) (lambda (,var) ,@body)))

# DO-FOO

Macros that iterate over something are often named with a `DO`-prefix. The macro syntax should usually be in form

    (do-foo (variable the-foo-being-done return-value)
      body...)

Examples from the standard: [`DOTIMES`](http://www.lispworks.com/documentation/HyperSpec/Body/m_dotime.htm), [`DOLIST`](http://www.lispworks.com/documentation/HyperSpec/Body/m_dolist.htm), [`DO-SYMBOLS`](http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm).

# FOOCASE, EFOOCASE, CFOOCASE

Macros that match an input against certain cases are often named with a `CASE`-postfix. There is often a `E...CASE`-variant, which signals an error if the input doesn't match any of the cases, and `C...CASE`, which signals a continuable error. They should have syntax like

    (foocase input
      (case-to-match-against (optionally-some-params-for-the-case)
       case-body-forms...)
      more-cases...
      [(otherwise otherwise-body)])

Examples from the standard: [`CASE`](http://www.lispworks.com/documentation/HyperSpec/Body/m_case_.htm), [`TYPECASE`](http://www.lispworks.com/documentation/HyperSpec/Body/m_tpcase.htm), [`HANDLER-CASE`](http://www.lispworks.com/documentation/HyperSpec/Body/m_hand_1.htm).

For example, a macro that matches a string against regular expressions and binds the register groups to variables. Uses [CL-PPCRE](http://weitz.de/cl-ppcre/) for regular expressions.

    (defmacro regexcase (input &body cases)
      (let ((block-sym (gensym "block"))
            (input-sym (gensym "input")))
        `(let ((,input-sym ,input))
           (block ,block-sym
             ,@(loop for (regex vars . body) in cases
                     if (eql regex 'otherwise)
                       collect `(return-from ,block-sym (progn ,vars ,@body))
                     else
                       collect `(cl-ppcre:register-groups-bind ,vars
                                    (,regex ,input-sym)
                                  (return-from ,block-sym
                                    (progn ,@body))))))))

    (defun test (input)
      (regexcase input
        ("(\\d+)-(\\d+)" (foo bar)
          (format t "Foo: ~a, Bar: ~a~%" foo bar))
        ("Foo: (\\w+)$" (foo)
          (format t "Foo: ~a.~%" foo))
        (otherwise (format t "Didn't match.~%"))))

    (test "asd 23-234 qwe")
    ; Foo: 23, Bar: 234
    (test "Foo: Foobar")
    ; Foo: Foobar.
    (test "Foo: 43 - 23")
    ; Didn't match.

# DEFINE-FOO, DEFFOO

Macros that define things are usually named either with `DEFINE-` or `DEF` -prefix.

Examples from the standard: [`DEFUN`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm), [`DEFMACRO`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm), [`DEFINE-CONDITION`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_5.htm).

## MACROEXPAND
Macro expansion is the process of turning macros into actual code. This usually happens as part of the compilation process. The compiler will expand all macro forms before actually compiling code. Macro expansion also happens during *interpretation* of Lisp code.

One can call [`MACROEXPAND`](http://www.lispworks.com/documentation/HyperSpec/Body/f_mexp_.htm) manually to see what a macro form expands to.

    CL-USER> (macroexpand '(with-open-file (file "foo")
                            (do-something-with file)))
    (LET ((FILE (OPEN "foo")) (#:G725 T))
      (UNWIND-PROTECT
          (MULTIPLE-VALUE-PROG1 (PROGN (DO-SOMETHING-WITH FILE)) (SETQ #:G725 NIL))
        (WHEN FILE (CLOSE FILE :ABORT #:G725))))

`MACROEXPAND-1` is the same, but only expands once. This s useful when trying to make sense of a macro form that expands to another macro form.

    CL-USER> (macroexpand-1 '(with-open-file (file "foo")
                              (do-something-with file)))
    (WITH-OPEN-STREAM (FILE (OPEN "foo")) (DO-SOMETHING-WITH FILE))

Note that neither `MACROEXPAND` nor `MACROEXPAND-1` expand the Lisp code on all levels. They only expand the top-level macro form. To macroexpand a form fully on all levels, one needs a *code walker* to do so. This facility is not provided in the Common Lisp standard.


## if-let, when-let, <foo>-let macros
These macros merge control flow and binding. They are an improvement over anaphoric anaphoric macros because they let the developer communicate meaning through naming. As such their use is recommended over their anaphoric counterparts.

    (if-let (user (get-user user-id))
      (show-dashboard user)
      (redirect 'login-page))

`FOO-LET` macros bind one or more variables, and then use those variables as the test form for the corresponding conditional (`IF`, `WHEN`). Multiple variables are combined with `AND`. The chosen branch is executed with the bindings in effect. A simple one variable implementation  of `IF-LET` might look something like:

    (defmacro if-let ((var test-form) then-form &optional else-form)
      `(let ((,var ,test-form))
         (if ,var ,then-form ,else-form)))

    (macroexpand '(if-let (a (getf '(:a 10 :b 20 :c 30) :a))
                   (format t "A: ~a~%" a)
                   (format t "Not found.~%")))
    ; (LET ((A (GETF '(:A 10 :B 20 :C 30) :A)))
    ;   (IF A
    ;       (FORMAT T "A: ~a~%" A)
    ;       (FORMAT T "Not found.~%")))


A version that supports multiple variables is available in the [Alexandria](https://common-lisp.net/project/alexandria/) library.

## Unique symbols to prevent name clashes in macros
The expansion of a macro often needs to use symbols that weren't passed as arguments by the user (as names for local variables, for example). One must make sure that such symbols cannot conflict with a symbol that the user is using in the surrounding code. 

This is usually achieved by using [`GENSYM`](http://www.lispworks.com/documentation/HyperSpec/Body/f_gensym.htm), a function that returns a fresh uninterned symbol.

**Bad**

Consider the macro below. It makes a `DOTIMES`-loop that also collects the result of the body into a list, which is returned at the end.

    (defmacro dotimes+collect ((var count) &body body)
      `(let ((result (list)))
         (dotimes (,var ,count (nreverse result))
           (push (progn ,@body) result))))

    (dotimes+collect (i 5)
      (format t "~a~%" i)
      (* i i))
    ; 0
    ; 1
    ; 2
    ; 3
    ; 4
    ;=> (0 1 4 9 16)

This seems to work in this case, but if the user happened to have a variable name `RESULT`, which they use in the body, the results would probably not be what the user expects. Consider this attempt to write a function that collects a list of sums of all integers up to `N`:

    (defun sums-upto (n)
      (let ((result 0))
        (dotimes+collect (i n)
          (incf result i))))

    (sums-upto 10) ;=> Error!

**Good**

To fix the problem, we need to use `GENSYM` to generate a unique name for the `RESULT`-variable in the macro expansion.

    (defmacro dotimes+collect ((var count) &body body)
      (let ((result-symbol (gensym "RESULT")))
        `(let ((,result-symbol (list)))
           (dotimes (,var ,count (nreverse ,result-symbol))
             (push (progn ,@body) ,result-symbol)))))
    
    (sums-upto 10) ;=> (0 1 3 6 10 15 21 28 36 45)

**TODO: How to make symbols from strings**

**TODO: Avoiding problems with symbols in different packages**

## Using Macros to define data structures
A common use of macros is to create templates for data structures which obey common rules but may contain different fields. By writing a macro, you can allow the detailed configuration of the data structure to be specified without needing to repeat boilerplate code, nor to use a less efficient structure (such as a hash) in memory purely to simplify programming.

For example, suppose that we wish to define a number of classes which have a range of different properties, each with a getter and setter. In addition, for some (but not all) of these properties, we wish to have the setter call a method on the object notifying it that the property has been changed. Although Common LISP already has a shorthand for writing getters and setters, writing a standard custom setter in this way would normally require duplicating the code that calls the notification method in every setter, which could be a pain if there are a large number of properties involved. However, by defining a macro it becomes much easier:

    (defmacro notifier (class slot) 
      "Defines a setf method in (class) for (slot) which calls the object's changed method."
       `(defmethod (setf ,slot) (val (item ,class))
         (setf (slot-value item ',slot) val)
         (changed item ',slot)))

    (defmacro notifiers (class slots)
      "Defines setf methods in (class) for all of (slots) which call the object's changed method."
      `(progn 
         ,@(loop for s in slots collecting `(notifier ,class ,s))))

    (defmacro defclass-notifier-slots (class nslots slots)  
      "Defines a class with (nslots) giving a list of slots created with notifiers, and (slots) giving a list of slots created with regular accessors."
      `(progn
         (defclass ,class () 
           ( ,@(loop for s in nslots collecting `(,s :reader ,s)) 
             ,@(loop for s in slots collecting `(,s :accessor ,s))))   
         (notifiers ,class ,nslots)))


We can now write `(defclass-notifier-slots foo (bar baz qux) (waldo))` and immediately define a class `foo` with a regular slot `waldo` (created by the second part of the macro with the specification `(waldo :accessor waldo)`), and slots `bar`, `baz`, and `qux` with setters that call the `changed` method (where the getter is defined by the first part of the macro, `(bar :reader bar)`, and the setter by the invoked `notifier` macro).

In addition to allowing us to quickly define multiple classes that behave this way, with large numbers of properties, without repetition, we have the usual benefit of code reuse: if we later decide to change how the notifier methods work, we can simply change the macro, and the structure of every class using it will change.


