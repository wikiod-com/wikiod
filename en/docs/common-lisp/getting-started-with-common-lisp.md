---
title: "Getting started with common-lisp"
slug: "getting-started-with-common-lisp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Common Lisp Learning Resources
**Online Books**

These are books that are freely accessible online.

* [Practical Common Lisp by Peter Seibel](http://www.gigamonkeys.com/book/) is a good introduction to CL for experienced programmers, which tries to highlight from the very beginning what makes CL different to other languages.
* [Common Lisp: A Gentle Introduction to Symbolic Computation by David S. Touretzky](http://www-2.cs.cmu.edu/~dst/LispBook/) is a good introduction for people new to programming.
* [Common Lisp: An interactive approach by Stuart C. Shapiro](https://www.cse.buffalo.edu/~shapiro/Commonlisp/) was used as a course textbook, and course notes accompany the book on the website.
* [Common Lisp, the Language by Guy L. Steele](https://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html) is a description of the Common Lisp language. According to the [CLiki](http://cliki.net/Getting+Started) it is outdated, but it contains better descriptions of the [loop macro](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node235.html#SECTION003000000000000000000) and [format](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html) than the Common Lisp Hyperspec does.
* [On Lisp by Paul Graham](http://www.paulgraham.com/onlisp.html) is a great book for intermediately experienced Lispers.
* [Let Over Lambda by Doug Hoyte](http://letoverlambda.com/) is an advanced book on Lisp Macros. [Several people recommended](https://www.reddit.com/r/lisp/comments/3actsc/let_over_lambda/) that you be comfortable with On Lisp before reading this book, and that the start is slow.

**Online References**

* [The Common Lisp Hyperspec](http://www.lispworks.com/documentation/common-lisp.html) is *the* language reference document for Common Lisp.
* [The Common Lisp Cookbook](http://cl-cookbook.sourceforge.net/) is a list of useful Lisp recipes. Also contains a list of other online sources of CL information.
* [Common Lisp Quick Reference](http://clqr.boundp.org/) has printable Lisp reference sheets.
* [Lispdoc.com](http://lispdoc.com/) searches several sources of Lisp information (Practical Common Lisp, Successful Lisp, On Lisp, the HyperSpec) for documentation.
* [L1sp.org](http://l1sp.org/html/) is a redirect service for documentation.

**Offline Books**

These are books that you'll likely have to buy, or lend from a library.

* [ANSI Common Lisp by Paul Graham](http://www.paulgraham.com/acl.html).
* [Common Lisp Recipes by Edmund Weitz](http://weitz.de/cl-recipes/).
* [Paradigms of Artificial Intelligence Programming](http://norvig.com/paip.html) has many interesting applications of Lisp, but is not a good reference for AI any more.

**Online Communities**

* The [CLiki](http://www.cliki.net/index) has a great [Getting Started Page](http://cliki.net/Getting+Started). A great resource for all things CL. Has an extensive list of [Lisp books](http://cliki.net/Lisp%20books).
* [Common Lisp subreddit](https://www.reddit.com/r/Common_Lisp/) has loads of useful links and reference documents in the sidebar. 
* IRC: #lisp, #ccl, #sbcl and [others](http://www.cliki.net/IRC) on [Freenode](https://freenode.net/).
* [Common-Lisp.net](https://common-lisp.net/) provides hosting for many [common lisp projects](https://common-lisp.net/phub/) and user groups.


**Libraries**

* <a href="https://www.quicklisp.org/beta/">Quicklisp</a> is library manager for Common Lisp, and has a long [list of supported libraries](https://www.quicklisp.org/beta/releases.html).
* [Quickdocs](http://quickdocs.org/) hosts library documentation for many CL libraries.
* [Awesome CL](https://github.com/CodyReichert/awesome-cl) is a community-driven curated list of libraries, frameworks and other shiny stuff sorted by category.


**Pre-packaged Lisp Environments**

These are Lisp editing environments that are easy to install and get started with because everything you need is pre-packaged and pre-configured.
* [Portacle](https://shinmera.github.io/portacle/) is a portable and multiplatform Common Lisp environment. It ships a slightly customized Emacs with Slime, SBCL (a popular Common Lisp implementation), Quicklisp and Git. No installation needed, so it's a very quick and easy way to get going.
* [Lispbox](https://common-lisp.net/project/lispbox/) is an IDE (Emacs + SLIME), Common Lisp environment (Clozure Common Lisp) and library manager (Quicklisp), pre-packaged as archives for Windows, Mac OSX and Linux. Descendant of "Lisp in a Box" Recommended in the Practical Common Lisp book.
* Not pre-packed, but [SLIME](http://common-lisp.net/project/slime/) turns Emacs into a Common Lisp IDE, and has a [user manual](http://common-lisp.net/project/slime/doc/html/) to help you get started. Requires a separate Common Lisp implementation.

**Common Lisp Implementations**

This section lists some common CL implementations and their manuals. Unless otherwise noted, these are free software implementations. See also the [Cliki's list of free software Common Lisp Implementations](http://www.cliki.net/Common%20Lisp%20implementation), and [Wikipedia's list of commercial Common Lisp Implementations](https://en.wikipedia.org/wiki/Common_Lisp#Commercial_implementations).

* [Allegro Common Lisp (ACL)](http://franz.com/products/allegrocl/) and [manual](http://www.franz.com/support/documentation/). Commercial, but has a free [Express Edition](http://franz.com/downloads/clp/survey) and [training videos on Youtube](https://www.youtube.com/channel/UCN36UrxtyNBJPaG0kmBJNRw).
* [CLISP](http://clisp.org/) and [manual](http://www.clisp.org/impnotes.html).
* [Clozure Common Lisp (CCL)](http://ccl.clozure.com/) and [manual](http://ccl.clozure.com/manual/).
* [Carnegie Mellon University Common Lisp (CMUCL)](https://www.cons.org/cmucl/), has a  [manual and other useful information](http://www.cons.org/cmucl/doc/index.html) page.
* [Embeddable Common Lisp (ECL)](https://common-lisp.net/project/ecl/) and [manual](https://common-lisp.net/project/ecl/static/manual/).
* [LispWorks](http://www.lispworks.com/products/index.html) and [manual](http://www.lispworks.com/documentation/index.html). Commercial, but has a [Personal Edition with some limitations](http://www.lispworks.com/downloads/index.html).
* [Steel Bank Common Lisp (SBCL)](http://www.sbcl.org/) and [manual](http://www.sbcl.org/manual/index.html).
* [Scieneer Common Lisp (SCL)](http://www.scieneer.com/scl/) and [manual](http://www.scieneer.com/scl/doc/) is a commercial Linux and Unix implementation, but has an [unrestricted free evaluation and non-commercial use version](http://www.scieneer.com/scl/free.html).


## Basic expressions
Let’s try some basic expression in the REPL:

    CL-USER> (+ 1 2 3)
    6
    CL-USER> (- 3 1 1)
    1
    CL-USER> (- 3)
    -3
    CL-USER> (+ 5.3 (- 3 2) (* 2 2))
    10.3
    CL-USER> (concatenate 'string "Hello, " "World!")
    "Hello, World!"
    CL-USER> 

The basic building block of a Common Lisp program is the *form*. In these examples we have *functions forms*, that is expressions, written as list, in which the first element is an operator (or function) and the rest of the elements are the operands (this is called “Prefix Notation”, or “Polish Notation”). Writing forms in the REPL causes their evaluation. In the examples you can see simple expressions whose arguments are constant numbers, strings and symbols (in the case of `'string`, which is the name of a type). You can also see that arithmetic operators can take any number of arguments.

It is important to note that parentheses are an integral part of the syntax, and cannot be used freely as in other programming languages. For instance the following is an error:

    (+ 5 ((+ 2 4)))
    > Error: Car of ((+ 2 4)) is not a function name or lambda-expression. ...

In Common Lisp forms can also be data, symbols, macro forms, special forms and lambda forms. They can be written to be evaluated, returning zero, one, or more values, or can be given in input to a macro, that transform them in other forms.



## Hello, Name
This is a slightly more advanced example that shows a few more features of common lisp. We start with a simple `Hello, World!` function and demonstrate some interactive development at the REPL. Note that any text from a semicolon, `;`, to the rest of the line is a comment.

```
CL-USER> (defun hello ()
       (format t "Hello, World!~%")) ;We start as before
HELLO
CL-USER> (hello)
Hello, World!
NIL
CL-USER> (defun hello-name (name) ;A function to say hello to anyone
       (format t "Hello, ~a~%" name)) ;~a prints the next argument to format
HELLO-NAME
CL-USER> (hello-name "Jack")
Hello, Jack
NIL
CL-USER> (hello-name "jack") ;doesn't capitalise names
Hello, jack
NIL
CL-USER> (defun hello-name (name) ;format has a feature to convert to title case
       (format t "Hello, ~:(~a~)~%" name)) ;anything between ~:( and ~) gets it
WARNING: redefining COMMON-LISP-USER::HELLO-NAME in DEFUN
HELLO-NAME
CL-USER> (hello-name "jack")
Hello, Jack
NIL
CL-USER> (defun hello-name (name)
       (format t "Hello, ~:(~a~)!~%" name))
WARNING: redefining COMMON-LISP-USER::HELLO-NAME in DEFUN
HELLO-NAME
CL-USER> (hello-name "jack") ;now this works
Hello, Jack!
NIL
CL-USER> (defun hello (&optional (name "world")) ;we can take an optional argument
       (hello-name name)) ;name defaults to "world"
WARNING: redefining COMMON-LISP-USER::HELLO in DEFUN
HELLO
CL-USER> (hello)
Hello, World!
NIL
CL-USER> (hello "jack")
Hello, Jack!
NIL
CL-USER> (hello "john doe") ;note that this capitalises both names
Hello, John Doe!
NIL
CL-USER> (defun hello-person (name &key (number))
       (format t "Hello, ~a ~r" name number)) ;~r prints a number in English
HELLO-PERSON
CL-USER> (hello-person "Louis" :number 16) ;this doesn't quite work
Hello, Louis sixteen
NIL
CL-USER> (defun hello-person (name &key (number))
       (format t "Hello, ~:(~a ~:r~)!" name number)) ;~:r prints an ordinal
WARNING: redefining COMMON-LISP-USER::HELLO-PERSON in DEFUN
HELLO-PERSON
CL-USER> (hello-person "Louis" :number 16)
Hello, Louis Sixteenth!
NIL
CL-USER> (defun hello-person (name &key (number))
       (format t "Hello, ~:(~a ~@r~)!" name number)) ;~@r prints Roman numerals
WARNING: redefining COMMON-LISP-USER::HELLO-PERSON in DEFUN
HELLO-PERSON
CL-USER> (hello-person "Louis" :number 16)
Hello, Louis Xvi!
NIL
CL-USER> (defun hello-person (name &key (number)) ;capitalisation was wrong
       (format t "Hello, ~:(~a~) ~:@r!" name number))
WARNING: redefining COMMON-LISP-USER::HELLO-PERSON in DEFUN
HELLO-PERSON
CL-USER> (hello-person "Louis" :number 16) ;thats better
Hello, Louis XVI!
NIL
CL-USER> (hello-person "Louis") ;we get an error because NIL is not a number
Hello, Louis ; Evaluation aborted on #<SB-FORMAT:FORMAT-ERROR {1006641AB3}>.
CL-USER> (defun say-person (name &key (number 1 number-p)
                                      (title nil) (roman-number t))
       (let ((number (if number-p
                 (typecase number
                   (integer
                (format nil (if roman-number " ~:@r" " ~:(~:r~)") number))
                   (otherwise
                (format nil " ~:(~a~)" number)))
                 "")) ; here we define a variable called number
         (title (if title 
                (format nil "~:(~a~) " title)
                ""))) ; and here one called title
         (format nil "~a~:(~a~)~a" title name number))) ;we use them here

SAY-PERSON
CL-USER> (say-person "John") ;some examples
"John"
CL-USER> (say-person "john doe")
"John Doe"
CL-USER> (say-person "john doe" :number "JR")
"John Doe Jr"
CL-USER> (say-person "john doe" :number "Junior")
"John Doe Junior"
CL-USER> (say-person "john doe" :number 1)
"John Doe I"
CL-USER> (say-person "john doe" :number 1 :roman-number nil) ;this is wrong
"John Doe First"
CL-USER> (defun say-person (name &key (number 1 number-p)
                                      (title nil) (roman-number t))
       (let ((number (if number-p
                 (typecase number
                   (integer
                (format nil (if roman-number " ~:@r" " the ~:(~:r~)") number))
                   (otherwise
                (format nil " ~:(~a~)" number)))
                 ""))
         (title (if title 
                (format nil "~:(~a~) " title)
                "")))
         (format nil "~a~:(~a~)~a" title name number)))
WARNING: redefining COMMON-LISP-USER::SAY-PERSON in DEFUN
SAY-PERSON
CL-USER> (say-person "john doe" :number 1 :roman-number nil) ;thats better
"John Doe the First"
CL-USER> (say-person "louis" :title "king" :number 16 :roman-number nil)
"King Louis the Sixteenth"
CL-USER> (say-person "louis" :title "king" :number 16 :roman-number t)
"King Louis XVI"
CL-USER> (defun hello (&optional (name "World") &rest arguments) ;now we will just
       (apply #'hello-name name arguments)) ;pass all arguments to hello-name
WARNING: redefining COMMON-LISP-USER::HELLO in DEFUN
HELLO
CL-USER> (defun hello-name (name &rest arguments) ;which will now just use
       (format t "Hello, ~a!" (apply #'say-person name arguments))) ;say-person
WARNING: redefining COMMON-LISP-USER::HELLO-NAME in DEFUN
HELLO-NAME
CL-USER> (hello "louis" :title "king" :number 16) ;this works now
Hello, King Louis XVI!
NIL
CL-USER>
```

This highlights some of the advanced features of Common Lisp's `format` function as well as some features like optional parameters and keyword arguments (e.g. `:number`). This also gives an example of interactive development at a REPL in common lisp.

## The simple Hello World program in REPL
Common Lisp REPL is an interactive environment. Every form written after the prompt is evaluated, and its value is afterwards printed as result of the evaluation. So the simplest possible “Hello, World!” program in Common Lisp is:

    CL-USER> "Hello, World!"
    "Hello, World!"
    CL-USER>

What happens here is that a string costant is given in input to the REPL, it is evaluated and the result is printed. What can be seen from this example is that strings, like numbers, special symbols like `NIL` and `T` and a few other literals, are *self-evaluating* forms: that is they evaluate to themselves.

## Lambda Expressions and Anonymous Functions
An [anonymous function][1] can be defined without a name through a [Lambda Expression][2]. For defining these type of functions, the keyword `lambda` is used  instead of the keyword `defun`. The following lines are all equivalent and define anonymous functions which output the sum of two numbers:

    (lambda (x y) (+ x y))
    (function (lambda (x y) (+ x y)))
    #'(lambda (x y) (+ x y))

Their usefulness is noticeable when creating [Lambda forms][3], i.e. a [form that is a list][4] where the first element is the lambda expression and the remaining elements are the anonymous function's arguments. Examples ([online execution][5]):

    (print ((lambda (x y) (+ x y)) 1 2)) ; >> 3

    (print (mapcar (lambda (x y) (+ x y)) '(1 2 3) '(2 -5 0))) ; >> (3 -3 3)


  [1]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Anonymous-Functions.html#Anonymous-Functions
  [2]: http://stackoverflow.com/q/13213611/6225838
  [3]: http://www.lispworks.com/documentation/HyperSpec/Body/03_ababd.htm
  [4]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_form
  [5]: http://rextester.com/DNB60705

## Sum of list of integers
    (defun sum-list-integers (list)
        (reduce '+ list))

    ; 10
    (sum-list-integers '(1 2 3 4))

    ; 55
    (sum-list-integers '(1 2 3 4 5 6 7 8 9 10))

## Hello World
What follows is an excerpt from a REPL session with Common Lisp in which a "Hello, World!" function is defined and executed. See the remarks at the bottom of this page for a more thorough description of a REPL.

```
CL-USER> (defun hello ()
           (format t "Hello, World!~%"))
HELLO
CL-USER> (hello)
Hello, World!
NIL
CL-USER> 
```

This defines the "function" of zero arguments named `hello`, which will write the string `"Hello, World!"` followed by a newline to standard output, and return `NIL`. 

To define a function we write
```
(defun name (parameters...)
  code...)
```
In this case the function is called `hello`, takes no parameters and the code it runs is to do one function call. The returned value from a lisp function is the last bit of code in the function to run so `hello` returns whatever `(format t "Hello, World!~%")` returns.

In lisp to call a function one writes `(function-name arguments...)` where `function-name` is the name of the function and `arguments...` is the (space-separated) list of arguments to the call. There are some special cases which look like function calls but are not, for example, in the above code there is no `defun` function that gets called, it gets treated specially and defines a function instead.

At the second prompt of the REPL, after we have defined the `hello` function, we call it with no parameters by writing `(hello)`. This in turn will call the `format` function with the parameters `t` and `"Hello, World!~%"`. The `format` function produces formatted output based on the arguments which it is given (a bit like an advanced version of `printf` in C). The first argument tells it where to output to, with `t` meaning standard-output. The second argument tells it what to print (and how to interpret any extra parameters). The directive (special code in the second argument) `~%` tells format to print a newline (i.e. on UNIX it might write `\n` and on windows `\r\n`). Format usually returns `NIL` (a bit like `NULL` in other languages).

After the second prompt we see that `Hello, World` has been printed and on the next line that the returned value was `NIL`.

