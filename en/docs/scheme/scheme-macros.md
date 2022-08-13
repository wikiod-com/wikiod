---
title: "Scheme Macros"
slug: "scheme-macros"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Hygienic and referentially-transparent macros with syntax-rules
LISP and Scheme's greatest advantage over other mainstream programming language is their macro system. Unlike the C preprocessor and other macro languages, Scheme macros take parsed code as input and return expanded code as output. This is one of the applications of Scheme's “code is data” phrase, and it is what makes the language so powerful.

Macros in Scheme are created with `define-syntax`, which can define a macro in a number of ways. The simplest method is to use `syntax-rules`, which uses pattern-matching to transform the input code into the output code.

This example creates a simple `for item in list` and `for list as item` syntax for looping over elements in a list:

    (define-syntax for
      (syntax-rules (in as) ; 'in' and 'as' keywords must match in the pattern
        ; When the 'for' macro is called, try matching this pattern
        ((for element in list
              body ...) ; Match one or more body expressions
         ; Transform the input code
         (for-each (lambda (element)
                     body ...)
                   list))
        ; Try matching another pattern if the first fails
        ((for list as element
              body ...)
         ; Use the existing macro for the transform
         (for element in list
              body ...))))

These two macros can then be used as follows, providing a more imperative style:

    (let ((names '(Alice Bob Eve)))
      (for name in names
        (display "Hello ")
        (display name)
        (newline))
      (for names as name
        (display "name: ")
        (display name)
        (newline)))

Running the code will provide the expected output:

    Hello Alice
    Hello Bob
    Hello Eve
    name: Alice
    name: Bob
    name: Eve

The most common mistake to look out for is not passing the correct values to a macro, which will often result in an unhelpful error message that applies to the expanded form instead of the macro call.

The `for` syntax definitions above do not check whether they are passed an identifier and a list, so passing any other type will result in an error pointing to the `for-each` call instead of the `for` call. Debugging this defeats the purpose of the macro, so it is up to the user to put the checks in there and report usage errors, which can then be caught at compile time.

