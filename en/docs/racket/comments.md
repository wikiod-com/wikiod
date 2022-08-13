---
title: "Comments"
slug: "comments"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

The most common comment types are line and s-expression comment (using `;` and `#;` respectively).

It is common to use from 1 to 3 semi colons depending on the type of comment made.
Refer to ???

## Single line comments
    ; We make single line comments by writing out text after a semicolon

## Block comments
    #| We make
    block comments
    like this |#

## S-expression comments
    #;(define (commented-out-function x)
        (print (string-append "This entire "
                "s-expression is commented out!")))

## Comments in at-exps
When a module is using at expressions, such as:

    #lang at-exp racket/base

or

    #lang scribble/manual

You have access to the following types of comments:

    @;{Block text that goes
       until the closing
       brace.}

As well as:

    @; Single line text.

Note that if you are using a language that only uses at-exps (such as most scribble languages), you will need ot use one of these types of comments.

