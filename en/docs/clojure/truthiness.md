---
title: "Truthiness"
slug: "truthiness"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Truthiness
In Clojure everything that is not `nil` or `false` is considered logical true.

Examples:

    (boolean nil)        ;=> false
    (boolean false)      ;=> false
    (boolean true)       ;=> true
    (boolean :a)         ;=> true
    (boolean "false")    ;=> true
    (boolean 0)          ;=> true
    (boolean "")         ;=> true
    (boolean [])         ;=> true
    (boolean '())        ;=> true

    (filter identity [:a false :b true])   ;=> (:a :b true)
    (remove identity [:a false :b true])   ;=> (false)

## Booleans
Any value in Clojure is considered truthy unless it is `false` or `nil`. You can find the truthiness of a value with `(boolean value)`. You can find the truthiness of a list of values using `(or)`, which returns `true` if any arguments are truthy, or `(and)` which returns `true` if all arguments are truthy.


    => (or false nil)
    nil    ; none are truthy
    => (and '() [] {} #{} "" :x 0 1 true)
    true    ; all are truthy
    => (boolean "false")
    true    ; because naturally, all strings are truthy


