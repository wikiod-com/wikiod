---
title: "Ternary and Elvis Operators"
slug: "ternary-and-elvis-operators"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The Elvis operator evaluates based on *Groovy-Truth* of the condition-part.

## Standard form vs Elvis form
    // long form
    String sayHello(String name){
        "Hello, ${name ? name : 'stranger'}."
    }
    
    // elvis
    String sayHello(String name){
        "Hello, ${name ?: 'stranger'}."
    }

Notice that the "elvis" format omits the "true" term because the original comparison value is to be used in the "true" case. If `name` is Groovy `true`, then it will be returned as the value of the expression.

## Usage (with condition) in assignment
    def results = []
    (1..4).each{
        def what = (it%2) ? 'odd' :  'even'
        results << what
    }
    assert results == ['odd', 'even', 'odd', 'even']

Here, the if-condition (in `(parentheses)`) is slightly more complex than just testing for existence/Groovy-Truth.

