---
title: "String Interpolation"
slug: "string-interpolation"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

## Syntax
- $
- ${}
- ${->}

## Basic
    def str = 'nice'
    assert "Groovy is $str" == 'Groovy is nice'

## Dotted Expression
    def arg = [phrase: 'interpolated']
    assert "This is $arg.phrase" == 'This is interpolated'

## Eager expression
    def str = 'old'
    def interpolated = "I am the ${str} value"
    assert interpolated == 'I am the old value'
    str = 'new'
    assert interpolated == 'I am the old value'

## Lazy expression
We can have lazy interpolation in Strings. This is different than normal interpolation as the GString can potentially have different values, depending on the closure, whenever it is converted into a String.

    def str = 'old'
    def interpolated = "I am the ${ -> str} value"
    assert interpolated == 'I am the old value'
    str = 'new'
    assert interpolated == 'I am the new value'


## Expression
    def str = 'dsl'
    def interpolated = "Groovy ${str.length() + 1} easy ${str.toUpperCase()}"
    assert interpolated == 'Groovy 4 easy DSL'
    str = 'Domain specific language'
    assert interpolated == 'Groovy 4 easy DSL'

