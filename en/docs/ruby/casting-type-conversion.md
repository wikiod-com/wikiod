---
title: "Casting (type conversion)"
slug: "casting-type-conversion"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Casting to a Float
    "123.50".to_f   #=> 123.5
    Float("123.50") #=> 123.5

However, there is a difference when the string is not a valid `Float`:

    "something".to_f   #=> 0.0
    Float("something") # ArgumentError: invalid value for Float(): "something"

## Casting to a String
    123.5.to_s    #=> "123.5"
    String(123.5) #=> "123.5"

Usually, `String()` will just call `#to_s`.

Methods `Kernel#sprintf` and `String#%` behave similar to C:

    sprintf("%s", 123.5) #=> "123.5"
    "%s" % 123.5 #=> "123.5"
    "%d" % 123.5 #=> "123"
    "%.2f" % 123.5 #=> "123.50"

## Casting to an Integer
    "123.50".to_i     #=> 123
    Integer("123.50") #=> 123

A string will take the value of any integer at its start, but will not take integers from anywhere else:

    "123-foo".to_i # => 123
    "foo-123".to_i # => 0

However, there is a difference when the string is not a valid Integer:

    "something".to_i     #=> 0
    Integer("something") # ArgumentError: invalid value for Integer(): "something"


## Floats and Integers
    1/2 #=> 0

Since we are dividing two integers, the result is an integer. To solve this problem, we need to cast at least one of those to Float:

    1.0 / 2      #=> 0.5
    1.to_f / 2   #=> 0.5
    1 / Float(2) #=> 0.5

Alternatively, `fdiv` may be used to return the floating point result of division without explicitly casting either operand:

    1.fdiv 2 # => 0.5

