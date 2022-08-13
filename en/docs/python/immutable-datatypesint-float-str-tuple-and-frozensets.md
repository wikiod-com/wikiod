---
title: "Immutable datatypes(int, float, str, tuple and frozensets)"
slug: "immutable-datatypesint-float-str-tuple-and-frozensets"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Individual characters of strings are not assignable
    foo = "bar"
    foo[0] = "c" # Error 
Immutable variable value can not be changed once they are created.

## Tuple's individual members aren't assignable
    foo = ("bar", 1, "Hello!",)
    foo[1] = 2 # ERROR!! 
Second line would return an error since tuple members once created aren't assignable.
Because of tuple's immutability.<br>


## Frozenset's are immutable and not assignable
    foo = frozenset(["bar", 1, "Hello!"])
    foo[2] = 7 # ERROR
    foo.add(3) # ERROR

Second line would return an error since frozenset members once created aren't assignable.
Third line would return error as frozensets do not support functions that can manipulate members.  

