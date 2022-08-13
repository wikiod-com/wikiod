---
title: "Safe Navigation Operator"
slug: "safe-navigation-operator"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Basic usage
Groovy's _safe navigation operator_ allows to avoid `NullPointerException`s when accessing to methods or attributes on variables that may assume `null` values. It is equivalent to `nullable_var == null ? null : nullable_var.myMethod()`

    def lst = ['foo', 'bar', 'baz']
    
    def f_value = lst.find { it.startsWith('f') }    // 'foo' found
    f_value?.length()    // returns 3
    
    def null_value = lst.find { it.startsWith('z') }    // no element found. Null returned
    
    // equivalent to  null_value==null ? null : null_value.length()
    null_value?.length()    // no NullPointerException thrown
    
    // no safe operator used
    ​null_value.length()​​​​​    // NullPointerException thrown

## Concatenation of safe navigation operators
    class User {
      String name
      int age
    }
    
    def users = [
      new User(name: "Bob", age: 20),
      new User(name: "Tom", age: 50),
      new User(name: "Bill", age: 45)
    ]
    
    def null_value = users.find { it.age > 100 }    // no over-100 found. Null 

    null_value?.name?.length()    // no NPE thrown
    //  null ?. name  ?. length()
    // (null ?. name) ?. length()
    // (    null    ) ?. length()
    // null

    null_value?.name.length()    // NPE thrown
    //  null ?. name  . length()
    // (null ?. name) . length()
    // (    null    ) . length()  ===> NullPointerException

the safe navigation on `null_value?.name` will return a `null` value. Thus `length()` will have to perform a check on `null` value to avoid a `NullPointerException`.

