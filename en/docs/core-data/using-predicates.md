---
title: "Using Predicates"
slug: "using-predicates"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Matching an exact string
    let fetchRequest = NSFetchRequest(entityName: "Foo")
    var thePredicate: NSPredicate?
    thePredicate = NSPredicate(format: "message == 'example'")

>The entity ``Foo`` has a ``message`` string attribute 



## Substitutions
Rather than passing a static string as a predicate's criteria. It is possible to substitute values by using format specifiers. There are five format specifiers: 

 - ``%K`` is a var arg substitution for a key path.
 - ``%@`` is a var arg substitution for an object value-often a string, number, date, or an array.
 - ``%ld`` is a var arg substitution for an int value.
 - ``%la`` is a var arg substitution for a double.
 - ``%a`` is a var arg substitution for a float.


In the following example, the ``%K`` format specifier serves as the left-hand argument which passes in the *"message"* property dynamically. The ``%@`` format specifier serves as the right-hand argument to dynamically pass in a string containing the word *"example"*.
 

    let predicate = NSPredicate(format:"%K == %@", "message", "example")

