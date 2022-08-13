---
title: "Spread Operator"
slug: "spread-operator"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

In most cases, the spread operator `*.` is identical to calling `.collect { it.________ }`.

```
def animals = ['cat', 'dog', 'fish']
assert animals*.length() == animals.collect { it.length() }
```

But if the subject is null, they behave a differently:

```
def animals = null
assert animals*.length() == null
assert animals.collect { it.length() } == []
```


## Accessing a property
```
class Vector {
    double x
    double y
}
def points = [
    new Vector(x: 10, y: -5),
    new Vector(x: -17.5, y: 3),
    new Vector(x: -3.3, y: -1)
]

assert points*.x == [10, -17.5, -3.3]
```

Note: The `*` is optional. We could also write the above statement as in the below line and Groovy compiler would still be happy about it.

```
assert points.x == [10, -17.5, -3.3]
```



## Calling a method
```
assert ['cat', 'dog', 'fish']*.length() == [3, 3, 4]
```

Note that when mixing types in the collection if the method not exists on some of the elements, a `groovy.lang.MissingMethodException` could be thrown:

```
['cat', 'dog', 'fish',3]*.length() 
// it throws groovy.lang.MissingMethodException: No signature of method: java.lang.Integer.length()
```



## Its null-safe
If there is a `null` object on the collection it not throws a `NPE`, it returns a `null` instead:

```
assert ['cat', 'dog', 'fish', null]*.length() == [3, 3, 4, null]
```

Using it directly in a `null` object it's also null-safe:

```
def nullCollection = null
assert nullCollection*.length() == null
```

