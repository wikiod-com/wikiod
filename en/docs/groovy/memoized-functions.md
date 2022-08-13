---
title: "Memoized Functions"
slug: "memoized-functions"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Memoize on closures
Since *Groovy* 1.8 a convenient `memoize()` method is added on closures:

```
// normal closure
def sum = { int x, int y  ->
    println "sum ${x} + ${y}"
    return x + y
}
sum(3, 4)
sum(3, 4)
// prints
// sum 3 + 4
// sum 3 + 4

// memoized closure
def sumMemoize = sum.memoize()
sumMemoize(3, 4)
// the second time the method is not called 
// and the result it's take from the previous 
// invocation cache
sumMemoize(3, 4)
// prints
// sum 3 + 4
```

## Memoize on methods
Since *Groovy 2.2* `groovy.transform.Memoized` annotation is added to convenient memoize methods with simply adding the `@Memoized` annotation:

```
import groovy.transform.Memoized

class Calculator {
    int sum(int x, int y){
        println "sum ${x} + ${y}"
        return x+y
    }   
    
    @Memoized
    int sumMemoized(int x, int y){
        println "sumMemoized ${x} + ${y}"
        return x+y
    }
}

def calc = new Calculator()

// without @Memoized, sum() method is called twice
calc.sum(3,4)
calc.sum(3,4)
// prints
// sum 3 + 4
// sum 3 + 4

// with @Memoized annotation
calc.sumMemoized(3,4)
calc.sumMemoized(3,4)
// prints
// sumMemoized 3 + 4
```

