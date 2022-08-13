---
title: "Ways of Iteration in Groovy"
slug: "ways-of-iteration-in-groovy"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Groovy has more ways of looping besides supporting the Java iterations. 

Groovy extends the ```Integer``` class with the ```step()```, ```upto()``` and ```times()``` methods. These methods take a closure as a parameter. In the closure we define the piece of code we want to be executed several times.

It also adds ```each()``` and ```eachWithIndex()``` methods to iterate over collections.



## How can I do something n times?
How can I print *hello world* 5 times?

    5.times{
        println "hello world"
    }

## Each and EachWithIndex
`each` and `eachWithIndex` are methods to iterate over collections. 

each have `it`(default iterator) and `eachWithIndex` have `it`,`index`(default iterator, default index).

We can also change the default iterator/index. Please see below examples.

    def list = [1,2,5,7]
    list.each{
        println it
    }

    list.each{val->
        println val
    }

    list.eachWithIndex{it,index->
        println "value " + it + " at index " +index
    }

