---
title: "@Named annotation in Kotlin"
slug: "named-annotation-in-kotlin"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

How to correctly use the named annotation in Kotlin v1.1

## Declaring a qualified dependency
    @Module
    class AppModule(val app: Application) {

        @Provides @Named("the_answer")
        fun providesTheAnswer(): Int { 
            return 42
        }
    }

## Setter based dependency injection
    class MyClass{
        @field:[Inject Named("the_answer")] lateinit var answer: Int
    }

In Android Development, this is the way in which you inject dependencies into `Activity`, `Fragment` or any other object that is instantiated directly by the OS.

To learn more about the `@field:` annotation in Kotlin visit the [documentation][1] 


  [1]: https://kotlinlang.org/docs/reference/annotations.html#annotation-use-site-targets

## Constructor based dependency injection
    class MyClass @Inject constructor(@Named val answer: Int){
        /* The nuts and bolts of your class */
    }

