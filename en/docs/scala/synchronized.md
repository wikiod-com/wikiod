---
title: "synchronized"
slug: "synchronized"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - `objectToSynchronizeOn.synchronized { /* code to run */}`
 - `synchronized {/* code to run, can be suspended with wait */}`

## synchronize on an object
`synchronized` is a low-level concurrency construct that can help preventing multiple threads access the same resources. [Introduction for the JVM using the Java language](https://docs.oracle.com/javase/tutorial/essential/concurrency/locksync.html).

    anInstance.synchronized {
      // code to run when the intristic lock on `anInstance` is acquired
      // other thread cannot enter concurrently unless `wait` is called on `anInstance` to suspend
      // other threads can continue of the execution of this thread if they `notify` or `notifyAll` `anInstance`'s lock
    }

In case of `object`s it might synchronize on the class of the object, not on the singleton instance.

## synchronize implicitly on this
     /* within a class, def, trait or object, but not a constructor */
     synchronized {
       /* code to run when an intrisctic lock on `this` is acquired */
       /* no other thread can get the this lock unless execution is suspended with
        * `wait` on `this`
        */
     }

