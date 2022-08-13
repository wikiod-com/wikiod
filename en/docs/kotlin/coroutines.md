---
title: "coroutines"
slug: "coroutines"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Examples of Kotlin's experimental(yet) implementation of coroutines

## Simple coroutine which delay's 1 second but not blocks
 (from official [doc][1])

    fun main(args: Array<String>) {
        launch(CommonPool) { // create new coroutine in common thread pool
            delay(1000L) // non-blocking delay for 1 second (default time unit is ms)
            println("World!") // print after delay
        }
        println("Hello,") // main function continues while coroutine is delayed
        Thread.sleep(2000L) // block main thread for 2 seconds to keep JVM alive
    }

result 

    Hello,
    World!


  [1]: https://github.com/Kotlin/kotlinx.coroutines/blob/master/coroutines-guide.md#your-first-coroutine

