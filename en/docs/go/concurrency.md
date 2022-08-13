---
title: "Concurrency"
slug: "concurrency"
draft: false
images: []
weight: 9607
type: docs
toc: true
---

In Go, concurrency is achieved through the use of goroutines, and communication between goroutines is usually done with channels. However, other means of synchronization, like mutexes and wait groups, are available, and should be used whenever they are more convenient than channels.

## Syntax
 - go doWork() // run the function doWork as a goroutine
 - ch := make(chan int) // declare new channel of type int
 - ch <- 1 // sending on a channel
 - value = <-ch // receiving from a channel

Goroutines in Go are similar to threads in other languages in terms of usage. Internally, Go creates a number of threads (specified by `GOMAXPROCS`) and then schedules the goroutines to run on the threads. Because of this design, Go's concurrency mechanisms are much more efficient than threads in terms of memory usage and initialization time. 

## Hello World Goroutine
single channel, single goroutine, one write, one read.

    package main
    
    import "fmt"
    import "time"
    
    func main() {
        // create new channel of type string
        ch := make(chan string)
    
        // start new anonymous goroutine
        go func() {
            time.Sleep(time.Second)
            // send "Hello World" to channel
            ch <- "Hello World"
        }()
        // read from channel
        msg, ok := <-ch
        fmt.Printf("msg='%s', ok='%v'\n", msg, ok)
    }


[Run it on playground][1]

The channel `ch` is an **[unbuffered or synchronous channel][2]**.  

The `time.Sleep` is here to illustrate `main()` function will **wait** on the `ch` channel, which means the [function literal][3] executed as a goroutine has the time to send a value through that channel: the [receive operator `<-ch`][4] will block the execution of `main()`. If it didn't, the goroutine would be killed when `main()` exits, and would not have time to send its value.



  [1]: https://play.golang.org/p/t-5U31vPcb
  [2]: https://golang.org/doc/effective_go.html#channels
  [3]: https://golang.org/ref/spec#Function_literals
  [4]: https://golang.org/ref/spec#Receive_operator


## Waiting for goroutines
[Go programs end when the `main` function ends][1], therefore it is common practice to wait for all goroutines to finish. A common solution for this is to use a [sync.WaitGroup][2] object.

    package main

    import (
        "fmt"
        "sync"
    )

    var wg sync.WaitGroup // 1

    func routine(i int) {
        defer wg.Done() // 3
        fmt.Printf("routine %v finished\n", i)
    }

    func main() {
        wg.Add(10) // 2
        for i := 0; i < 10; i++ {
            go routine(i) // *
        }
        wg.Wait() // 4
        fmt.Println("main finished")
    }

[Run the example in the playground](https://play.golang.org/p/64vfZSXXHv)

WaitGroup usage in order of execution:

 1. Declaration of global variable. Making it global is the easiest way to make it visible to all functions and methods.
 2. Increasing the counter. This must be done in the main goroutine because there is no guarantee that a newly started goroutine will execute before 4 due to memory model [guarantees][3].
 3. Decreasing the counter. This must be done at the exit of a goroutine. By using a deferred call, we make sure that it [will be called whenever function ends][4], no matter how it ends.
 4. Waiting for the counter to reach 0. This must be done in the main goroutine to prevent the program from exiting before all goroutines have finished.

\* Parameters are [evaluated before starting a new goroutine][5]. Thus it is necessary to define their values explicitly before `wg.Add(10)` so that possibly-panicking code will not increase the counter. Adding 10 items to the WaitGroup, so it will wait for 10 items before `wg.Wait` returns the control back to `main()` goroutine. Here, the value of i is defined in the for loop.

  [1]: http://golang.org/ref/spec#Program_execution
  [2]: http://golang.org/pkg/sync/#WaitGroup
  [3]: http://golang.org/ref/mem#tmp_5
  [4]: http://golang.org/ref/spec#Defer_statements
  [5]: http://golang.org/ref/spec#Go_statements

## Creating goroutines
Any function can be invoked as a goroutine by prefixing its invocation with the keyword `go`:

    func DoMultiply(x,y int) {
        // Simulate some hard work
        time.Sleep(time.Second * 1)
        fmt.Printf("Result: %d\n", x * y)
    }

    go DoMultiply(1,2) // first execution, non-blocking
    go DoMultiply(3,4) // second execution, also non-blocking

    // Results are printed after a single second only, 
    // not 2 seconds because they execute concurrently:
    // Result: 2
    // Result: 12

Note that the return value of the function is ignored.

## Using closures with goroutines in a loop
When in a loop, the loop variable (val) in the following example is a single variable that changes value as it goes over the loop. Therefore one must do the following to actually pass each val of values to the goroutine:

    for val := range values {
        go func(val interface{}) {
            fmt.Println(val)
        }(val)
    }

If you were to do just do go `func(val interface{}) { ... }()` without passing val, then the value of `val` will be whatever val is when the goroutines actually runs.

Another way to get the same effect is:

    for val := range values {
        val := val
        go func() {
            fmt.Println(val)
        }()
    }

The strange-looking `val := val` creates a new variable in each iteration, which is then accessed by the goroutine.

## Ping pong with two goroutines
    package main

    import (
        "fmt"
        "time"
    )

    // The pinger prints a ping and waits for a pong
    func pinger(pinger <-chan int, ponger chan<- int) {
        for {
            <-pinger
            fmt.Println("ping")
            time.Sleep(time.Second)
            ponger <- 1
        }
    }

    // The ponger prints a pong and waits for a ping
    func ponger(pinger chan<- int, ponger <-chan int) {
        for {
            <-ponger
            fmt.Println("pong")
            time.Sleep(time.Second)
            pinger <- 1
        }
    }

    func main() {
        ping := make(chan int)
        pong := make(chan int)

        go pinger(ping, pong)
        go ponger(ping, pong)

        // The main goroutine starts the ping/pong by sending into the ping channel
        ping <- 1

        for {
            // Block the main thread until an interrupt
            time.Sleep(time.Second)
        }
    }

[Run a slightly modified version of this code in Go Playground][1]


  [1]: https://play.golang.org/p/LXcPiIPrgf

## Stopping goroutines
    package main

    import (
        "log"
        "sync"
        "time"
    )

    func main() {
        // The WaitGroup lets the main goroutine wait for all other goroutines
        // to terminate. However, this is no implicit in Go. The WaitGroup must
        // be explicitely incremented prior to the execution of any goroutine 
        // (i.e. before the `go` keyword) and it must be decremented by calling
        // wg.Done() at the end of every goroutine (typically via the `defer` keyword). 
        wg := sync.WaitGroup{}

        // The stop channel is an unbuffered channel that is closed when the main
        // thread wants all other goroutines to terminate (there is no way to 
        // interrupt another goroutine in Go). Each goroutine must multiplex its
        // work with the stop channel to guarantee liveness.
        stopCh := make(chan struct{})


        for i := 0; i < 5; i++ {
            // It is important that the WaitGroup is incremented before we start
            // the goroutine (and not within the goroutine) because the scheduler
            // makes no guarantee that the goroutine starts execution prior to 
            // the main goroutine calling wg.Wait().
            wg.Add(1)
            go func(i int, stopCh <-chan struct{}) {
                // The defer keyword guarantees that the WaitGroup count is 
                // decremented when the goroutine exits.
                defer wg.Done()

                log.Printf("started goroutine %d", i)

                select {
                // Since we never send empty structs on this channel we can 
                // take the return of a receive on the channel to mean that the
                // channel has been closed (recall that receive never blocks on
                // closed channels).   
                case <-stopCh:
                    log.Printf("stopped goroutine %d", i)
                }
            }(i, stopCh)
        }

        time.Sleep(time.Second * 5)
        close(stopCh)
        log.Printf("stopping goroutines")
        wg.Wait()
        log.Printf("all goroutines stopped")
    }

