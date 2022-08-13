---
title: "Channels"
slug: "channels"
draft: false
images: []
weight: 9887
type: docs
toc: true
---

A channel contains values of a given type. Values can be written to a channel and read from it, and they circulate inside the channel in first-in-first-out order. There is a distinction between buffered channels, which can contain several messages, and unbuffered channels, which cannot. Channels are typically used to communicate between goroutines, but are also useful in other circumstances.

## Syntax
- make(chan int) // create an unbuffered channel
- make(chan int, 5) // create a buffered channel with a capacity of 5
- close(ch) // closes a channel "ch"
- ch <- 1 // write the value of 1 to a channel "ch"
- val := <-ch // read a value from channel "ch"
- val, ok := <-ch // alternate syntax; ok is a bool indicating if the channel is closed

A channel holding the empty struct `make(chan struct{})` is a clear message to the user that no information is transmitted over the channel and that it's purely used for synchronization.

Regarding unbuffered channels, a channel write will block until a corresponding read occurs from another goroutine. The same is true for a channel read blocking while waiting for a writer.

## Timeouts
Channels are often used to implement timeouts.

    func main() {
        // Create a buffered channel to prevent a goroutine leak. The buffer
        // ensures that the goroutine below can eventually terminate, even if
        // the timeout is met. Without the buffer, the send on the channel
        // blocks forever, waiting for a read that will never happen, and the
        // goroutine is leaked.
        ch := make(chan struct{}, 1)
    
        go func() {
            time.Sleep(10 * time.Second)
            ch <- struct{}{}
        }()
    
        select {
        case <-ch:
            // Work completed before timeout.
        case <-time.After(1 * time.Second):
            // Work was not completed after 1 second.
        }
    }



## Using range
When reading multiple values from a channel, using `range` is a common pattern:

```
func foo() chan int {
    ch := make(chan int)
    
    go func() {
        ch <- 1
        ch <- 2
        ch <- 3
        close(ch)

    }()
    
    return ch
}

func main() {    
    for n := range foo() {
        fmt.Println(n)
    }
    
    fmt.Println("channel is now closed")
}
```

[Playground][1]

Output

```
1
2
3
channel is now closed
```


  [1]: https://play.golang.org/p/18ODvaZub9

## Coordinating goroutines
Imagine a goroutine with a two step process, where the main thread needs to do some work between each step:

```
func main() {
    ch := make(chan struct{})
    go func() {
        // Wait for main thread's signal to begin step one
        <-ch
        
        // Perform work
        time.Sleep(1 * time.Second)
        
        // Signal to main thread that step one has completed
        ch <- struct{}{}
        
        // Wait for main thread's signal to begin step two
        <-ch
        
        // Perform work
        time.Sleep(1 * time.Second)
        
        // Signal to main thread that work has completed
        ch <- struct{}{}
    }()
    
    // Notify goroutine that step one can begin
    ch <- struct{}{}
    
    // Wait for notification from goroutine that step one has completed
    <-ch

    // Perform some work before we notify
    // the goroutine that step two can begin
    time.Sleep(1 * time.Second)
    
    // Notify goroutine that step two can begin
    ch <- struct{}{}
    
    // Wait for notification from goroutine that step two has completed
    <-ch
}
```

## Buffered vs unbuffered
    func bufferedUnbufferedExample(buffered bool) {
        // We'll declare the channel, and we'll make it buffered or
        // unbuffered depending on the parameter `buffered` passed
        // to this function.
        var ch chan int
        if buffered {
            ch = make(chan int, 3)
        } else {
            ch = make(chan int)
        }
        
        // We'll start a goroutine, which will emulate a webserver
        // receiving tasks to do every 25ms.
        go func() {
            for i := 0; i < 7; i++ {
                // If the channel is buffered, then while there's an empty
                // "slot" in the channel, sending to it will not be a
                // blocking operation. If the channel is full, however, we'll
                // have to wait until a "slot" frees up.
                // If the channel is unbuffered, sending will block until
                // there's a receiver ready to take the value. This is great
                // for goroutine synchronization, not so much for queueing
                // tasks for instance in a webserver, as the request will
                // hang until the worker is ready to take our task.
                fmt.Println(">", "Sending", i, "...")
                ch <- i
                fmt.Println(">", i, "sent!")
                time.Sleep(25 * time.Millisecond)
            }
            // We'll close the channel, so that the range over channel
            // below can terminate.
            close(ch)
        }()
        
        for i := range ch {
            // For each task sent on the channel, we would perform some
            // task. In this case, we will assume the job is to
            // "sleep 100ms".
            fmt.Println("<", i, "received, performing 100ms job")
            time.Sleep(100 * time.Millisecond)
            fmt.Println("<", i, "job done")
        }
    }

[go playground](https://play.golang.org/p/PUR0kDdxli)

## Blocking & unblocking of channels
By default communication over the channcels is sync; when you send some value there must be a receiver. Otherwise you will get `fatal error: all goroutines are asleep - deadlock!` as follows:

    package main
    
    import "fmt"
    
    func main() {
        msg := make(chan string)
        msg <- "Hey There"
        go func() {
            fmt.Println(<-msg)
        }()
    }
Bu there is a solution use: use buffered channels :

    package main
    
    import "fmt"
    import "time"
    
    func main() {
        msg :=make(chan string, 1)
        msg <- "Hey There!"
        go func() {
            fmt.Println(<-msg)
        }()
        time.Sleep(time.Second * 1)
    }

## Waiting for work to finish
A common technique for using channels is to create some number of workers (or consumers) to read from the channel. Using a sync.WaitGroup is an easy way to wait for those workers to finish running.

```
package main

import (
    "fmt"
    "sync"
    "time"
)

func main() {
    numPiecesOfWork := 20
    numWorkers := 5

    workCh := make(chan int)
    wg := &sync.WaitGroup{}

    // Start workers
    wg.Add(numWorkers)
    for i := 0; i < numWorkers; i++ {
        go worker(workCh, wg)
    }

    // Send work
    for i := 0; i < numPiecesOfWork; i++ {
        work := i % 10 // invent some work
        workCh <- work
    }

    // Tell workers that no more work is coming
    close(workCh)

    // Wait for workers to finish
    wg.Wait()

    fmt.Println("done")
}

func worker(workCh <-chan int, wg *sync.WaitGroup) {
    defer wg.Done() // will call wg.Done() right before returning

    for work := range workCh { // will wait for work until workCh is closed
        doWork(work)
    }
}

func doWork(work int) {
    time.Sleep(time.Duration(work) * time.Millisecond)
    fmt.Println("slept for", work, "milliseconds")
}
```

