---
title: "Goroutines"
slug: "goroutines"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

 A goroutine is a lightweight thread managed by the Go runtime.

go f(x, y, z)

starts a new goroutine running

f(x, y, z)

The evaluation of f, x, y, and z happens in the current goroutine and the execution of f happens in the new goroutine.

Goroutines run in the same address space, so access to shared memory must be synchronized. The sync package provides useful primitives, although you won't need them much in Go as there are other primitives.

Reference: https://tour.golang.org/concurrency/1

## Goroutines Basic Program
    package main
    
    import (
        "fmt"
        "time"
    )
    
    func say(s string) {
        for i := 0; i < 5; i++ {
            time.Sleep(100 * time.Millisecond)
            fmt.Println(s)
        }
    }
    
    func main() {
        go say("world")
        say("hello")
    }


A goroutine is a function that is capable of running concurrently with other functions. To create a goroutine we use the keyword **`go`** followed by a function invocation:

    package main
    
    import "fmt"
    
    func f(n int) {
      for i := 0; i < 10; i++ {
        fmt.Println(n, ":", i)
      }
    }
    
    func main() {
      go f(0)
      var input string
      fmt.Scanln(&input)
    }
Generally, function call executes all the statements inside the function body and return to the next line. But, with goroutines we return immediately to the next line as it don't wait for the function to complete. So, a call to a `Scanln` function included, otherwise the program has been exited without printing the numbers.


