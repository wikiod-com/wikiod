---
title: "Panic and Recover"
slug: "panic-and-recover"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

This article assumes knowledge of https://www.wikiod.com/go/defer#Defer Basics

For ordinary error handling, read the [topic on error handling]( https://www.wikiod.com/go/error-handling)

## Panic
A panic halts normal execution flow and exits the current function.  Any deferred calls will then be executed before control is passed to the next higher function on the stack.  Each stack's function will exit and run deferred calls until the panic is handled using a deferred `recover()`, or until the panic reaches `main()` and terminates the program.  If this occurs, the argument provided to panic and a stack trace will be printed to `stderr`.


    package main
    
    import "fmt"

    func foo() {
        defer fmt.Println("Exiting foo")
        panic("bar")
    }
    
    func main() {
        defer fmt.Println("Exiting main")
        foo()
    }

Output:

    Exiting foo
    Exiting main
    panic: bar


    goroutine 1 [running]:
    panic(0x128360, 0x1040a130)
        /usr/local/go/src/runtime/panic.go:481 +0x700
    main.foo()
        /tmp/sandbox550159908/main.go:7 +0x160
    main.main()
        /tmp/sandbox550159908/main.go:12 +0x120


It is important to note that `panic` will accept any type as its parameter.

## Recover
Recover as the name implies, can attempt to recover from a `panic`.
The recover *must* be attempted in a deferred statement as normal execution flow has been halted.  The `recover` statement must appear *directly* within the deferred function enclosure.  Recover statements in functions called by deferred function calls will not be honored.  The `recover()` call will return the argument provided to the initial panic, if the program is currently panicking.  If the program is not currently panicking, `recover()` will return `nil`.

    package main

    import "fmt"

    func foo() {
        panic("bar")
    }

    func bar() {
        defer func() {
            if msg := recover(); msg != nil {
                fmt.Printf("Recovered with message %s\n", msg)
            }
        }()
        foo()
        fmt.Println("Never gets executed")
    }

    func main() {
        fmt.Println("Entering main")
        bar()
        fmt.Println("Exiting main the normal way")
    }

Output:

    Entering main
    Recovered with message bar
    Exiting main the normal way

