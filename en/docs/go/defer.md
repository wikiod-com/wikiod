---
title: "Defer"
slug: "defer"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

A `defer` statement pushes a function call onto a list. The list of saved calls is executed after the surrounding function returns. Defer is commonly used to simplify functions that perform various clean-up actions.

## Syntax
- defer someFunc(args)
- defer func(){
    //code goes here
  }()

Defer works by injecting a new stack frame (the called function after the `defer` keyword) into the call stack below the currently executing function. This means that defer is guaranteed to run as long as the stack will be unwound (if your program crashes or gets a `SIGKILL`, defer will not execute).

## Defer Basics
A *defer statement* in Go is simply a function call marked to be executed at a later time. Defer statement is an ordinary function call prefixed by the keyword `defer`.

    defer someFunction()

A deferred function is executed once the function that contains the `defer` statement returns. Actual call to the deferred function occurs when the enclosing function:
- executes a return statement 
- falls off the end
- panics

Example:

    func main() {
        fmt.Println("First main statement")
        defer logExit("main") // position of defer statement here does not matter
        fmt.Println("Last main statement")
    }
    
    func logExit(name string) {
        fmt.Printf("Function %s returned\n", name)
    }

Output:

    First main statement
    Last main statement
    Function main returned

If a function has multiple deferred statements, they form a stack. The last `defer` is the first one to execute after the enclosing function returns, followed by subsequent calls to preceding `defer`s in order (below example returns by causing a panic):

    func main() {
        defer logNum(1)
        fmt.Println("First main statement")
        defer logNum(2)
        defer logNum(3)
        panic("panic occurred")
        fmt.Println("Last main statement") // not printed
        defer logNum(3) // not deferred since execution flow never reaches this line
    }
    
    func logNum(i int) {
        fmt.Printf("Num %d\n", i)
    }

Output:

    First main statement
    Num 3
    Num 2
    Num 1
    panic: panic occurred
    
    goroutine 1 [running]:
    ....

Note that deferred functions have their arguments evaluated at the time `defer` executes:

    func main() {
        i := 1
        defer logNum(i) // deferred function call: logNum(1)
        fmt.Println("First main statement")
        i++
        defer logNum(i) // deferred function call: logNum(2)
        defer logNum(i*i) // deferred function call: logNum(4)
        return // explicit return
    }
    
    func logNum(i int) {
        fmt.Printf("Num %d\n", i)
    }

Output:

    First main statement
    Num 4
    Num 2
    Num 1

If a function has named return values, a deferred anonymous function within that function can access and update the returned value even after the function has returned:

    func main() {
        fmt.Println(plusOne(1)) // 2
        return
    }
    
    func plusOne(i int) (result int) { // overkill! only for demonstration
        defer func() {result += 1}() // anonymous function must be called by adding ()

        // i is returned as result, which is updated by deferred function above
        // after execution of below return
        return i
    }

Finally, a `defer` statement is generally used operations that often occur together. For example:
- open and close a file
- connect and disconnect
- lock and unlock a mutex
- mark a waitgroup as done (`defer wg.Done()`)

This use ensures proper release of system resources irrespective of the flow of execution.

    resp, err := http.Get(url)
    if err != nil {
    return err
    }
    defer resp.Body.Close() // Body will always get closed

## Deferred Function Calls


