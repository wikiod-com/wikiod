---
title: "Error Handling"
slug: "error-handling"
draft: false
images: []
weight: 9800
type: docs
toc: true
---

In Go, unexpected situations are handled using **errors**, not exceptions. This approach is more similar to that of C, using errno, than to that of Java or other object-oriented languages, with their try/catch blocks. However, an error is not an integer but an interface.

A function that may fail typically returns an **error** as its last return value. If this error is not **nil**, something went wrong, and the caller of the function should take action accordingly.

Note how in Go you don't _raise_ an error. Instead, you _return_ an error in case of failure.

If a function can fail, the last returned value is generally an `error` type.

    // This method doesn't fail
    func DoSomethingSafe() {
    }
    
    // This method can fail
    func DoSomething() (error) {
    }
    
    // This method can fail and, when it succeeds,
    // it returns a string.
    func DoAndReturnSomething() (string, error) {
    }



## Creating an error value
The simplest way to create an error is by using the [`errors`](https://golang.org/pkg/errors/) package.

    errors.New("this is an error")

If you want to add additional information to an error, the [`fmt`](https://golang.org/pkg/fmt/) package also provides a useful error creation method:

    var f float64
    fmt.Errorf("error with some additional information: %g", f)

Here's a full example, where the error is returned from a function:

    package main

    import (
        "errors"
        "fmt"
    )

    var ErrThreeNotFound = errors.New("error 3 is not found")

    func main() {
        fmt.Println(DoSomething(1)) // succeeds! returns nil
        fmt.Println(DoSomething(2)) // returns a specific error message
        fmt.Println(DoSomething(3)) // returns an error variable
        fmt.Println(DoSomething(4)) // returns a simple error message
    }

    func DoSomething(someID int) error {
        switch someID {
        case 3:
            return ErrThreeNotFound
        case 2:
            return fmt.Errorf("this is an error with extra info: %d", someID)
        case 1:
            return nil
        }

        return errors.New("this is an error")
    }

[Open in Playground][1]


  [1]: https://play.golang.org/p/4xlwXJo2m0

## Creating a custom error type
In Go, an error is represented by any value that can describe itself as string. Any type that implement the built-in `error` interface is an error.

    // The error interface is represented by a single
    // Error() method, that returns a string representation of the error
    type error interface {
        Error() string
    }

The following example shows how to define a new error type using a string composite literal.

    // Define AuthorizationError as composite literal
    type AuthorizationError string

    // Implement the error interface
    // In this case, I simply return the underlying string
    func (e AuthorizationError) Error() string {
        return string(e)
    }

I can now use my custom error type as error:

    package main

    import (
        "fmt"
    )

    // Define AuthorizationError as composite literal
    type AuthorizationError string

    // Implement the error interface
    // In this case, I simply return the underlying string
    func (e AuthorizationError) Error() string {
        return string(e)
    }

    func main() {
        fmt.Println(DoSomething(1)) // succeeds! returns nil
        fmt.Println(DoSomething(2)) // returns an error message
    }

    func DoSomething(someID int) error {
        if someID != 1 {
            return AuthorizationError("Action not allowed!")
        }

        // do something here

        // return a nil error if the execution succeeded
        return nil
    }


## Handling an error
In Go errors can be returned from a function call. The convention is that if a method can fail, the last returned argument is an `error`.

    func DoAndReturnSomething() (string, error) {
        if os.Getenv("ERROR") == "1" {
            return "", errors.New("The method failed")
        }

        // The method succeeded.
        return "Success!", nil
    }

You use multiple variable assignments to check if the method failed.

    result, err := DoAndReturnSomething()
    if err != nil {
        panic(err)
    }

    // This is executed only if the method didn't return an error
    fmt.Println(result)

If you are not interested in the error, you can simply ignore it by assigning it to `_`.

    result, _ := DoAndReturnSomething()
    fmt.Println(result)

Of course, ignoring an error can have serious implications. Therefore, this is generally not recommended.

If you have multiple method calls, and one or more methods in the chain may return an error, you should propagate the error to the first level that can handle it.

    func Foo() error {
        return errors.New("I failed!")    
    }

    func Bar() (string, error) {
        err := Foo()
        if err != nil {
            return "", err
        }

        return "I succeeded", nil
    }

    func Baz() (string, string, error) {
        res, err := Bar()
        if err != nil {
            return "", "", err
        }

        return "Foo", "Bar", nil
    }



## Returning an error
In Go you don't _raise_ an error. Instead, you _return_ an `error` in case of failure.

    // This method can fail
    func DoSomething() error {
        // functionThatReportsOK is a side-effecting function that reports its
        // state as a boolean. NOTE: this is not a good practice, so this example
        // turns the boolean value into an error. Normally, you'd rewrite this
        // function if it is under your control.
        if ok := functionThatReportsOK(); !ok {
            return errors.New("functionThatReportsSuccess returned a non-ok state")
        }

        // The method succeeded. You still have to return an error
        // to properly obey to the method signature.
        // But in this case you return a nil error.
        return nil
    }

If the method returns multiple values (and the execution can fail), then the standard convention is to return the error as the last argument.

    // This method can fail and, when it succeeds,
    // it returns a string.
    func DoAndReturnSomething() (string, error) {
        if os.Getenv("ERROR") == "1" {
            return "", errors.New("The method failed")
        }

        s := "Success!"

        // The method succeeded.
        return s, nil
    }

    result, err := DoAndReturnSomething()
    if err != nil {
        panic(err)
    }


## Recovering from panic
A common mistake is to declare a slice and start requesting indexes from it without initializing it, which leads to an "index out of range" panic. The following code explains how to recover from the panic without exiting the program, which is the normal behavior for a panic. In most situations, returning an error in this fashion rather than exiting the program on a panic is only useful for development or testing purposes.

    type Foo struct {
        Is []int
    }

    func main() {
        fp := &Foo{}
        if err := fp.Panic(); err != nil {
            fmt.Printf("Error: %v", err)
        } 
        fmt.Println("ok")
    }

    func (fp *Foo) Panic() (err error) {
        defer PanicRecovery(&err)
        fp.Is[0] = 5
        return nil
    }
   
    func PanicRecovery(err *error) {

        if r := recover(); r != nil {
            if _, ok := r.(runtime.Error); ok {
                 //fmt.Println("Panicing")
                 //panic(r)
                 *err = r.(error) 
            } else {
                *err = r.(error)
            }
        }
    }

The use of a separate function (rather than closure) allows re-use of the same function in other functions prone to panic.



