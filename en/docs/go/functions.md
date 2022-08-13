---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9843
type: docs
toc: true
---

Functions in Go provide organized, reusable code to perform a set of actions. Functions simplify the coding process, prevent redundant logic, and make code easier to follow. This topic describes the declaration and utilization of functions, arguments, parameters, return statements and scopes in Go.

## Syntax
- func() // function type with no arguments and no return value
- func(x int) int // accepts integer and returns an integer
- func(a, b int, z float32) bool // accepts 2 integers, one float and returns a boolean
- func(prefix string, values ...int) // "variadic" function which accepts one string and one or more number of integers
- func() (int, bool) // function returning two values
- func(a, b int, z float64, opt ...interface{}) (success bool) // accepts 2 integers, one float and one or more number of interfaces and returns named boolean value (which is already initialized inside of function)

## Literal functions & closures
A simple literal function, printing `Hello!` to stdout:

    package main

    import "fmt"

    func main() {
        func(){
            fmt.Println("Hello!")
        }()
    }

[play it on playground](https://play.golang.org/p/upOAwpOaue)


----------


A literal function, printing the `str` argument to stdout:

    package main
    
    import "fmt"
    
    func main() {
        func(str string) {
            fmt.Println(str)
        }("Hello!")
    }

[play it on playground](https://play.golang.org/p/jz-5wpEkY2)


----------


A literal function, closing over the variable `str`:

    package main

    import "fmt"

    func main() {
        str := "Hello!"
        func() {
            fmt.Println(str)
        }()
    }

[play it on playground](https://play.golang.org/p/j6ZgyAna7l)


----------


It is possible to assign a literal function to a variable:

    package main
    
    import (
        "fmt"
    )
    
    func main() {
        str := "Hello!"
        anon := func() {
            fmt.Println(str)
        }
        anon()
    }


[play it on playground](https://play.golang.org/p/Ick7RmdTFb)

## Basic Declaration
A simple function that does not accept any parameters and does not return any values:

    func SayHello() {
        fmt.Println("Hello!")
    }

## Named Return Values
Return values can be assigned to a local variable. An empty `return` statement can then be used to return their current values. This is known as *"naked"* return. Naked return statements should be used only in short functions as they harm readability in longer functions:

    func Inverse(v float32) (reciprocal float32) {
        if v == 0 {
            return
        }
        reciprocal = 1 / v
        return
    }
[play it on playground](https://play.golang.org/p/dS_bGmP6W0)
    
    //A function can also return multiple values
    func split(sum int) (x, y int) {
        x = sum * 4 / 9
        y = sum - x
        return
    }
[play it on playground](https://play.golang.org/p/upOAwpOaue)

Two important things must be noted:

- The parenthesis around the return values are **mandatory**.
- An empty `return` statement must always be provided.

## Variadic functions
A variadic function can be called with any number of **trailing** arguments. Those elements are stored in a slice.

    package main
    
    import "fmt"
    
    func variadic(strs ...string) {
         // strs is a slice of string
         for i, str := range strs {
             fmt.Printf("%d: %s\n", i, str)
         }
    }
    
    func main() {
         variadic("Hello", "Goodbye")
         variadic("Str1", "Str2", "Str3")
    }

[play it on playground](https://play.golang.org/p/rnzg1yK_Va)

You can also give a slice to a variadic function, with `...`:

    func main() {
         strs := []string {"Str1", "Str2", "Str3"}
    
         variadic(strs...)
    }
[play it on playground](https://play.golang.org/p/gl4L5R9v8_)

## Return Values
A function can return one or more values to the caller:

    
    func AddAndMultiply(a, b int) (int, int) {
        return a+b, a*b
    }

The second return value can also be the error var :

    import errors

    func Divide(dividend, divisor int) (int, error) {
        if divisor == 0 {
            return 0, errors.New("Division by zero forbidden")
        }
        return dividend / divisor, nil
    }

Two important things must be noted:

- The parenthesis may be omitted for a single return value.
- Each `return` statement must provide a value for **all** declared return values.

## Parameters
A function can optionally declare a set of parameters:

    func SayHelloToMe(firstName, lastName string, age int) {
        fmt.Printf("Hello, %s %s!\n", firstName, lastName)
        fmt.Printf("You are %d", age)
    }

Notice that the type for `firstName` is omitted because it is identical to `lastName`.

