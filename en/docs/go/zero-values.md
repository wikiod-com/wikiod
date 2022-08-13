---
title: "Zero values"
slug: "zero-values"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Explanation
Zero values or zero initialization are simple to implement. Coming from languages like Java it may seem complicated that some values can be `nil` while others are not. In summary from [Zero Value: The Go Programming Language Specification][1]:

> Pointers, functions, interfaces, slices, channels, and maps are the only types that can be nil. The rest are initializated to false, zero, or empty strings based on their respective types.

If a functions that checks some condition, problems may arise:

    func isAlive() bool {
        //Not implemented yet
        return false
    }

The zero value will be false even before implementation. Unit tests dependant on the return of this function could be giving false positives/negatives.

A typical workaround is to also return an error, which is idiomatic in Go:

    package main

    import "fmt"
    
    func isAlive() (bool, error) {
        //Not implemented yet
        return false, fmt.Errorf("Not implemented yet")
    }
    
    func main() {
        _, err := isAlive()
        if err != nil {
            fmt.Printf("ERR: %s\n", err.Error())
        }
        
    }

[play it on playground](https://play.golang.org/p/Ix04UCi9hI)

When returning both a struct and an error you need a User structure for return, which is not very elegant. There are two counter-options:

 - Work with interfaces: Return nil by returning an interface.
 - Work with pointers: A pointer **can** be `nil`

For example, the following code returns a pointer:

    func(d *DB) GetUser(id uint64) (*User, error) {
        //Some error ocurred
        return nil, err
    }

  [1]: https://golang.org/ref/spec#Program_initialization_and_execution

