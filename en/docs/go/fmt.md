---
title: "Fmt"
slug: "fmt"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Stringer
The `fmt.Stringer` interface requires a single method, `String() string` to be satisfied. The string method defines the "native" string format for that value, and is the default representation if the value is provided to any of the `fmt` packages formatting or printing routines.

<!-- language: lang-go -->

    package main
    
    import (
        "fmt"
    )
    
    type User struct {
        Name  string
        Email string
    }
    
    // String satisfies the fmt.Stringer interface for the User type
    func (u User) String() string {
        return fmt.Sprintf("%s <%s>", u.Name, u.Email)
    }
    
    func main() {
        u := User{
            Name:  "John Doe",
            Email: "johndoe@example.com",
        }
    
        fmt.Println(u)
        // output: John Doe <johndoe@example.com>
    }

[`Playground`](https://play.golang.org/p/Cew5S4nl94)

## Basic fmt
Package fmt implements formatted I/O using format *verbs*:

    %v    // the value in a default format
    %T    // a Go-syntax representation of the type of the value
    %s    // the uninterpreted bytes of the string or slice

# Format Functions 

There are **4** main function types in `fmt` and several variations within.

## Print

    fmt.Print("Hello World")        // prints: Hello World
    fmt.Println("Hello World")      // prints: Hello World\n
    fmt.Printf("Hello %s", "World") // prints: Hello World

## Sprint

    formattedString := fmt.Sprintf("%v %s", 2, "words") // returns string "2 words"

## Fprint

    byteCount, err := fmt.Fprint(w, "Hello World") // writes to io.Writer w
   
`Fprint` can be used, inside `http` handlers:

    func handler(w http.ResponseWriter, r *http.Request) {
        fmt.Fprintf(w, "Hello %s!", "Browser")
    }   // Writes: "Hello Browser!" onto http response

## Scan

Scan scans text read from standard input.

    var s string
    fmt.Scanln(&s) // pass pointer to buffer
    // Scanln is similar to fmt.Scan(), but it stops scanning at new line.
    fmt.Println(s) // whatever was inputted

# Stringer Interface

Any value which has a `String()` method implements the `fmt` **inteface** `Stringer`

    type Stringer interface {
            String() string
    }

