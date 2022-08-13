---
title: "Parsing Command Line Arguments And Flags"
slug: "parsing-command-line-arguments-and-flags"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Flags
Go standard library provides package `flag` that helps with parsing flags passed to program.

**Note** that `flag` package doesn't provide usual GNU-style flags. That means that multi-letter flags must be started with single hyphen like this:
`-exampleflag` , not this: `--exampleflag`. GNU-style flags can be done with some 3-rd party package.

    package main
    
    import (
        "flag"
        "fmt"
    )
    
    func main() {
    
        // basic flag can be defined like this:
        stringFlag := flag.String("string.flag", "default value", "here comes usage")
        // after that stringFlag variable will become a pointer to flag value
    
        // if you need to store value in variable, not pointer, than you can
        // do it like:
        var intFlag int
        flag.IntVar(&intFlag, "int.flag", 1, "usage of intFlag")
    
        // after all flag definitions you must call
        flag.Parse()
    
        // then we can access our values
        fmt.Printf("Value of stringFlag is: %s\n", *stringFlag)
        fmt.Printf("Value of intFlag is: %d\n", intFlag)
    
    }

`flag` does help message for us:

    $ ./flags -h
    Usage of ./flags:
      -int.flag int
            usage of intFlag (default 1)
      -string.flag string
            here comes usage (default "default value")
Call with all flags:

    $ ./flags -string.flag test -int.flag 24
    Value of stringFlag is: test
    Value of intFlag is: 24

Call with missing flags:

    $ ./flags
    Value of stringFlag is: default value
    Value of intFlag is: 1



## Command line arguments
Command line arguments parsing is Go is very similar to other languages. In you code you just access slice of arguments where first argument will be the name of program itself.

Quick example:

    package main
    
    import (
        "fmt"
        "os"
    )
    
    func main() {
    
        progName := os.Args[0]
        arguments := os.Args[1:]
    
        fmt.Printf("Here we have program '%s' launched with following flags: ", progName)
    
        for _, arg := range arguments {
            fmt.Printf("%s ", arg)
        }
    
        fmt.Println("")
    }

And output would be:

    $ ./cmd test_arg1 test_arg2
    Here we have program './cmd' launched with following flags: test_arg1 test_arg2

Each argument is just a string. In `os` package it looks like:
`var Args []string`

