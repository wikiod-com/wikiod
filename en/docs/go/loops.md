---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

As one of the most basic functions in programming, loops are an important piece to nearly every programming language. Loops enable developers to set certain portions of their code to repeat through a number of loops which are referred to as iterations. This topic covers using multiple types of loops and applications of loops in Go.

## Basic Loop
`for` is the only loop statement in go, so a basic loop implementation could look like this:

    // like if, for doesn't use parens either.
    // variables declared in for and if are local to their scope.
    for x := 0; x < 3; x++ { // ++ is a statement.
        fmt.Println("iteration", x)
    }

    // would print:
    // iteration 0
    // iteration 1
    // iteration 2

## Conditional loop
The `for` keyword is also used for conditional loops, traditionally `while` loops in other programming languages. 

    package main
    
    import (
        "fmt"
    )
    
    func main() {
        i := 0
        for i < 3 { // Will repeat if condition is true
            i++
            fmt.Println(i)
        }
    }

[play it on playground](https://play.golang.org/p/18NqQo3PA6)

Will output:

    1
    2
    3

**infinite loop:**

    for {
        // This will run until a return or break.
    }

## Different Forms of For Loop
**Simple form using one variable:**

    for i := 0; i < 10; i++ {
        fmt.Print(i, " ")
    }

**Using two variables (or more):**

    for i, j := 0, 0; i < 5 && j < 10; i, j = i+1, j+2 {
        fmt.Println(i, j)
    }

**Without using initialization statement:**

    i := 0
    for ; i < 10; i++ {
        fmt.Print(i, " ")
    }

**Without a test expression:**

    for i := 1; ; i++ {
        if i&1 == 1 {
            continue
        }
        if i == 22 {
            break
        }
        fmt.Print(i, " ")
    }
**Without increment expression:**  

    for i := 0; i < 10; {
        fmt.Print(i, " ")
        i++
    }

**When all three initialization, test and increment expressions are removed, the loop becomes infinite:**

    i := 0
    for {
        fmt.Print(i, " ")
        i++
        if i == 10 {
            break
        }
    }
**This is an example of infinite loop with counter initialized with zero:**

    for i := 0; ; {
        fmt.Print(i, " ")
        if i == 9 {
            break
        }
        i++
    }
**When just the test expression is used (acts like a typical while loop):**

    i := 0
    for i < 10 {
        fmt.Print(i, " ")
        i++
    }

**Using just increment expression:**

    i := 0
    for ; ; i++ {
        fmt.Print(i, " ")
        if i == 9 {
            break
        }
    }

**Iterate over a range of values using index and value:**

    ary := [5]int{0, 1, 2, 3, 4}
    for index, value := range ary {
        fmt.Println("ary[", index, "] =", value)
    }

**Iterate over a range using just index:**

    for index := range ary {
        fmt.Println("ary[", index, "] =", ary[index])
    }

**Iterate over a range using just index:**

    for index, _ := range ary {
        fmt.Println("ary[", index, "] =", ary[index])
    }

**Iterate over a range using just value:**

    for _, value := range ary {
        fmt.Print(value, " ")
    }

**Iterate over a range using key and value for map (may not be in order):**

    mp := map[string]int{"One": 1, "Two": 2, "Three": 3}
    for key, value := range mp {
        fmt.Println("map[", key, "] =", value)
    }

**Iterate over a range using just key for map (may be not in order):**

    for key := range mp {
        fmt.Print(key, " ") //One Two Three
    }

**Iterate over a range using just key for map (may be not in order):**

    for key, _ := range mp {
        fmt.Print(key, " ") //One Two Three
    }

**Iterate over a range using just value for map (may be not in order):**

    for _, value := range mp {
        fmt.Print(value, " ") //2 3 1
    }

**Iterate over a range for channels (exits if the channel is closed):**

    ch := make(chan int, 10)
    for i := 0; i < 10; i++ {
        ch <- i
    }
    close(ch)
    
    for i := range ch {
        fmt.Print(i, " ")
    }

**Iterate over a range for string (gives Unicode code points):**

    utf8str := "B = \u00b5H" //B = µH
    for _, r := range utf8str {
        fmt.Print(r, " ") //66 32 61 32 181 72
    }
    fmt.Println()
    for _, v := range []byte(utf8str) {
        fmt.Print(v, " ") //66 32 61 32 194 181 72
    }
    fmt.Println(len(utf8str)) //7
as you see `utf8str` has 6 runes (Unicode code points) and 7 bytes.  




## Timed loop
    package main
    
    import(
        "fmt"
        "time"
    )
    
    func main() {
        for _ = range time.Tick(time.Second * 3) {
            fmt.Println("Ticking every 3 seconds")
        }
    }

## Break and Continue
Breaking out of the loop and continuing to the next iteration is also supported in Go, like in many other languages:

    for x := 0; x < 10; x++ { // loop through 0 to 9
        if x < 3 { // skips all the numbers before 3
            continue
        } 
        if x > 5 { // breaks out of the loop once x == 6
            break
        }
        fmt.Println("iteration", x)
    }
    
    // would print:
    // iteration 3
    // iteration 4
    // iteration 5

The `break` and `continue` statements additionally accept an optional label, used to identify outer loops to target with the statement:

    OuterLoop:
    for {
        for {
            if allDone() {
                break OuterLoop
            }
            if innerDone() {
                continue OuterLoop
            }
            // do something
        }
    }



