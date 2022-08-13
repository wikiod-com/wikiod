---
title: "Iota"
slug: "iota"
draft: false
images: []
weight: 9704
type: docs
toc: true
---

Iota provides a way of declaring numeric constants from a starting value that grows monotonically. Iota can be used to declare bitmasks which are often used in system and network programming and other lists of constants with related values.

The `iota` identifier is used to assign values to lists of constants. When iota is used in a list it starts with a value of zero, and increments by one for each value in the list of constants and is reset on each `const` keyword. Unlike the enumerations of other languages, iota can be used in expressions (eg. `iota + 1`) which allows for greater flexibility.

## Using iota in an expression
`iota` can be used in expressions, so it can also be used to assign values other than simple incrementing integers starting from zero. To create constants for SI units, use this example from [Effective Go][1]:

    type ByteSize float64

    const (
        _           = iota // ignore first value by assigning to blank identifier
        KB ByteSize = 1 << (10 * iota)
        MB
        GB
        TB
        PB
        EB
        ZB
        YB
    )


  [1]: https://golang.org/doc/effective_go.html#initialization

## Simple use of iota
To create a list of constants - assign `iota` value to each element:

    const (
      a = iota // a = 0
      b = iota // b = 1
      c = iota // c = 2
    )

To create a list of constants in a shortened way - assign `iota` value to the first element:

    const (
      a = iota // a = 0
      b        // b = 1
      c        // c = 2
    )

## Use of iota in an expression list
Because `iota` is incremented after each [`ConstSpec`](https://golang.org/ref/spec#ConstSpec), values within the same expression list will have the same value for `iota`:

    const (
        bit0, mask0 = 1 << iota, 1<<iota - 1  // bit0 == 1, mask0 == 0
        bit1, mask1                           // bit1 == 2, mask1 == 1
        _, _                                  // skips iota == 2
        bit3, mask3                           // bit3 == 8, mask3 == 7
    )

This example was taken from the [Go Spec](https://golang.org/ref/spec#Iota) (CC-BY 3.0).

## Skipping values
The value of `iota` is still incremented for every entry in a constant list even if iota is not used:

    const ( // iota is reset to 0
        a = 1 << iota  // a == 1
        b = 1 << iota  // b == 2
        c = 3          // c == 3  (iota is not used but still incremented)
        d = 1 << iota  // d == 8
    )

it will also be incremented even if no constant is created at all, meaning the empty identifier can be used to skip values entirely:

    const (
      a = iota // a = 0
      _        // iota is incremented
      b        // b = 2
    )

The first code block was taken from the [Go Spec](https://golang.org/ref/spec#Iota) (CC-BY 3.0).



## Use of iota in a bitmask
Iota can be very useful when creating a bitmask. For instance, to represent the state of a network connection which may be secure, authenticated, and/or ready, we might create a bitmask like the following:

    const (
        Secure = 1 << iota // 0b001
        Authn              // 0b010
        Ready              // 0b100
    )
    
    ConnState := Secure|Authn // 0b011: Connection is secure and authenticated, but not yet Ready

## Use of iota in const
This is an enumeration for const creation. Go compiler starts iota from 0 and increments by one for each following constant.  The value is determined at compile time rather than run time. Because of this we can't apply iota to expressions which are evaluated at run time. 

Program to use iota in const

    package main
    
    import "fmt"
    
    const (
        Low = 5 * iota
        Medium
        High
    )
    
    func main() {
        // Use our iota constants.
        fmt.Println(Low)
        fmt.Println(Medium)
        fmt.Println(High)
    }

Try it in [Go Playground][1]


  [1]: https://play.golang.org/p/jyJEzyZSi6

