---
title: "Type conversions"
slug: "type-conversions"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Basic Type Conversion
There are two basic styles of type conversion in Go:

    // Simple type conversion
    var x := Foo{}    // x is of type Foo
    var y := (Bar)Foo // y is of type Bar, unless Foo cannot be cast to Bar, then compile-time error occurs.
    // Extended type conversion
    var z,ok := x.(Bar)    // z is of type Bar, ok is of type bool - if conversion succeeded, z has the same value as x and ok is true. If it failed, z has the zero value of type Bar, and ok is false.

## Testing Interface Implementation
As Go uses implicit interface implementation, you will not get a compile-time error if your struct does not implement an interface you had intended to implement. You can test the implementation explicitly using type casting:
    type MyInterface interface {
        Thing()
    }

    type MyImplementer struct {}

    func (m MyImplementer) Thing() {
        fmt.Println("Huzzah!")
    }
    
    // Interface is implemented, no error. Variable name _ causes value to be ignored.
    var _ MyInterface = (*MyImplementer)nil

    type MyNonImplementer struct {}

    // Compile-time error - cannot case because interface is not implemented.
    var _ MyInterface = (*MyNonImplementer)nil

## Implement a Unit System with Types
This example illustrates how Go's type system can be used to implement some unit system.

    package main
    
    import (
        "fmt"
    )
    
    type MetersPerSecond float64
    type KilometersPerHour float64
    
    func (mps MetersPerSecond) toKilometersPerHour() KilometersPerHour {
        return KilometersPerHour(mps * 3.6)
    }
    
    func (kmh KilometersPerHour) toMetersPerSecond() MetersPerSecond {
        return MetersPerSecond(kmh / 3.6)
    }
    
    func main() {
        var mps MetersPerSecond
        mps = 12.5
        kmh := mps.toKilometersPerHour()
        mps2 := kmh.toMetersPerSecond()
        fmt.Printf("%vmps = %vkmh = %vmps\n", mps, kmh, mps2)
    }

[Open in Playground](https://play.golang.org/p/bhtAQWt5ci)


