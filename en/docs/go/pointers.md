---
title: "Pointers"
slug: "pointers"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
 - pointer := &variable // get pointer from variable
 - variable := *pointer // get variable from pointer
 - *pointer = value // set value from variable through the pointer
 - pointer := new(Struct) // get pointer of new struct

## Basic Pointers
Go supports [pointers](http://en.wikipedia.org/wiki/Pointer_(computer_programming)), allowing you to pass references to values and records within your program.

    package main
    
    import "fmt"
    
    // We'll show how pointers work in contrast to values with
    // 2 functions: `zeroval` and `zeroptr`. `zeroval` has an
    // `int` parameter, so arguments will be passed to it by
    // value. `zeroval` will get a copy of `ival` distinct
    // from the one in the calling function.
    func zeroval(ival int) {
        ival = 0
    }
    
    // `zeroptr` in contrast has an `*int` parameter, meaning
    // that it takes an `int` pointer. The `*iptr` code in the
    // function body then _dereferences_ the pointer from its
    // memory address to the current value at that address.
    // Assigning a value to a dereferenced pointer changes the
    // value at the referenced address.
    func zeroptr(iptr *int) {
        *iptr = 0
    }

Once these functions are defined, you can do the following:

    func main() {
        i := 1
        fmt.Println("initial:", i) // initial: 1
    
        zeroval(i)
        fmt.Println("zeroval:", i) // zeroval: 1
        // `i` is still equal to 1 because `zeroval` edited
        // a "copy" of `i`, not the original.

        // The `&i` syntax gives the memory address of `i`,
        // i.e. a pointer to `i`. When calling `zeroptr`,
        // it will edit the "original" `i`.
        zeroptr(&i)
        fmt.Println("zeroptr:", i) // zeroptr: 0
    
        // Pointers can be printed too.
        fmt.Println("pointer:", &i) // pointer: 0x10434114
    }

[Try this code](https://play.golang.org/p/KdE4TBbUL2)

## Pointer v. Value Methods
# Pointer Methods

Pointer methods can be called even if the variable is itself not a pointer.

According to the [Go Spec](https://golang.org/ref/spec#Method_values),

>  . . . a reference to a non-interface method with a pointer receiver using an addressable value will automatically take the address of that value: `t.Mp` is equivalent to `(&t).Mp`.

You can see this in this example:

```
package main

import "fmt"

type Foo struct {
    Bar int
}

func (f *Foo) Increment() {
    f.Bar += 1
}

func main() {
    var f Foo

    // Calling `f.Increment` is automatically changed to `(&f).Increment` by the compiler.
    f = Foo{}
    fmt.Printf("f.Bar is %d\n", f.Bar)
    f.Increment()
    fmt.Printf("f.Bar is %d\n", f.Bar)
    
    // As you can see, calling `(&f).Increment` directly does the same thing.
    f = Foo{}
    fmt.Printf("f.Bar is %d\n", f.Bar)
    (&f).Increment()
    fmt.Printf("f.Bar is %d\n", f.Bar)
}
```
**[Play it](https://play.golang.org/p/jlQLrSnH-E)**

# Value Methods

Similarly to pointer methods, value methods can be called even if the variable is itself not a value.

According to the [Go Spec](https://golang.org/ref/spec#Method_values),

>  . . . a reference to a non-interface method with a value receiver using a pointer will automatically dereference that pointer: `pt.Mv` is equivalent to `(*pt).Mv`.

You can see this in this example:

```
package main

import "fmt"

type Foo struct {
    Bar int
}

func (f Foo) Increment() {
    f.Bar += 1
}

func main() {
    var p *Foo

    // Calling `p.Increment` is automatically changed to `(*p).Increment` by the compiler.
    // (Note that `*p` is going to remain at 0 because a copy of `*p`, and not the original `*p` are being edited)
    p = &Foo{}
    fmt.Printf("(*p).Bar is %d\n", (*p).Bar)
    p.Increment()
    fmt.Printf("(*p).Bar is %d\n", (*p).Bar)
    
    // As you can see, calling `(*p).Increment` directly does the same thing.
    p = &Foo{}
    fmt.Printf("(*p).Bar is %d\n", (*p).Bar)
    (*p).Increment()
    fmt.Printf("(*p).Bar is %d\n", (*p).Bar)
}
```
**[Play it](https://play.golang.org/p/Efc0IVgzh8)**

----------

To learn more about pointer and value methods, visit the [Go Spec section on Method Values](https://golang.org/ref/spec#Method_values), or see the [Effective Go section about Pointers v. Values](https://golang.org/doc/effective_go.html#pointers_vs_values).

----------

_Note 1: The parenthesis (`()`) around `*p` and `&f` before selectors like `.Bar` are there for grouping purposes, and must be kept._

_Note 2: Although pointers can be converted to values (and vice-versa) when they are the receivers for a method, they are_ not _automattically converted to eachother when they are arguments inside of a function._

## Dereferencing Pointers
Pointers can be **dereferenced** by adding an asterisk * before a pointer.

    package main
    
    import (
        "fmt"
    )
    
    type Person struct {
        Name string
    }
    
    func main() {
        c := new(Person) // returns pointer
        c.Name = "Catherine"
        fmt.Println(c.Name) // prints: Catherine
        d := c
        d.Name = "Daniel"
        fmt.Println(c.Name) // prints: Daniel
        // Adding an Asterix before a pointer dereferences the pointer
        i := *d
        i.Name = "Ines"
        fmt.Println(c.Name) // prints: Daniel
        fmt.Println(d.Name) // prints: Daniel
        fmt.Println(i.Name) // prints: Ines
    }

## Slices are Pointers to Array Segments
Slices are **pointers** to arrays, with the length of the segment, and its capacity. They behave as pointers, and assigning their value to another slice, will assign the memory address. To **copy** a slice value to another, use the built-in **copy** function: `func copy(dst, src []Type) int` (returns the amount of items copied).

    package main
    
    import (
        "fmt"
    )
    
    func main() {
        x := []byte{'a', 'b', 'c'}
        fmt.Printf("%s", x)       // prints: abc
        y := x
        y[0], y[1], y[2] = 'x', 'y', 'z'
        fmt.Printf("%s", x)       // prints: xyz
        z := make([]byte, len(x))
        // To copy the value to another slice, but 
        // but not the memory address use copy:
        _ = copy(z, x)            // returns count of items copied
        fmt.Printf("%s", z)       // prints: xyz
        z[0], z[1], z[2] = 'a', 'b', 'c'
        fmt.Printf("%s", x)       // prints: xyz
        fmt.Printf("%s", z)       // prints: abc
    }

## Simple Pointers
    
    func swap(x, y *int) {
      *x, *y = *y, *x
    }
    
    func main() {
      x := int(1)
      y := int(2)
      // variable addresses
      swap(&x, &y)
      fmt.Println(x, y)
    }

