---
title: "Interfaces"
slug: "interfaces"
draft: false
images: []
weight: 9842
type: docs
toc: true
---

[Interfaces][1] in Go are just fixed method sets. A type _implicitly_ implements an interface if its method set is a superset of the interface. _There is no declaration of intent._

  [1]: http://golang.org/ref/spec#Interface_types


## Simple interface
In Go, an interface is just a set of methods. We use an interface to specify a behavior of a given object.

    type Painter interface {
        Paint()
    }

The implementing type **need not** declare that it is implementing the interface. It is enough to define methods of the same signature. 

    type Rembrandt struct{}

    func (r Rembrandt) Paint() {
        // use a lot of canvas here 
    }

Now we can use the structure as an interface.

    var p Painter
    p = Rembrandt{}

An interface can be satisfied (or implemented) by an arbitrary number of types. Also a type can implement an arbitrary number of interfaces.

    type Singer interface {
         Sing()
    }

    type Writer interface {
         Write()
    }

    type Human struct{}

    func (h *Human) Sing() {
        fmt.Println("singing")
    }

    func (h *Human) Write() {
        fmt.Println("writing")
    }



    type OnlySinger struct{}
    func (o *OnlySinger) Sing() {
        fmt.Println("singing")
    }

Here, The `Human` struct satisfy both the `Singer` and `Writer` interface, but the `OnlySinger` struct only satisfy `Singer` interface.

__________


**Empty Interface**

There is an empty interface type, that contains no methods. We declare it as `interface{}`. This contains no methods so every `type` satisfies it. Hence empty interface can contain any type value.

    var a interface{}
    var i int = 5
    s := "Hello world"
    
    type StructType struct {
        i, j int
        k string
    }


    // all are valid statements
    a = i
    a = s
    a = &StructType{1, 2, "hello"}

The most common use case for interfaces is to ensure that a variable supports one or more behaviours. By contrast, the primary use case for the empty interface is to define a variable which can hold any value, regardless of its concrete type.

To get these values back as their original types we just need to do
    
    i = a.(int)
    s = a.(string)
    m := a.(*StructType)

or
    
    i, ok := a.(int)
    s, ok := a.(string)
    m, ok := a.(*StructType)

`ok` indicates if the `interface a` is convertible to given type. If it is not possible to cast `ok` will be `false`. 

___________

**Interface Values**

If you declare a variable of an interface, it may store any value type that implements the methods declared by the interface!

If we declare `h` of `interface Singer`, it may store a value of type `Human` or `OnlySinger.` This is because of the fact that they all implement methods specified by the `Singer` interface.

    var h Singer
    h = &human{}

    h.Sing()


## Compile-time check if a type satisfies an interface
Interfaces and implementations (types that implement an interface) are "detached". So it is a rightful question how to check at compile-time if a type implements an interface.

One way to ask the compiler to check that the type `T` implements the interface `I` is by attempting an assignment using the zero value for `T` or pointer to `T`, as appropriate. And we may choose to assign to the [blank identifier][2] to avoid unnecessary garbage:

    type T struct{}

    var _ I = T{}       // Verify that T implements I.
    var _ I = (*T)(nil) // Verify that *T implements I.

If `T` or `*T` does not implement `I`, it will be a compile time error.

This question also appears in the official FAQ: [How can I guarantee my type satisfies an interface?][3]


  [2]: https://golang.org/ref/spec#Blank_identifier
  [3]: https://golang.org/doc/faq#guarantee_satisfies_interface

## Go Interfaces from a Mathematical Aspect
In mathematics, especially *Set Theory*, we have a collection of things which is called *set* and we name those things as *elements*. We show a set with its name like A, B, C, ... or explicitly with putting its member on brace notation: {a, b, c, d, e}. Suppose we have an arbitrary element x and a set Z, The key question is: "How we can understand that x is member of Z or not?". Mathematician answer to this question with a concept: **Characteristic Property** of a set. *Characteristic Property* of a set is an expression which describe set completely. For example we have set of *Natural Numbers* which is {0, 1, 2, 3, 4, 5, ...}. We can describe this set with this expression: {a<sub>n</sub> | a<sub>0</sub> = 0, a<sub>n</sub> = a<sub>n-1</sub>+1}. In last expression a<sub>0</sub> = 0, a<sub>n</sub> = a<sub>n-1</sub>+1 is the characteristic property of set of natural numbers. **If we have this expression, we can build this set completely**. Let describe the set of *even numbers* in this manner. We know that this set is made by this numbers: {0, 2, 4, 6, 8, 10, ...}. With a glance we understand that all of this numbers are also a *natural number*, in other words *if we add some extra conditions to characteristic property of natural numbers, we can build a new expression which describe this set*. So we can describe with this expression: {n | n is a member of natural numbers *and* the reminder of n on 2 is zero}. Now we can create a filter which get the characteristic property of a set and filter some desired elements to return elements of our set. For example if we have a natural number filter, both of natural numbers and even numbers can pass this filter, but if we have a even number filter, then some elements like 3 and 137871 can't pass the filter.

Definition of interface in Go is like defining the characteristic property and mechanism of using interface as an argument of a function is like a filter which detect the element is a member of our desired set or not. Lets describe this aspect with code:

    type Number interface {
        IsNumber() bool // the implementation filter "meysam" from 3.14, 2 and 3
    }
    
    type NaturalNumber interface {
        Number
        IsNaturalNumber() bool // the implementation filter 3.14 from 2 and 3
    }
    
    type EvenNumber interface {
        NaturalNumber
        IsEvenNumber() bool // the implementation filter 3 from 2
    }

The characteristic property of `Number` is all structures that have `IsNumber` method, for `NaturalNumber` is all ones that have `IsNumber` and `IsNaturalNumber` methods and finally for `EvenNumber` is all types which have `IsNumber`, `IsNaturalNumber` and `IsEvenNumber` methods. Thanks to this interpretation of interface, easily we can understand that since `interface{}` doesn't have any characteristic property, accept all types (because it doesn't have any filter for distinguishing between values).

## Determining underlying type from interface
In go it can sometimes be useful to know which underlying type you have been passed. This can be done with a type switch. This assumes we have two structs:
```go
type Rembrandt struct{}

func (r Rembrandt) Paint() {}

type Picasso struct{}

func (r Picasso) Paint() {}
```

That implement the Painter interface:
```go
type Painter interface {
    Paint()
}
```

Then we can use this switch to determine the underlying type:

```go

func WhichPainter(painter Painter) {
    switch painter.(type) {
    case Rembrandt:
        fmt.Println("The underlying type is Rembrandt")
    case Picasso:
        fmt.Println("The underlying type is Picasso")
    default:
        fmt.Println("Unknown type")
    }
}
```

## Type switch
Type switches can also be used to get a variable that matches the type of the case:

    func convint(v interface{}) (int,error) {
        switch u := v.(type) {
        case int:
            return u, nil
        case float64:
            return int(u), nil
        case string:
            return strconv(u)
        default:
            return 0, errors.New("Unsupported type")
        }
    }

## Type Assertion
You can access the real data type of interface with Type Assertion.

    interfaceVariable.(DataType)

Example of struct `MyType` which implement interface `Subber`: 

    package main
 
    import (
        "fmt"
    )
 
    type Subber interface {
        Sub(a, b int) int
    }
 
    type MyType struct {
        Msg string
    }
     
    //Implement method Sub(a,b int) int
    func (m *MyType) Sub(a, b int) int {
        m.Msg = "SUB!!!"
 
        return a - b;
    }
 
    func main() {
        var interfaceVar Subber = &MyType{}
        fmt.Println(interfaceVar.Sub(6,5))
        fmt.Println(interfaceVar.(*MyType).Msg)
    }

Without `.(*MyType)` we wouldn't able to access `Msg` Field. If we try `interfaceVar.Msg` it will show compile error: 

    interfaceVar.Msg undefined (type Subber has no field or method Msg)


