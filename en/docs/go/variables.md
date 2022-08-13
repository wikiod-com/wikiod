---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

## Syntax
- var x int   // declare variable x with type int
- var s string // declare variable s with type string
- x = 4 // define x value
- s = "foo" // define s value
- y := 5 // declare and define y inferring its type to int
- f := 4.5 // declare and define f inferring its type to float64
- b := "bar" // declare and define b inferring its type to string



## Basic Variable Declaration
Go is a statically typed language, meaning you generally have to declare the type of the variables you are using.

    // Basic variable declaration. Declares a variable of type specified on the right.
    // The variable is initialized to the zero value of the respective type.
    var x int
    var s string
    var p Person // Assuming type Person struct {}

    // Assignment of a value to a variable
    x = 3

    // Short declaration using := infers the type
    y := 4

    u := int64(100)    // declare variable of type int64 and init with 100 
    var u2 int64 = 100 // declare variable of type int64 and init with 100


## Multiple Variable Assignment
In Go, you can declare multiple variables at the same time.

    // You can declare multiple variables of the same type in one line
    var a, b, c string

    var d, e string = "Hello", "world!"

    // You can also use short declaration to assign multiple variables
    x, y, z := 1, 2, 3

    foo, bar := 4, "stack" // `foo` is type `int`, `bar` is type `string`

If a function returns multiple values, you can also assign values to variables based on the function's return values.

    func multipleReturn() (int, int) {
        return 1, 2
    }

    x, y := multipleReturn() // x = 1, y = 2

    func multipleReturn2() (a int, b int) {
        a = 3
        b = 4
        return
    }

    w, z := multipleReturn2() // w = 3, z = 4

## Blank Identifier
Go will throw an error when there is a variable that is unused, in order to encourage you to write better code. However, there are some situations when you really don't need to use a value stored in a variable. In those cases, you use a "blank identifier" `_` to assign and discard the assigned value.

A blank identifier can be assigned a value of any type, and is most commonly used in functions that return multiple values.

**Multiple Return Values**

```
func SumProduct(a, b int) (int, int) {
    return a+b, a*b
}

func main() {
    // I only want the sum, but not the product
    sum, _ := SumProduct(1,2) // the product gets discarded
    fmt.Println(sum) // prints 3
}
```

**Using `range`**

```
func main() {

    pets := []string{"dog", "cat", "fish"}

    // Range returns both the current index and value
    // but sometimes you may only want to use the value
    for _, pet := range pets {
        fmt.Println(pet)
    }

}
```

## Checking a variable's type
There are some situations where you won't be sure what type a variable is when it is returned from a function. You can always check a variable's type by using `var.(type)` if you are unsure what type it is:

```
x := someFunction() // Some value of an unknown type is stored in x now

switch x := x.(type) {
    case bool:
        fmt.Printf("boolean %t\n", x)             // x has type bool
    case int:
        fmt.Printf("integer %d\n", x)             // x has type int
    case string:
        fmt.Printf("pointer to boolean %s\n", x) // x has type string
    default:
        fmt.Printf("unexpected type %T\n", x)     // %T prints whatever type x is
}
```

