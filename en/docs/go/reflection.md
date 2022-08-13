---
title: "Reflection"
slug: "reflection"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

The [`reflect` docs](https://golang.org/pkg/reflect/) are a great reference. In general computer programming, **reflection** is ability of a program to **examine** the structure and behavior of **itself** at **`runtime`**. 

Based on its strict `static type` system [Go](https://golang.org/) lang has some rules ([laws of reflection](https://blog.golang.org/laws-of-reflection)) 

## Structs
<!-- language: lang-go -->
<pre><code>
import "reflect"

type S struct {
    A int
    b string
}

func (s *S) String() { return s.b }

s := &S{
    A: 5,
    b: "example",
}

indirect := reflect.ValueOf(s) // effectively a pointer to an S
value := indirect.Elem()       // this is addressable, since we've derefed a pointer

value.FieldByName("A").Interface() // 5
value.Field(2).Interface()         // "example"

value.NumMethod()    // 0, since String takes a pointer receiver
indirect.NumMethod() // 1

indirect.Method(0).Call([]reflect.Value{})              // "example"
indirect.MethodByName("String").Call([]reflect.Value{}) // "example"
</code></pre>

## Slices
<!-- language: lang-go -->
<pre><code>
import "reflect"

s := []int{1, 2, 3}

value := reflect.ValueOf(s)

value.Len()                // 3
value.Index(0).Interface() // 1
value.Type().Kind()        // reflect.Slice
value.Type().Elem().Name() // int

value.Index(1).CanAddr()   // true -- slice elements are addressable
value.Index(1).CanSet()    // true -- and settable
value.Index(1).Set(5)

typ := reflect.SliceOf(reflect.TypeOf("example"))
newS := reflect.MakeSlice(typ, 0, 10) // an empty []string{} with capacity 10
</code></pre>

## reflect.Value.Elem()
<!-- language: lang-go -->
<pre><code>
import "reflect"

// this is effectively a pointer dereference

x := 5
ptr := reflect.ValueOf(&x)
ptr.Type().Name() // *int
ptr.Type().Kind() // reflect.Ptr
ptr.Interface()   // [pointer to x]
ptr.Set(4)        // panic

value := ptr.Elem() // this is a deref
value.Type().Name() // int
value.Type().Kind() // reflect.Int
value.Set(4)        // this works
value.Interface()   // 4
</code></pre>

## Type of value - package "reflect"
reflect.TypeOf  can be used to check the type of variables when comparing
    
    package main
        
        import (
            "fmt"
            "reflect"
        )
        type Data struct {
         a int
        }
        func main() {
            s:="hey dude"
            fmt.Println(reflect.TypeOf(s))
            
            D := Data{a:5}
            fmt.Println(reflect.TypeOf(D))
            
        }

Output :  
string  
main.Data



## Basic reflect.Value Usage
<!-- language: lang-go -->
<pre><code>
import "reflect"

value := reflect.ValueOf(4)

// Interface returns an interface{}-typed value, which can be type-asserted
value.Interface().(int) // 4

// Type gets the reflect.Type, which contains runtime type information about
// this value
value.Type().Name() // int

value.SetInt(5) // panics -- non-pointer/slice/array types are not addressable

x := 4
reflect.ValueOf(&x).Elem().SetInt(5) // works
</code></pre>

