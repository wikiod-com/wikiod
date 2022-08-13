---
title: "Methods"
slug: "methods"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
- func (t T) exampleOne(i int) (n int) { return i } // this function will receive copy of struct
- func (t *T) exampleTwo(i int) (n int) { return i }
// this method will receive pointer to struct and will be able to modify it

## Basic methods
Methods in Go are just like functions, except they have *receiver*.

Usually receiver is some kind of struct or type.

    package main
    
    import (
        "fmt"
    )
    
    type Employee struct {
        Name string
        Age  int
        Rank int
    }
    
    func (empl *Employee) Promote() {
        empl.Rank++
    }
    
    func main() {
    
        Bob := new(Employee)
    
        Bob.Rank = 1
        fmt.Println("Bobs rank now is: ", Bob.Rank)
        fmt.Println("Lets promote Bob!")
    
        Bob.Promote()
    
        fmt.Println("Now Bobs rank is: ", Bob.Rank)
    
    }

Output:

    Bobs rank now is:  1
    Lets promote Bob!
    Now Bobs rank is:  2

## Chaining methods
With methods in golang you can do method "chaining" passing pointer to method and returning pointer to the same struct like this:

    package main
    
    import (
        "fmt"
    )
    
    type Employee struct {
        Name string
        Age  int
        Rank int
    }
    
    func (empl *Employee) Promote() *Employee {
        fmt.Printf("Promoting %s\n", empl.Name)
        empl.Rank++
        return empl
    }
    
    func (empl *Employee) SetName(name string) *Employee {
        fmt.Printf("Set name of new Employee to %s\n", name)
        empl.Name = name
        return empl
    }
    
    func main() {
    
        worker := new(Employee)
    
        worker.Rank = 1
    
        worker.SetName("Bob").Promote()
    
        fmt.Printf("Here we have %s with rank %d\n", worker.Name, worker.Rank)
    
    }

Output:

    Set name of new Employee to Bob
    Promoting Bob
    Here we have Bob with rank 2


## Increment-Decrement operators as arguments in Methods
Though Go supports ++ and -- operators and the behaviour is found to be almost similar to c/c++, variables with such operators cannot be passed as argument to function.

        package main
    
        import (
            "fmt"
        )
        
        func abcd(a int, b int) {
         fmt.Println(a," ",b)
        }
        func main() {
            a:=5
            abcd(a++,++a)
        }

Output: syntax error: unexpected ++, expecting comma or )



