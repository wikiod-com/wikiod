---
title: "Object Oriented Programming"
slug: "object-oriented-programming"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Interface can't be implemented with pointer receivers because `*User` is not `User`

## Structs
Go supports user defined types in the form of structs and type aliases. structs are composite types, the component pieces of data that constitute the struct type are called *fields*. a field has a type and a name which must be unqiue.

    package main

    type User struct {
        ID uint64
        FullName string
        Email    string
    }

    func main() {
        user := User{
            1,
            "Zelalem Mekonen",
            "zola.mk.27@gmail.com",
        }

        fmt.Println(user) // {1 Zelalem Mekonen zola.mk.27@gmail.com}
    }

this is also a legal syntax for definining structs

    type User struct {
        ID uint64
        FullName, Email string
    }

    user := new(User)

    user.ID = 1
    user.FullName = "Zelalem Mekonen"
    user.Email = "zola.mk.27@gmail.com"

## Methods
In Go a method is 

> a function that acts on a variable of a certain type, called the receiver

the receiver can be anything, not only `structs` but even a `function`, alias types for built in types such as `int`, `string`, `bool` can have a method, an exception to this rule is that `interfaces`(discussed later) cannot have methods, since an interface is an abstract definition and a method is an implementation, trying it generate a compile error.

combining `structs` and `methods` you can get a close eqivalent of a `class` in Object Oriented programming.

a method in Go has the following signature

`func (name receiverType) methodName(paramterList) (returnList) {}`

    package main

    type Admin struct {
        Username, Password string
    }

    func (admin Admin) Delete() {
        fmt.Println("Admin Deleted")
    }

    type User struct {
        ID uint64
        FullName, Email string
        Admin
    }

    func (user User) SendEmail(email string) {
        fmt.Printf("Email sent to: %s\n", user.Email)
    } 

    func main() {
        admin := Admin{
            "zola",
            "supersecretpassword",
        }
    
        user := User{
            1,
            "Zelalem Mekonen",
            "zola.mk.27@gmail.com",
            admin,
        }

        user.SendEmail("Hello") // Email sent to: zola.mk.27@gmail.com

        admin.Delete() // Admin Deleted
    }

## Interface & Polymorphism
Interfaces provide a way to specify the behaviour of an object, if something can do this then it can be used here. an interface defines a set of methods, but these methods do not contain code as they are abstract or the implemntation is left to the user of the interface. unlike most Object Oriented languages interfaces can contain variables in Go.

Polymorphism is the essence of object-oriented programming: the ability to treat objects of different types uniformly as long as they adhere to the same interface. Go interfaces provide this capability in a very direct and intuitive way

    package main

    type Runner interface {
        Run()
    }

    type Admin struct {
        Username, Password string
    }

    func (admin Admin) Run() {
        fmt.Println("Admin ==> Run()");
    }

    type User struct {
        ID uint64
        FullName, Email string
    }

    func (user User) Run() {
        fmt.Println("User ==> Run()")
    }

    // RunnerExample takes any type that fullfils the Runner interface
    func RunnerExample(r Runner) {
        r.Run()
    }

    func main() {
        admin := Admin{
            "zola",
            "supersecretpassword",
        }

        user := User{
            1,
            "Zelalem Mekonen",
            "zola.mk.27@gmail.com",
        }

        RunnerExample(admin)

        RunnerExample(user)
        
    }

## Embedded structs
because a struct is also a data type, it can be used as an anonymous field, the outer struct can directly access the fields of the embedded struct even if the struct came from a diffrent package. this behaviour provides a way to derive some or all of your implementation from another type or a set of types.

    package main

    type Admin struct {
        Username, Password string
    }

    type User struct {
        ID uint64
        FullName, Email string
        Admin // embedded struct
    }

    func main() {
        admin := Admin{
            "zola",
            "supersecretpassword",
        }

        user := User{
            1,
            "Zelalem Mekonen",
            "zola.mk.27@gmail.com",
            admin,
        }

        fmt.Println(admin) // {zola supersecretpassword}

        fmt.Println(user) // {1 Zelalem Mekonen zola.mk.27@gmail.com {zola supersecretpassword}}

        fmt.Println(user.Username) // zola

        fmt.Println(user.Password) // supersecretpassword
    }

## Pointer Vs Value receiver
the receiver of a method is usually a pointer for performance reason because we wouldn't make a copy of the instance, as it would be the case in value receiver, this is especially true if the receiver type is a struct. anoter reason to make the receiver type a pointer would be so we could modify the data the receiver points to.

a value receiver is used to avoid modification of the data the receiver contains, a vaule receiver may cause a performance hit if the receiver is a large struct.

    package main

    type User struct {
        ID uint64
        FullName, Email string
    }

    // We do no require any special syntax to access field because receiver is a pointer
    func (user *User) SendEmail(email string) {
        fmt.Printf("Sent email to: %s\n", user.Email)
    }    

    // ChangeMail will modify the users email because the receiver type is a ponter
    func (user *User) ChangeEmail(email string) {
        user.Email = email;
    }

    func main() {
        user := User{
            1,
            "Zelalem Mekonen",
            "zola.mk.27@gmail.com",
        }

        user.SendEmail("Hello") // Sent email to: zola.mk.27@gmail.com

        user.ChangeEmail("zola@gmail.com")

        fmt.Println(user.Email) // zola@gmail.com
    }

