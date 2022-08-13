---
title: "Structs"
slug: "structs"
draft: false
images: []
weight: 9668
type: docs
toc: true
---

Structs are sets of various variables packed together. The struct itself is only a _package_ containing variables and making them easily accessible.

Unlike in C, Go's structs can have methods attached to them. It also allows them to implement interfaces. That makes Go's structs similar to objects, but they are (probably intentionally) missing some major features known in object oriented languages like inheritance.

## Methods
Struct methods are very similar to functions:

    type User struct {
        name string
    }

    func (u User) Name() string {
        return u.name
    }

    func (u *User) SetName(newName string) {
        u.name = newName
    }

The only difference is the addition of the method receiver. It may be declared either as an instance of the type or a pointer to an instance of the type. Since `SetName()` mutates the instance, the receiver must be a pointer in order to effect a permanent change in the instance.

For example:

    package main
    
    import "fmt"
    
    type User struct {
        name string
    }
    
    func (u User) Name() string {
        return u.name
    }
    
    func (u *User) SetName(newName string) {
        u.name = newName
    }
    
    func main() {
        var me User
    
        me.SetName("Slim Shady")
        fmt.Println("My name is", me.Name())
    }

[Go Playground][1]


  [1]: https://play.golang.org/p/I5e3yOaRcI

## Anonymous struct
It is possible to create an anonymous struct:

    data := struct {
        Number int 
        Text   string
    } { 
        42, 
        "Hello world!",
    }

Full example:

    package main
    
    import (
        "fmt"
    )
    
    func main() {
        data := struct {Number int; Text string}{42, "Hello world!"} // anonymous struct
        fmt.Printf("%+v\n", data)
    }
[play it on playground](https://play.golang.org/p/atpNnP5wE_)

## Composition and Embedding
Composition provides an alternative to inheritance. A struct may include another type by name in its declaration:

    type Request struct {
        Resource string
    }

    type AuthenticatedRequest struct {
        Request
        Username, Password string
    }

In the example above, `AuthenticatedRequest` will contain four public members: `Resource`, `Request`, `Username`, and `Password`.

Composite structs can be instantiated and used the same way as normal structs:

    func main() {
        ar := new(AuthenticatedRequest)
        ar.Resource = "example.com/request"
        ar.Username = "bob"
        ar.Password = "P@ssw0rd"
        fmt.Printf("%#v", ar)
    }

[play it on playground](https://play.golang.org/p/MfBhvhNMa-)

# Embedding

In the previous example, `Request` is an embedded field. Composition can also be achieved by embedding a different type. This is useful, for example, to decorate a Struct with more functionality. For example, continuing with the Resource example, we want a function that formats the content of the Resource field to prefix it with `http://` or `https://`. We have two options: create the new methods on AuthenticatedRequest or **embed** it from a different struct:

    type ResourceFormatter struct {}

    func(r *ResourceFormatter) FormatHTTP(resource string) string {
        return fmt.Sprintf("http://%s", resource)
    }
    func(r *ResourceFormatter) FormatHTTPS(resource string) string {
        return fmt.Sprintf("https://%s", resource)
    }


    type AuthenticatedRequest struct {
        Request
        Username, Password string
        ResourceFormatter
    }
    

And now the main function could do the following:

    func main() {
        ar := new(AuthenticatedRequest)
        ar.Resource = "www.example.com/request"
        ar.Username = "bob"
        ar.Password = "P@ssw0rd"

        println(ar.FormatHTTP(ar.Resource))
        println(ar.FormatHTTPS(ar.Resource))

        fmt.Printf("%#v", ar)
    }

Look that the `AuthenticatedRequest` that has a `ResourceFormatter` embedded struct.

**But** the downside of it is that you cannot access objects outside of your composition. So `ResourceFormatter` cannot access members from `AuthenticatedRequest`.

[play it on playground](https://play.golang.org/p/Ngl3D8UW5I)

## Exported vs. Unexported Fields (Private vs Public)
Struct fields whose names begin with an uppercase letter are exported. All other names are unexported.

    type Account struct {
        UserID      int    // exported
        accessToken string // unexported
    }

Unexported fields can only be accessed by code within the same package. As such, if you are ever accessing a field from a _different_ package, its name needs to start with an uppercase letter.

    package main
    
    import "bank"
    
    func main() {
        var x = &bank.Account{
            UserID: 1,          // this works fine
            accessToken: "one", // this does not work, since accessToken is unexported
        }
    }
    
However, from within the `bank` package, you can access both UserId and accessToken without issue.

The package `bank` could be implemented like this:

    package bank

    type Account struct {
        UserID int
        accessToken string
    }

    func ProcessUser(u *Account) {    
        u.accessToken = doSomething(u) // ProcessUser() can access u.accessToken because 
                                       // it's defined in the same package
    }

## Basic Declaration
A basic struct is declared as follows:

    type User struct {
        FirstName, LastName string
        Email               string
        Age                 int
    }

Each value is called a field. Fields are usually written one per line, with the field's name preceeding its type. Consecutive fields of the same type may be combined, as `FirstName` and `LastName` in the above example.

## Tags
Struct fields can have tags associated with them. These tags can be read by the `reflect` package to get custom information specified about a field by the developer.

    struct Account {
        Username      string `json:"username"`
        DisplayName   string `json:"display_name"`
        FavoriteColor string `json:"favorite_color,omitempty"`
    }

In the above example, the tags are used to change the key names used by the `encoding/json` package when marshaling or unmarshaling JSON.

While the tag can be any string value, it's considered best practice to use space separated `key:"value"` pairs:

    struct StructName {
        FieldName int `package1:"customdata,moredata" package2:"info"`
    }

The struct tags used with the `encoding/xml` and `encoding/json` package are used throughout the standard libarary.

## Struct Literals
A value of a struct type can be written using a *struct literal* that specifies values for its fields.

    type Point struct { X, Y int }
    p := Point{1, 2}

The above example specifies every field in the right order. Which is not useful, because programmers have to remember the exact fields in order. More often, a struct can be initialized by listing some or all of the field names and their corresponding values.

    anim := gif.GIF{LoopCount: nframes}

Omitted fields are set to the zero value for its type.

Note: **The two forms cannot be mixed in the same literal.**

## Making struct copies.
A struct can simply be copied using assignment.

    type T struct {
        I int
        S string
    }

    // initialize a struct
    t := T{1, "one"}
    
    // make struct copy
    u := t // u has its field values equal to t
    
    if u == t { // true
        fmt.Println("u and t are equal") // Prints: "u and t are equal"
    }

In above case, `'t'` and 'u' are now separate objects (struct values).

Since `T` does not contain any reference types (slices, map, channels) as its fields, `t` and `u` above can be modified without affecting each other.

    fmt.Printf("t.I = %d, u.I = %d\n", t.I, u.I) // t.I = 100, u.I = 1

However, if `T` contains a reference type, for example:

    type T struct {
        I  int
        S  string
        xs []int // a slice is a reference type
    }

Then a simple copy by assignment would copy the value of slice type field as well to the new object. This would result in two different objects referring to the same slice object.

    // initialize a struct
    t := T{I: 1, S: "one", xs: []int{1, 2, 3}}
    
    // make struct copy
    u := t // u has its field values equal to t
    
Since both u and t refer to the same slice through their field xs updating a value in the slice of one object would reflect the change in the other.

    // update a slice field in u
    u.xs[1] = 500

    fmt.Printf("t.xs = %d, u.xs = %d\n", t.xs, u.xs)
    // t.xs = [1 500 3], u.xs = [1 500 3]

Hence, extra care must be taken to ensure this reference type property does not produce unintended behavior.

To copy above objects for example, an explicit copy of the slice field could be performed:

    // explicitly initialize u's slice field
    u.xs = make([]int, len(t.xs))
    // copy the slice values over from t
    copy(u.xs, t.xs)
    
    // updating slice value in u will not affect t
    u.xs[1] = 500

    fmt.Printf("t.xs = %d, u.xs = %d\n", t.xs, u.xs)
    // t.xs = [1 2 3], u.xs = [1 500 3]
    


## Empty struct
A struct is a sequence of named elements, called fields, each of which has a name and a type. Empty struct has no fields, like this anonymous empty struct:

    var s struct{}

Or like this named empty struct type:

    type T struct{}

The interesting thing about the empty struct is that, its size is zero (try [The Go Playground][2]):

    fmt.Println(unsafe.Sizeof(s))

This prints `0`, so the empty struct itself takes no memory. so it is good option for quit channel, like (try [The Go Playground][1]):

 

    package main
    
    import (
        "fmt"
        "time"
    )
    
    func main() {
        done := make(chan struct{})
        go func() {
            time.Sleep(1 * time.Second)
            close(done)
        }()
    
        fmt.Println("Wait...")
        <-done
        fmt.Println("done.")
    }


----------



  [1]: https://play.golang.org/p/j3qowmGdmC
  [2]: https://play.golang.org/p/ICQkZn01ng

