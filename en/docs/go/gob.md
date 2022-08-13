---
title: "gob"
slug: "gob"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Gob is a Go specific serialisation method. it has support for all Go data types except for channels and functions. Gob also encodes the type information into the serialised form, what makes it different from say XML is that it is much more efficient.

The inclusion of type information makes encoding and decoding fairly robust to differences between encoder and decoder.

## How to encode data and write to file with gob?
    package main

    import (
        "encoding/gob"
        "os"
    )

    type User struct {
        Username string
        Password string
    }

    func main() {

        user := User{
            "zola",
            "supersecretpassword",
        }

        file, _ := os.Create("user.gob")

        defer file.Close()

        encoder := gob.NewEncoder(file)

        encoder.Encode(user)

    }


## How to read data from file and decode with go?
    package main

    import (
        "encoding/gob"
        "fmt"
        "os"
    )

    type User struct {
        Username string
        Password string
    }

    func main() {

        user := User{}

        file, _ := os.Open("user.gob")

        defer file.Close()

        decoder := gob.NewDecoder(file)

        decoder.Decode(&user)

        fmt.Println(user)

    }


## How to encode an interface with gob?
    package main

    import (
        "encoding/gob"
        "fmt"
        "os"
    )

    type User struct {
        Username string
        Password string
    }

    type Admin struct {
        Username string
        Password string
        IsAdmin  bool
    }

    type Deleter interface {
        Delete()
    }

    func (u User) Delete() {
        fmt.Println("User ==> Delete()")
    }

    func (a Admin) Delete() {
        fmt.Println("Admin ==> Delete()")
    }

    func main() {

        user := User{
            "zola",
            "supersecretpassword",
        }

        admin := Admin{
            "john",
            "supersecretpassword",
            true,
        }

        file, _ := os.Create("user.gob")

        adminFile, _ := os.Create("admin.gob")

        defer file.Close()

        defer adminFile.Close()

        gob.Register(User{}) // registering the type allows us to encode it

        gob.Register(Admin{}) // registering the type allows us to encode it

        encoder := gob.NewEncoder(file)

        adminEncoder := gob.NewEncoder(adminFile)

        InterfaceEncode(encoder, user)

        InterfaceEncode(adminEncoder, admin)

    }

    func InterfaceEncode(encoder *gob.Encoder, d Deleter) {

        if err := encoder.Encode(&d); err != nil {
            fmt.Println(err)
        }

    }


## How to decode an interface with gob?
    package main

    import (
        "encoding/gob"
        "fmt"
        "log"
        "os"
    )

    type User struct {
        Username string
        Password string
    }

    type Admin struct {
        Username string
        Password string
        IsAdmin  bool
    }

    type Deleter interface {
        Delete()
    }

    func (u User) Delete() {
        fmt.Println("User ==> Delete()")
    }

    func (a Admin) Delete() {
        fmt.Println("Admin ==> Delete()")
    }

    func main() {

        file, _ := os.Open("user.gob")

        adminFile, _ := os.Open("admin.gob")

        defer file.Close()

        defer adminFile.Close()

        gob.Register(User{}) // registering the type allows us to encode it

        gob.Register(Admin{}) // registering the type allows us to encode it

        var admin Deleter

        var user Deleter

        userDecoder := gob.NewDecoder(file)

        adminDecoder := gob.NewDecoder(adminFile)

        user = InterfaceDecode(userDecoder)

        admin = InterfaceDecode(adminDecoder)

        fmt.Println(user)

        fmt.Println(admin)

    }

    func InterfaceDecode(decoder *gob.Decoder) Deleter {

        var d Deleter

        if err := decoder.Decode(&d); err != nil {
            log.Fatal(err)
        }

        return d

    }


