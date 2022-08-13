---
title: "mgo"
slug: "mgo"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

mgo (pronounced as mango) is a MongoDB driver for the Go language that implements a rich and well tested selection of features under a very simple API following standard Go idioms.

API Documentation

[https://gopkg.in/mgo.v2][1]


  [1]: https://gopkg.in/mgo.v2

## Example


    package main
    
    import (
            "fmt"
            "log"
            "gopkg.in/mgo.v2"
            "gopkg.in/mgo.v2/bson"
    )
    
    type Person struct {
            Name string
            Phone string
    }
    
    func main() {
            session, err := mgo.Dial("server1.example.com,server2.example.com")
            if err != nil {
                    panic(err)
            }
            defer session.Close()
    
            // Optional. Switch the session to a monotonic behavior.
            session.SetMode(mgo.Monotonic, true)
    
            c := session.DB("test").C("people")
            err = c.Insert(&Person{"Ale", "+55 53 8116 9639"},
                       &Person{"Cla", "+55 53 8402 8510"})
            if err != nil {
                    log.Fatal(err)
            }
    
            result := Person{}
            err = c.Find(bson.M{"name": "Ale"}).One(&result)
            if err != nil {
                    log.Fatal(err)
            }
    
            fmt.Println("Phone:", result.Phone)
    }

