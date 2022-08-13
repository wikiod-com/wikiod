---
title: "Records"
slug: "records"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Add member functions to records
    type person = {Name: string; Age: int} with   // Defines person record
        member this.print() =
            printfn "%s, %i" this.Name this.Age

    let user = {Name = "John Doe"; Age = 27} // creates a new person

    user.print() // John Doe, 27

## Basic usage
    type person = {Name: string; Age: int}   // Defines person record

    let user1 = {Name = "John Doe"; Age = 27} // creates a new person
    let user2 = {user1 with Age = 28}         // creates a copy, with different Age
    let user3 = {user1 with Name = "Jane Doe"; Age = 29} //creates a copy with different Age and Name

    let printUser user =
        printfn "Name: %s, Age: %i" user.Name user.Age

    printUser user1 // Name: John Doe, Age: 27
    printUser user2 // Name: John Doe, Age: 28
    printUser user3 // Name: Jane Doe, Age: 29

