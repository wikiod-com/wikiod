---
title: "Type Providers"
slug: "type-providers"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Using the CSV Type Provider
Given the following CSV file:

    Id,Name
    1,"Joel"
    2,"Adam"
    3,"Ryan"
    4,"Matt"

You can read the data with the following script:

    #r "FSharp.Data.dll"
    open FSharp.Data
    
    type PeopleDB = CsvProvider<"people.csv">
    
    let people = PeopleDB.Load("people.csv") // this can be a URL
    
    let joel = people.Rows |> Seq.head
    
    printfn "Name: %s, Id: %i" joel.Name joel.Id

## Using the WMI Type Provider
The WMI type provider allows you to query WMI services with strong typing. 

To output the results of a WMI query as JSON, 

    open FSharp.Management
    open Newtonsoft.Json

    // `Local` is based off of the WMI available at localhost. 
    type Local = WmiProvider<"localhost">

    let data = 
        [for d in Local.GetDataContext().Win32_DiskDrive -> d.Name, d.Size]

    printfn "%A" (JsonConvert.SerializeObject data)



