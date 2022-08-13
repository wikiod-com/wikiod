---
title: "CSV parsing"
slug: "csv-parsing"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Basic usage of Import-Csv
Given the following CSV-file

    String,DateTime,Integer
    First,2016-12-01T12:00:00,30
    Second,2015-12-01T12:00:00,20
    Third,2015-12-01T12:00:00,20

One can import the CSV rows in PowerShell objects using the `Import-Csv` command

    > $listOfRows = Import-Csv .\example.csv
    > $listOfRows
    
    String DateTime            Integer
    ------ --------            -------
    First  2016-12-01T12:00:00 30     
    Second 2015-11-03T13:00:00 20     
    Third  2015-12-05T14:00:00 20 
    
    > Write-Host $row[0].String1
    Third

## Import from CSV and cast properties to correct type
By default, `Import-CSV` imports all values as strings, so to get DateTime- and integer-objects, we need to cast or parse them.

Using `Foreach-Object`:

    > $listOfRows = Import-Csv .\example.csv
    > $listOfRows | ForEach-Object {
        #Cast properties
        $_.DateTime = [datetime]$_.DateTime
        $_.Integer = [int]$_.Integer
    
        #Output object
        $_
    }

Using calculated properties:

    > $listOfRows = Import-Csv .\example.csv
    > $listOfRows | Select-Object String,
        @{name="DateTime";expression={ [datetime]$_.DateTime }},
        @{name="Integer";expression={ [int]$_.Integer }}

Output:

    String DateTime            Integer
    ------ --------            -------
    First  01.12.2016 12:00:00      30
    Second 03.11.2015 13:00:00      20
    Third  05.12.2015 14:00:00      20

