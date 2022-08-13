---
title: "Dictionary Objects"
slug: "dictionary-objects"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Iterate all items in the dictionary
    set oDic = CreateObject("Scripting.Dictionary")
    oDic.add "USA", "United States of America"
    oDic.add "UK", "United Kingdom"
    oDic.add "CAN", "Canada"
    
    For Each obj in oDic.Items
        Msgbox obj
    Next
    Set oDic = Nothing

***Output:**

United States of America

United Kingdom

Canada


## Create dictionary and Add Items to dictionary
    Dim oDic
    Set oDic = CreateObject("Scripting.Dictionary")
    oDic.Add "US", "United States of America"
    oDic.Add "UK", "United Kingdom"



## Check if key Exists in Dictionary
    If oDic.Exists("US") Then
        msgbox "The Key US Exist. The value is " + oDic("US")
    Else
        msgbox "Key Does not exist."
    End If 

## Delete Key/ keys from Dictionary
    set oDic = CreateObject("Scripting.Dictionary")
    oDic.add "USA", "United States of America"
    oDic.add "UK", "United Kingdom"
    oDic.add "CAN", "Canada"
    
    ' Delete only if Key exists
    If oDic.Exists("UK") Then
        oDic.Remove "UK"
    End If    

    ' Delete all keys from Dictionary
    oDic.removeAll 

    Set oDic = Nothing

## Remove Item from Dictionary
    If oDic.Exists("UK") Then
        oDic.remove("UK")
    End If

## Iterate all keys in dictionary
    set oDic = CreateObject("Scripting.Dictionary")
    oDic.add "USA", "United States of America"
    oDic.add "UK", "United Kingdom"
    oDic.add "CAN", "Canada"
    
    For Each obj in oDic.keys
        Msgbox "Key: " & obj & " Value: " & oDic(obj)
    Next
    Set oDic = Nothing

