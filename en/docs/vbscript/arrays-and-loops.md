---
title: "Arrays and Loops"
slug: "arrays-and-loops"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## 1. Arrays - Static
    Dim cars(2)
    cars(0) = "Ford"
    cars(1) = "Audi"
    cars(2) = "Prius"

## 2. Arrays - Dynamic
    Dim cars()
    Redim cars(0) 'Give it 1 entry
    Dim tmp
    tmp = "Ford"

    'ubound(arrayvariable) is the count of array size.
    'in this case, it would be 1, since there is 1 entry. 
    cars(ubound(cars)) = tmp 'cars(0)
    Redim preserve cars(ubound(cars)+1)

## 5. Creating an array from a text file.
    Dim cars
    Dim filefullname : filefullname = "C:\testenv\test.txt"
    'If you can, create an instaneous read for text file data for better memory handling.
    'Unless it's a large file and that's impossible. 
    cars = Split(CreateObject("Scripting.FileSystemObject").OpenTextFile(filefullname, 1).ReadAll, vbcrlf)

## 7. For Each loop.
You cannot alter the array's contents through the loop variable because it's a temporary each element is being assigned to.

    Dim cars(2) 'collection of different cars
    Dim trace 'track iteration details
    cars(0) = "Ford"
    cars(1) = "Audi"
    cars(2) = "Prius"
    For Each car in cars
        trace = trace & car & " temporarily changed to "
        car = "Jeep" 'affects car but not the cars array
        trace = trace & car & vbNewLine
    Next

    MsgBox trace 'show what happened during the loop

    Dim jeeps : jeeps = 0
    For Each car in cars
        If car = "Jeep" Then jeeps = jeeps +1
    Next

    MsgBox jeeps & " of the cars are Jeeps."

## 6. For Loop
    Dim i, cars(2)
    cars(0) = "Ford"
    cars(1) = "Audi"
    cars(2) = "Prius"
    For i=0 to ubound(cars)
        If cars(i) = "Audi" Then Exit For
    Next

## 8. Do While Loop
    Dim x, cars
    x = 0
    cars = Split(CreateObject("Scripting.FileSystemObject").OpenTextFile("C:\testenv\example.txt", 1).ReadAll, vbcrlf)
    Do While x < ubound(cars)
        If cars(x) = "Audi" Then Exit Loop
        x = x + 1
    Loop

## 9. Do Until Loop
    Dim copycars(), cars(2), x
    Redim copycars(0)
    x = 0
    cars(0) = "Ford"
    cars(1) = "Audi"
    cars(2) = "Prius"
    Do Until x = ubound(cars)
        copycars(ubound(copycars)) = cars(x)
        redim preserve copycars(ubound(copycars)+1)
        x = x + 1
    Loop
    redim preserve copycars(ubound(copycars)-1) 'trim off the empty last entry

## 3. Arrays - Multi-Dimensional
    Dim mdArray(2,3)
    mdArray(0, 0) = "test1"
    mdArray(0, 1) = "test2"
    mdArray(0, 2) = "test3"
    mdArray(0, 3) = "test4"
    mdArray(1, 0) = "test5"
    mdArray(1, 1) = "test6"
    mdArray(1, 2) = "test7"
    mdArray(1, 3) = "test8"
    mdArray(2, 0) = "test9"
    mdArray(2, 1) = "test10"
    mdArray(2, 2) = "test11"
    mdArray(2, 3) = "test12"

## 4. Arrays - Multi-Dimensional - Dynamic
    Dim mddArray()
    ReDim mddArray(0)
    Dim ti, testinc: testinc = "test": ti = 1
    For i = 0 To 4
        Dim tmpArray(): ReDim tmpArray(0)
        For j = 0 To 3
            tmpArray(UBound(tmpArray)) = testinc & ti
            ti = ti + 1
            ReDim Preserve tmpArray(UBound(tmpArray) + 1)
        Next
        ReDim Preserve tmpArray(UBound(tmpArray) - 1)
        mddArray(i) = tmpArray
        ReDim Preserve mddArray(UBound(mddArray) + 1)
    Next
    ReDim Preserve mddArray(UBound(mddArray) - 1)

