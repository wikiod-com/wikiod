---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

MSDN Date/Time, String and Numeric Functions

https://msdn.microsoft.com/en-us/library/3ca8tfek(v=vs.84).aspx

## 1. Standard String
In vbscript, an object doesn't necessarily need a designated type. Similar to C#'s var variable. 

    Dim ExampleString1 As String
    Dim ExampleString2

## 2. String Manipulation Basics
    'Base string
    Dim exStr : exStr = " <Head>data</Head> "
    
    'Left
    Dim res: res = Left(exStr,6) 'res now equals " <Head"
    'Right
    Dim res: res = Right(exStr,6) 'res now equals "Head> "
    'Mid
    Dim res: res = Mid(exStr,8,4) 'res now equals "data"
    'Replace
    Dim res: res = Replace("variable", "var", "") 'res now equals "riable"
    'LCase
    Dim res: res = Lcase(exStr) 'res now equals " <head>data</head> "
    'UCase
    Dim res: res = UCase(exStr) 'res now equals " <HEAD>DATA</HEAD> "
    'LTrim
    Dim res: res = LTrim(exStr) 'res now equals "<Head>data</Head> " notice no space on left side
    'RTrim
    Dim res: res = RTrim(exStr) 'res now equals "<Head>data</Head> " notice no space on right side
    'Trim
    Dim res: res = Trim(exStr) 'res now equals "<Head>data</Head>"
    'StrReverse
    Dim res: res = StrReverse(exStr) 'res now equals " >daeH/<atad>daeH< "
    'String
    Dim res: res = String(4,"c") 'res now equals "cccc"


    'StrComp - String Compare, by default, compares the binary of 2 strings.
    'The third parameter allows text comparison, but does not compare case(capitalization). 
    'Binary
    '-1 = if Binary structure of "cars" < "CARS"
    ' 0 = if Binary structure of "cars" = "cars"
    ' 1 = if Binary structure of "CARS" > "cars"
    Dim res: res = StrComp("cars", "CARS") 'res now equals -1
    Dim res: res = StrComp("cars", "cars") 'res now equals 0
    Dim res: res = StrComp("CARS", "cars") 'res now equals 1

    'Text
    '-1 = if Text structure of "cars" < "CARSSS"
    ' 0 = if Text structure of "cars" = "cars"
    ' 1 = if Text structure of "CARSSS" > "cars"
    Dim res: res = StrComp("cars", "CARSSS", 1) 'res now equals -1
    Dim res: res = StrComp("cars", "cars", 1) 'res now equals 0
    Dim res: res = StrComp("CARSSS", "cars", 1) 'res now equals 1

    'Space
    Dim res: res = "I" & Space(1) & "Enjoy" & Space(1) & "Waffles" 'res now equals "I Enjoy Waffles"

    'Instr - Returns position of character or string in the variable.
    Dim res: res = Instr(exStr, ">") ' res now equals 6
    'InstrRev - Returns position of character or string in the variable from right to left. 
    Dim res: res = Instr(exStr, ">") ' res now equals 2

    'Split and Join
    'These are methods that can be used with strings to convert a string to an array
    'or combine an array into a string
    Dim res1 : res1 = Split(exStr, ">")
    'res1(0) = " <Head"
    'res1(1) = "data</Head"
    'res1(2) = " "
    Dim res2 : res2 = Join(res1, ">")
    'res2 now equals " <Head>data</Head> "    

## 3. Searching a String
Given a text file `test.txt`:

    Ford
    Jeep
    Honda
The following script is processing this text file:

    'Read in File Data to an array, separate by newline vb equivalent (vbcrlf)
    Dim car, cars
    Dim filefullname : filefullname = "C:\testenv\test.txt"
    cars = Split(CreateObject("Scripting.FileSystemObject").OpenTextFile(filefullname, 1).ReadAll, vbcrlf)
    
    'Exact Match search. 
    Dim searchstring : searchstring = "Jeep"
    For Each car In cars
        If Car = searchstring Then Exit For
    Next
    
    'Partial Match search
    'Instr returns >0 if any result is found, if none, InStr returns -1
    'The If statement will use -1 = false, >0 = true
    Dim searchstring : searchstring = "Jee"
    Dim position
    For car = 0 To ubound(cars)
        If InStr(cars(car), searchstring) Then 
            position = car
            Exit For
        End If
    Next

## 5. Populating array with  specific text from string via start and end characters.
    'Note: I use this method to extract non-html data in extracted GET telnet results
    'This example effectively grabs every other vehicle that have start and finish
    'characters, which in this case is "/". 
    'Resulting in an array like this:
    'extractedData(0) = "/Jeep"
    'extractedData(1) = "/Ford"
    'extractedData(2) = "/Honda"
    
    
    Dim combined : combined = Join(cars, "/") & "/" & Join(cars, "/")
    'combined now equals Ford/Jeep/Honda/Ford/Jeep/Honda
    
    Dim record, trigger : record = false : trigger = false
    Dim extractedData() : ReDim extractedData(0)
    For I = 1 to len(combined) 'searching the string one character at a time
        If trigger Then 'if I've already started recording values
            If Mid(combined, I, 1) = "/" Then 'End Character is found, stop recording
                record = false
                trigger = false
                ReDim Preserve extractedData(ubound(extractedData)+1) 'Prep next Array Entry
            End If
        Else
            If Mid(combined, I, 1) = "/" Then record = true 'Start recording on start character 
        End If
        If record Then 
            'Increment text on array entry until end variable is found. 
            extractedData(ubound(extractedData)) = extractedData(ubound(extractedData)) & Mid(combined, I, 1) 
            trigger = true
        End If
    Next

## 4. Chaining string manipulation methods together.
    Dim exStr : exStr = " <Head>data</Head> "
    
    Dim res
    res = Ucase(Replace(Mid(exStr, instr(exStr, ">")+1,4), "ata", "ark")) 
    'res now equals DARK
    'instr(exStr, ">") returns 7
    'Mid(" <Head>data</Head> ", 7+1, 4) returns "data"
    'Replace("data",  "ata", "ark") returns "dark"
    'Ucase("dark") returns "DARK"

