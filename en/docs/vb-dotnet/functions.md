---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The function is just like sub. But function returns a value. A function can accept single or multiple parameters.

## Defining a Function
It's really easy to define the functions.

    Function GetAreaOfARectangle(ByVal Edge1 As Integer, ByVal Edge2 As Integer) As Integer
        Return Edge1 * Edge2
    End Function

----------

    Dim Area As Integer = GetAreaOfARectangle(5, 8)
    Console.Writeline(Area) 'Output: 40



## Defining a Function #2
    Function Age(ByVal YourAge As Integer) As String   
        Select Case YourAge
            Case Is < 18
                Return("You are younger than 18! You are teen!")
            Case 18 to 64
                Return("You are older than 18 but younger than 65! You are adult!")
            Case Is >= 65
                Return("You are older than 65! You are old!")                
        End Select
    End Function

----------

    Console.WriteLine(Age(48)) 'Output: You are older than 18 but younger than 65! You are adult!
    

