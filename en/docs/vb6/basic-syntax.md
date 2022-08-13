---
title: "Basic Syntax"
slug: "basic-syntax"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Select Case Statement
        Dim number As Integer = 8
        Select Case number
            Case 1 To 5
                Debug.WriteLine("Between 1 and 5, inclusive")
                ' The following is the only Case clause that evaluates to True.
            Case 6, 7, 8
                Debug.WriteLine("Between 6 and 8, inclusive")
            Case 9 To 10
                Debug.WriteLine("Equal to 9 or 10")
            Case Else
                Debug.WriteLine("Not between 1 and 10, inclusive")
        End Select

## if / else statement
    If condition Then 
        code to execute if true
    ElseIf condition Then
        code
    Else
        code to execute if conditions are both false
    End If

## for loop
    For I as Integer = 1 To 10 Step 1
        code to execute
    Next

Step is optional and Step 1 is the default. Step tells it how to count, so -1 would have it subtract 1 each time and Step 5 would have it add 5 each time thru the loop. 

In case the loop need to be stopped, then the `Exit For` statement can be used, as in the below example;

    Dim iIndex as integer
    
    For I as Integer = 1 To 10 Step 1
    
    Debug.Print  I     
    iIndex = I * 10

    If iIndex > 90 Then
        Exit For
    End If
    
    Loop

Here, instead of printing 1 to 10, it will stop at 9, since the condition told the process to stop when iIndex reaches 90.


## Do Loop
Another common type of loop in Visual Basic is the `DO loop`, which would run a piece of code continuously until it is told to stop. On the contrary of some other loops whereby indexes are used to stop the process, in this particular loop, it should be told to stop.

A simple example illustrating the loop is as follows

    Dim iIndex1 As Integer
    iIndex1 = 1
       
    Do
       Debug.Print iIndex1 
       iIndex1 = iIndex1 + 1
    
       If iIndex1 = 10 Then
          Exit Do
       End If
    Loop

The above piece of code will take an Index, initialized to 1, and increment it. A `Debug.Print` will help print the index to rack the loop. On each loop, the code will verify if the index has reached 10 and if and only if the condition is true, the `Exit Do` will be executed, which will stop the loop.

