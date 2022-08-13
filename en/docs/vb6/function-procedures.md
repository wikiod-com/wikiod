---
title: "Function Procedures"
slug: "function-procedures"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Function is a series of statements enclosed by "Function" and "End Function" statements. 

The Function performs an activity and returns control to the caller. When it returns control, it also returns a value to the calling code. 

You can define a Function in a Class, Structure & Module. By default It is Public. It means, you can call it from anywhere in your application that has access to the class, Structure or Module in which you defined it.


## Syntax
- [Modifiers] Function Name_Of_The_Function [(Arg_List)] As Return_Type  
- [Statements]   
- End Function

- The two Function Modifiers used in this examples are Public & Private. This Modifiers define the scope of the Function.
- Functions with a Private scope can only be called from the source file from where they were defined. In our case it can be called with in the Module. And cannot be called outside the Module. 
- Functions with Public scope can be called both outside and inside the Module. Simply we can say as "We can call it any where in the program". 
- Default Modifier of the Function is Public.
- By default, the function arguments are passed by reference (In a separate topic, this will be explained in detail). 

## Creating & Calling a Function
 This Example using Standard EXE Project With addition of a Module File.
- Create New "Standard EXE" Project. So here, a Form will get added to the Project by default.
- Add a Module File to the Project
- Place a Command Button on the Form
- Create Command Button Click Event.


> Module code

Created two Functions in the Module. One is a Public Function (FnAdd). It takes two Integer arguments val_1 & val_2. It returns an Integer. This Function add the two arguments and return the value to the caller. Before the addition, the two arguments undergo a process in another Function. Which is a Private Function. Characteristic/Rules of the Public & Private Function explained in the Remarks section.

    Public Function FnAdd(val_1 As Integer, val_2 As Integer) As Integer
    
    'Calling private function
    val_1 = FnMultiplyBy5(val_1)
    
    'Calling private function
    val_2 = FnMultiplyBy5(val_2)
    
    'Function return statement
    FnAdd = val_1 + val_2
    
    End Function

Below is the Private function in the Module. It takes one integer arguments val.
It returns an integer. This function multiply a value 5 with the argument and return the result to the caller.
    
    Private Function FnMultiplyBy5(Val As Integer) As Integer
    
    'Function return statement
    FnMultiplyBy5 = Val * 5
    
    End Function

> Form Code

In the Command Button click Event. Here we are calling the Module Public function "FnAdd"

    Private Sub Command1_Click()
    Debug.Print FnAdd(3, 7)
    End Sub

> Result in the Immediate Window

    50

