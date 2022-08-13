---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Variable types
There are different variable types for different purposes. In Visual Basic 6 the following variable types are available:

* Array
* Boolean
* Byte
* Currency
* Date
* Double
* Integer
* Long
* Single
* String
* Variant

You declare a variable by using the `Dim` keyword:

    Dim RandomNumber As Integer

If you do not specify a variable type the variable will default to `Variant`:

    Dim Foo

is equivalent to

    Dim Foo As Variant

## **Boolean** ##  
Boolean is the simplest variable type as it can contain only one of two values: True or False.

    Foo = True
    Bar = False

Booleans can be used to control the flow of code:

    Dim Foo as Boolean
    Foo = True

    If Foo Then
      MsgBox "True"
    Else
      MsgBox "False"
    End If

## **Integer** ##
An integer is a numeric data type and can contain a 16-bit signed value (-32768 to +32767). If you know that a variable will only contain whole numbers (such as 9) and not fractional numbers (such as 5.43), declare it as an integer (or long) datatype.

    Dim RandomNumber As Integer
    RandomNumber = 9

Integers are commonly used as counters in `For...Next` loops:

    Dim Counter As Integer

    For Counter = 0 to 2
      MsgBox Counter
    Next Counter

Trying to assign a value less than -32768 or greater than 32767 to an integer will result in a run-time error:

    Dim MyNumber As Integer
    MyNumber = 40000  'Run-time error '6': Overflow
    
## **String** ##
A string variable can contain an empty text, a character, a word or a text of variable length. The string value must be contained in quotation marks (`"`).

    Dim Fruit as String
    Fruit = "Banana"

If you need quotation marks inside a string literal you use two subsequent quotation marks (`""`).

    Dim Quote as String
    Quote = "Bill says: ""Learn VB!"""


