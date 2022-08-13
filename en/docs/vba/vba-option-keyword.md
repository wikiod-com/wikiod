---
title: "VBA Option Keyword"
slug: "vba-option-keyword"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Syntax
- Option optionName [value]
- Option Explicit
- Option Compare {Text | Binary | Database}
- Option Private Module
- Option Base {0 | 1}

## Parameters

|Option |Detail|
|-------|------|
|Explicit| *Require variable declaration* in the module it's specified in (ideally all of them); with this option specified, using an undeclared (/mispelled) variable becomes a compilation error.|
|Compare&nbsp;Text| Makes the module's string comparisons be case-insensitive, based on system locale, prioritizing alphabetical equivalency (e.g. "a" = "A").|
|Compare&nbsp;Binary| Default string comparison mode. Makes the module's string comparisons be case sensitive, comparing strings using the binary representation / numeric value of each character (e.g. ASCII).|
|Compare&nbsp;Database|(MS-Access only) Makes the module's string comparisons work the way they would in an SQL statement.|
|Private&nbsp;Module| Prevents the module's `Public` member from being accessed from outside of the project that the module resides in, effectively hiding procedures from the host application (i.e. not available to use as macros or user-defined functions).|
|Option&nbsp;Base&nbsp;0| Default setting. Sets the implicit array lower bound to `0` in a module. When an array is declared without an explicit lower boundary value, `0` will be used.|
|Option&nbsp;Base&nbsp;1| Sets the implicit array lower bound to `1` in a module. When an array is declared without an explicit lower boundary value, `1` will be used.|

It is much easier to control the boundaries of arrays by declaring the boundaries explicitly rather than letting the compiler fall back on an `Option Base {0|1}` declaration. This can be done like so:

    Dim myStringsA(0 To 5) As String '// This has 6 elements (0 - 5)
    Dim myStringsB(1 To 5) As String '// This has 5 elements (1 - 5)
    Dim myStringsC(6 To 9) As String '// This has 3 elements (6 - 9)

## Option Explicit
It is deemed best practice to always use `Option Explicit` in VBA as it forces the developer to declare all their variables before use. This has other benefits too, such as auto-capitalization for declared variable names and IntelliSense.

    Option Explicit

    Sub OptionExplicit()
        Dim a As Integer
        a = 5
        b = 10 '// Causes compile error as 'b' is not declared
    End Sub

Setting **Require Variable Declaration** within the VBE's Tools ► Options ► Editor property page will put the **[Option 
Explicit](https://msdn.microsoft.com/en-us/library/y9341s4f.aspx)** statement at the top of each newly created code sheet. 

[![require_variable_declaration][1]][1]

This will avoid silly coding mistakes like misspellings as well as influencing you to use the correct variable type in the variable declaration. (Some more examples are given at <https://www.wikiod.com/excel-vba/vba-best-practices#ALWAYS Use "Option Explicit">.)

  [1]: http://i.stack.imgur.com/C29RO.png

## Option Base {0 | 1}
`Option Base` is used to declare the default lower bound of **array** elements. It is declared at module level and is valid only for the current module.

By default (and thus if no Option Base is specified), the Base is 0. Which means that the first element of any array declared in the module has an index of 0. 

If `Option Base 1` is specified, the first array element has the index 1 

Example in Base 0 :
-------------------

    Option Base 0
    
    Sub BaseZero()
    
        Dim myStrings As Variant
        
        ' Create an array out of the Variant, having 3 fruits elements
        myStrings = Array("Apple", "Orange", "Peach")
        
        Debug.Print LBound(myStrings) ' This Prints "0"
        Debug.Print UBound(myStrings) ' This print "2", because we have 3 elements beginning at 0 -> 0,1,2
                
        For i = 0 To UBound(myStrings)
        
            Debug.Print myStrings(i) ' This will print "Apple", then "Orange", then "Peach"
        
        Next i

    End Sub

Same Example with Base 1
------------------------

    Option Base 1
    
    Sub BaseOne()
    
        Dim myStrings As Variant
        
        ' Create an array out of the Variant, having 3 fruits elements
        myStrings = Array("Apple", "Orange", "Peach")
        
        Debug.Print LBound(myStrings) ' This Prints "1"
        Debug.Print UBound(myStrings) ' This print "3", because we have 3 elements beginning at 1 -> 1,2,3
                
        For i = 0 To UBound(myStrings)
        
            Debug.Print myStrings(i) ' This triggers an error 9 "Subscript out of range"
        
        Next i

    End Sub

The second example generated a [Subscript out of range (Error 9)][1] at the first loop stage because an attempt to access the index 0 of the array was made, and this index doesn't exists as the module is declared with `Base 1`
 

The correct code with Base 1 is :
---------------------------------

  
        For i = 1 To UBound(myStrings)
        
            Debug.Print myStrings(i) ' This will print "Apple", then "Orange", then "Peach"
        
        Next i

It should be noted that the [Split function](https://msdn.microsoft.com/en-us/library/aa263365.aspx) **always** creates an array with a zero-based element index regardless of any `Option Base` setting. Examples on how to use the **Split** function can be found [here](https://www.wikiod.com/vba/arrays#Use of Split to create an array from a string)

><dl><dt><sup>Split Function</sup></dt><dd><sub>Returns a zero-based, one-dimensional array containing a specified number of substrings.</sub></dd></dl>

In Excel, the `Range.Value` and `Range.Formula` properties for a multi-celled range *always* returns a 1-based 2D Variant array.

Likewise, in ADO, the `Recordset.GetRows` method *always* returns a 1-based 2D array.

One recommended 'best practice' is to always use the [LBound](https://msdn.microsoft.com/en-us/library/t9a7w1ac.aspx) and [UBound](https://msdn.microsoft.com/en-us/library/office/gg278658.aspx) functions to determine the extents of an array.

    'for single dimensioned array
    Debug.Print LBound(arr) & ":" & UBound(arr)
    Dim i As Long
    For i = LBound(arr) To UBound(arr)
        Debug.Print arr(i)
    Next i

    'for two dimensioned array
    Debug.Print LBound(arr, 1) & ":" & UBound(arr, 1)
    Debug.Print LBound(arr, 2) & ":" & UBound(arr, 2)
    Dim i As long, j As Long
    For i = LBound(arr, 1) To UBound(arr, 1)
        For j = LBound(arr, 2) To UBound(arr, 2)
             Debug.Print arr(i, j)
        Next j
    Next i

The `Option Base 1` must be at the top of every code module where an array is created or re-dimensioned if arrays are to be consistently created with an lower boundary of 1.

  [1]: https://msdn.microsoft.com/en-us/library/aa264519.aspx

## Option Compare {Binary | Text | Database}
Option Compare Binary
---

Binary comparison makes all checks for string equality within a module/class case *sensitive*. Technically, with this option, string comparisons are performed using sort order of the binary representations of each character.

>    A < B < E < Z < a < b < e < z

If no Option Compare is specified in a module, Binary is used by default.

    Option Compare Binary
    
    Sub CompareBinary()
    
        Dim foo As String
        Dim bar As String
        
        '// Case sensitive
        foo = "abc"
        bar = "ABC"
        
        Debug.Print (foo = bar) '// Prints "False"
        
        '// Still differentiates accented characters
        foo = "ábc"
        bar = "abc"
        
        Debug.Print (foo = bar) '// Prints "False"
        
        '// "b" (Chr 98) is greater than "a" (Chr 97)
        foo = "a"
        bar = "b"
        
        Debug.Print (bar > foo) '// Prints "True"
        
        '// "b" (Chr 98) is NOT greater than "á" (Chr 225)
        foo = "á"
        bar = "b"
        
        Debug.Print (bar > foo) '// Prints "False"
    
    End Sub

Option Compare Text
---

Option Compare Text makes all string comparisons within a module/class use a case *insensitive* comparison. 

> (A | a) < (B | b) < (Z | z)

    Option Compare Text
    
    Sub CompareText()
    
        Dim foo As String
        Dim bar As String
        
        '// Case insensitivity
        foo = "abc"
        bar = "ABC"
        
        Debug.Print (foo = bar) '// Prints "True"
        
        '// Still differentiates accented characters
        foo = "ábc"
        bar = "abc"
        
        Debug.Print (foo = bar) '// Prints "False"
        
        '// "b" still comes after "a" or "á"
        foo = "á"
        bar = "b"
        
        Debug.Print (bar > foo) '// Prints "True"
    
    End Sub

Option Compare Database
---

Option Compare Database is only available within MS Access. It sets the module/class to use the current database settings to determine whether to use Text or Binary mode.

*Note: The use of this setting is discouraged unless the module is used for writing custom Access UDFs (User defined functions) that should treat text comparisons in the same manner as SQL queries in that database.*

