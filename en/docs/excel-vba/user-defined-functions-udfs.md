---
title: "User Defined Functions (UDFs)"
slug: "user-defined-functions-udfs"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Syntax
1. **Function functionName(argumentVariable As dataType, argumentVariable2 As dataType, Optional argumentVariable3 As dataType) As functionReturnDataType**  
Basic declaration of a function. Every function needs a name, but it does not have to take any arguments. It may take 0 arguments, or it may take a given number of arguments. You may also declare an argument as optional (meaning it does not matter if you supply it when calling the function). It is best practice to supply the variable type for each argument, and likewise, to return what data type the function itself is going to return.

2. **functionName = theVariableOrValueBeingReturned**  
If you're coming from other programming languages, you may be used to the `Return` keyword. This is not used in VBA - instead, we use the function name. You can set it to the contents of a variable or to some directly supplied value. Note that if you did set a data type for the function's return, the variable or data you are supplying this time must be of that data type.

3. **End Function**  
Mandatory. Signifies the end of the `Function` codeblock and must thusly be at the end. The VBE usually supplies this automatically when you create a new function.

A User Defined Function (aka UDF) refers to a task specific function that has been created by the user. It can be called as a worksheet function (ex: `=SUM(...)`) or used to return a value to a running process in a Sub procedure. A UDF returns a value, typically from information passed into it as one or more parameters.

It can be created by :

 1. using VBA .
 2. using Excel C API - By creating an XLL that exports compiled functions to Excel.
 3. using the COM interface.

## Allow full column references without penalty
It's easier to implement some UDFs on the worksheet if full column references can be passed in as parameters. However, due to the explicit nature of coding, any loop involving these ranges may be processing hundreds of thousands of cells that are completely empty. This reduces your VBA project (and workbook) to a frozen mess while unnecessary non-values are processed.

Looping through a worksheet's cells is one of the slowest methods of accomplishing a task but sometimes it is unavoidable. Cutting the work performed down to what is actually required makes perfect sense.

The solution is to truncate the full column or full row references to the [Worksheet.UsedRange property](https://msdn.microsoft.com/en-us/library/office/ff840732.aspx) with the [Intersect method](https://msdn.microsoft.com/en-us/library/office/aa195772.aspx). The following sample will loosely replicate a worksheet's native SUMIF function so the *criteria_range* will also be resized to suit the *sum_range* since each value in the *sum_range* must be accompanied by a value in the *criteria_range*.

The [Application.Caller](https://msdn.microsoft.com/en-us/library/office/ff193687.aspx) for a UDF used on a worksheet is the cell in which it resides. The cell's [.Parent](https://msdn.microsoft.com/en-us/library/office/aa224980.aspx) property is the worksheet. This will be used to define the .UsedRange.

In a Module code sheet:

    Option Explicit
    
    Function udfMySumIf(rngA As Range, rngB As Range, _
                        Optional crit As Variant = "yes")
        Dim c As Long, ttl As Double
        
        With Application.Caller.Parent
            Set rngA = Intersect(rngA, .UsedRange)
            Set rngB = rngB.Resize(rngA.Rows.Count, rngA.Columns.Count)
        End With
        
        For c = 1 To rngA.Cells.Count
            If IsNumeric(rngA.Cells(c).Value2) Then
                If LCase(rngB(c).Value2) = LCase(crit) Then
                    ttl = ttl + rngA.Cells(c).Value2
                End If
            End If
        Next c
        
        udfMySumIf = ttl
    
    End Function
    
><sup>Syntax:</sup><br/>        `=udfMySumIf(*sum_range*, *criteria_range*, [*criteria*])`

[![udf_sumifs][1]][1]

While this is a fairly simplistic example, it adequately demonstrates passing in two full column references (1,048,576 rows each) but only processing 15 rows of data and criteria. 

---
<sub>Linked official MSDN documentation of individual methods and properties courtesy of Microsoft™.</sub>

  [1]: http://i.stack.imgur.com/sgMr4.png

## UDF - Hello World

 1. Open Excel
 2. Open the Visual Basic Editor ( see  https://www.wikiod.com/excel-vba/getting-started-with-excel-vba#Opening the Visual Basic Editor (VBE) )
 3. Add a new module by clicking Insert --> Module :

[![enter image description here][1]][1]

 4. Copy and Paste the following code in the new module :


    Public Function Hello() As String
    'Note: the output of the function is simply the function's name
    Hello = "Hello, World !"
    End Function

 
To obtain :

[![enter image description here][2]][2]
       

5. Go back to your workbook and type "=Hello()" into a cell to see the "Hello World".

[![enter image description here][3]][3]



  [1]: http://i.stack.imgur.com/0KhKM.png
  [2]: http://i.stack.imgur.com/1r1E7.png
  [3]: http://i.stack.imgur.com/PFQsX.png

## Count Unique values in Range
    Function countUnique(r As range) As Long
        'Application.Volatile False ' optional
        Set r = Intersect(r, r.Worksheet.UsedRange) ' optional if you pass entire rows or columns to the function
        Dim c As New Collection, v
        On Error Resume Next   ' to ignore the Run-time error 457: "This key is already associated with an element of this collection".
        For Each v In r.Value  ' remove .Value for ranges with more than one Areas
            c.Add 0, v & ""
        Next
        c.Remove "" ' optional to exclude blank values from the count
        countUnique = c.Count
    End Function

https://www.wikiod.com/vba/collections

