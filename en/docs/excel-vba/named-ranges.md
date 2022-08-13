---
title: "Named Ranges"
slug: "named-ranges"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Topic should include information specifically related to named ranges in Excel including methods for creating, modifying, deleting, and accessing defined named ranges.

## Define A Named Range
Using named ranges allows you to describe the meaning of a cell(s) contents and use this defined name in place of an actual cell address.

For example, formula `=A5*B5` can be replaced with `=Width*Height` to make the formula much easier to read and understand.  

To define a new named range, select cell or cells to name and then type new name into the Name Box next to the formula bar.

[![enter image description here][1]][1]

-----------

> Note: Named Ranges default to global scope meaning that they can be
> accessed from anywhere within the workbook.  Older versions of Excel allow for duplicate names so care must be taken to
> prevent duplicate names of global scope otherwise results will be
> unpredictable.  Use Name Manager from Formulas tab to change scope.

  [1]: https://i.stack.imgur.com/KqLbf.gif

## Using Named Ranges in VBA
**Create** new named range called ‘MyRange’ assigned to cell `A1`

    ThisWorkbook.Names.Add Name:="MyRange", _
        RefersTo:=Worksheets("Sheet1").Range("A1")

-----
**Delete** defined named range by name

    ThisWorkbook.Names("MyRange").Delete

-----
**Access Named Range** by name

    Dim rng As Range
    Set rng = ThisWorkbook.Worksheets("Sheet1").Range("MyRange")
    Call MsgBox("Width = " & rng.Value)

-----
**Access a Named Range with a Shortcut**

[Just like any other range](http://www.informit.com/articles/article.aspx?p=2021718&seqNum=4), named ranges can be accessed directly with through a shortcut notation that does not require a `Range` object to be created. The three lines from the code excerpt above can be replaced by a single line:

    Call MsgBox("Width = " & [MyRange])

> Note: The default property for a Range is its Value, so `[MyRange]` is the same as `[MyRange].Value`

You can also call methods on the range. The following selects `MyRange`: 

    [MyRange].Select

> Note: One caveat is that the shortcut notation does not work with words
> that are used elsewhere in the VBA library. For example, a range named
> `Width` would not be accessible as `[Width]` but would work as expected
> if accessed through `ThisWorkbook.Worksheets("Sheet1").Range("Width")`

## Manage Named Range(s) using Name Manager
Formulas tab > Defined Names group > Name Manager button

Named Manager allows you to:
1.    Create or change name 
2.    Create or change cell reference 
3.    Create or change scope
4.    Delete existing named range

[![enter image description here][1]][1]

-----

Named Manager provides a useful quick look for broken links.

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/62GIj.jpg
  [2]: https://i.stack.imgur.com/wx6B0.jpg

## Named Range Arrays
Example sheet

[![enter image description here][1]][1]

----------

**Code**

    Sub Example()
        Dim wks As Worksheet
        Set wks = ThisWorkbook.Worksheets("Sheet1")
        
        Dim units As Range
        Set units = ThisWorkbook.Names("Units").RefersToRange
        
        Worksheets("Sheet1").Range("Year_Max").Value = WorksheetFunction.Max(units)
        Worksheets("Sheet1").Range("Year_Min").Value = WorksheetFunction.Min(units)
    End Sub

----------

**Result**

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/Q7YIB.png
  [2]: https://i.stack.imgur.com/RYAKu.png

