---
title: "Working with Excel Tables in VBA"
slug: "working-with-excel-tables-in-vba"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

This topic is about working with tables in VBA, and assumes knowledge of Excel Tables. In VBA, or rather the Excel Object Model, tables are known as ListObjects. The most frequently used properties of a ListObject are ListRow(s), ListColumn(s), DataBodyRange, Range and HeaderRowRange.

## Instantiating a ListObject
    Dim lo as ListObject
    Dim MyRange as Range

    Set lo = Sheet1.ListObjects(1)

    'or
 
    Set lo = Sheet1.ListObjects("Table1")

    'or

    Set lo = MyRange.ListObject

## Working with ListRows / ListColumns
    Dim lo as ListObject
    Dim lr as ListRow
    Dim lc as ListColumn

    Set lr = lo.ListRows.Add
    Set lr = lo.ListRows(5)

    For Each lr in lo.ListRows
        lr.Range.ClearContents
        lr.Range(1, lo.ListColumns("Some Column").Index).Value = 8
    Next

    Set lc = lo.ListColumns.Add
    Set lc = lo.ListColumns(4)
    Set lc = lo.ListColumns("Header 3")

    For Each lc in lo.ListColumns
        lc.DataBodyRange.ClearContents   'DataBodyRange excludes the header row
        lc.Range(1,1).Value = "New Header Name"    'Range includes the header row
    Next


## Converting an Excel Table to a normal range
    Dim lo as ListObject

    Set lo = Sheet1.ListObjects("Table1")
    lo.Unlist

