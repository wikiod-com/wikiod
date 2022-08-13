---
title: "Use Worksheet object and not Sheet object"
slug: "use-worksheet-object-and-not-sheet-object"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Plenty of VBA users consider Worksheets and Sheets objects synonyms.
They are not.

Sheets object consists of both Worksheets and Charts. Thus, if we have charts in our Excel Workbook, we should be careful, not to use `Sheets` and `Worksheets` as synonyms.

## Print the name of the first object
[![enter image description here][1]][1]

    Option Explicit
    
    Sub CheckWorksheetsDiagram()
    
        Debug.Print Worksheets(1).Name
        Debug.Print Charts(1).Name
        Debug.Print Sheets(1).Name
    
    End Sub

The result:

    Sheet1
    Chart1
    Chart1


  [1]: https://i.stack.imgur.com/x3VBw.png

