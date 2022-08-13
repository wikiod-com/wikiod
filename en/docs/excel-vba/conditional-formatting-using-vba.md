---
title: "Conditional formatting using VBA"
slug: "conditional-formatting-using-vba"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

You cannot define more than three conditional formats for a range. Use the Modify method to modify an existing conditional format, or use the Delete method to delete an existing format before adding a new one.

## FormatConditions.Add
Syntax:
=======

    FormatConditions.Add(Type, Operator, Formula1, Formula2)

Parameters:
===========

Name | Required / Optional | Data Type
---- | ------------------- | ---------
Type | Required | XlFormatConditionType
Operator | Optional | Variant
Formula1 | Optional | Variant
Formula2 | Optional | Variant

XlFormatConditionType enumaration:
----------------------------------

Name | Description 
---- | -----------
xlAboveAverageCondition | Above average condition
xlBlanksCondition | Blanks condition
xlCellValue | Cell value
xlColorScale | Color scale
xlDatabar | Databar
xlErrorsCondition | Errors condition
xlExpression | Expression
XlIconSet | Icon set
xlNoBlanksCondition | No blanks condition
xlNoErrorsCondition | No errors condition
xlTextString | Text string
xlTimePeriod | Time period
xlTop10 | Top 10 values
xlUniqueValues | Unique values

Formatting by cell value:
=========================

    With Range("A1").FormatConditions.Add(xlCellValue, xlGreater, "=100")
        With .Font
            .Bold = True
            .ColorIndex = 3
         End With
    End With

Operators:
----------

Name |
-----|
xlBetween|
xlEqual|
xlGreater|
xlGreaterEqual|
xlLess|
xlLessEqual|
xlNotBetween|
xlNotEqual|
If Type is xlExpression, the Operator argument is ignored.


Formatting by text contains:
============================

    With Range("a1:a10").FormatConditions.Add(xlTextString, TextOperator:=xlContains, String:="egg")
        With .Font
            .Bold = True
            .ColorIndex = 3
        End With
    End With

Operators:
----------

Name | Description
---- | ----------
xlBeginsWith | Begins with a specified value.
xlContains | Contains a specified value.
xlDoesNotContain | Does not contain the specified value.
xlEndsWith | Endswith the specified value


Formatting by time period
=========================

    With Range("a1:a10").FormatConditions.Add(xlTimePeriod, DateOperator:=xlToday)
        With .Font
            .Bold = True
            .ColorIndex = 3
        End With
    End With

Operators:
----------

Name | 
---- | 
xlYesterday |
xlTomorrow |
xlLast7Days |
xlLastWeek |
xlThisWeek |
xlNextWeek |
xlLastMonth |
xlThisMonth |
xlNextMonth |

## Remove conditional format
Remove all conditional format in range:
-------------------------------------

    Range("A1:A10").FormatConditions.Delete

Remove all conditional format in worksheet:
-------------------------------------------

    Cells.FormatConditions.Delete



## FormatConditions.AddUniqueValues
Highlighting Duplicate Values
========================================

    With Range("E1:E100").FormatConditions.AddUniqueValues
       .DupeUnique = xlDuplicate
       With .Font
           .Bold = True
           .ColorIndex = 3
       End With
    End With

Highlighting Unique Values
========================================

    With Range("E1:E100").FormatConditions.AddUniqueValues
       With .Font
           .Bold = True
           .ColorIndex = 3
       End With
    End With

## FormatConditions.AddTop10
Highlighting Top 5 Values
=========================

    With Range("E1:E100").FormatConditions.AddTop10
        .TopBottom = xlTop10Top
        .Rank = 5
        .Percent = False
        With .Font
            .Bold = True
            .ColorIndex = 3
        End With
    End With

## FormatConditions.AddAboveAverage
    With Range("E1:E100").FormatConditions.AddAboveAverage
        .AboveBelow = xlAboveAverage
        With .Font
            .Bold = True
            .ColorIndex = 3
        End With
    End With

Operators:
----------

Name | Description
---- | --------
XlAboveAverage | Above average
XlAboveStdDev | Above standard deviation
XlBelowAverage | Below average
XlBelowStdDev | Below standard deviation
XlEqualAboveAverage | Equal above average
XlEqualBelowAverage | Equal below average

## FormatConditions.AddIconSetCondition
[![enter image description here][1]][1]

    Range("a1:a10").FormatConditions.AddIconSetCondition
    With Selection.FormatConditions(1)
        .ReverseOrder = False
        .ShowIconOnly = False
        .IconSet = ActiveWorkbook.IconSets(xl3Arrows)
    End With
    
    With Selection.FormatConditions(1).IconCriteria(2)
        .Type = xlConditionValuePercent
        .Value = 33
        .Operator = 7
    End With
    
    With Selection.FormatConditions(1).IconCriteria(3)
        .Type = xlConditionValuePercent
        .Value = 67
        .Operator = 7
    End With

IconSet:
--------
Name | 
----- | 
xl3Arrows |
xl3ArrowsGray |
xl3Flags |
xl3Signs |
xl3Stars |
xl3Symbols |
xl3Symbols2 |
xl3TrafficLights1 |
xl3TrafficLights2 |
xl3Triangles |
xl4Arrows |
xl4ArrowsGray |
xl4CRV |
xl4RedToBlack |
xl4TrafficLights |
xl5Arrows |
xl5ArrowsGray | 
xl5Boxes |
xl5CRV |
xl5Quarters |

[![enter image description here][2]][2]

Type:
-----

Name |
----- |
xlConditionValuePercent |
xlConditionValueNumber |
xlConditionValuePercentile |
xlConditionValueFormula |

Operator:
---------
Name | Value |
---- | ----- |
xlGreater | 5 |
xlGreaterEqual | 7 |

Value:
------
Returns or sets the threshold value for an icon in a conditional format.

  [1]: https://i.stack.imgur.com/HYy5B.png
  [2]: https://i.stack.imgur.com/Fgkr1.png

