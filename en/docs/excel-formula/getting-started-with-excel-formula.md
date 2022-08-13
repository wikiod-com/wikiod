---
title: "Getting started with excel-formula"
slug: "getting-started-with-excel-formula"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
Excel formulas are used to process simple or complex equations and output the result to a specific cell within a worksheet.  
___
_(There is an exception to this where the `WorksheetFunction` class is used in VBA programming, however VBA is out of scope for this tag - please refer to the `excel-vba` or `vba` documentation for VBA related articles.)_  
___

Formulas can be used to convert, manipulate or evaluate many different kinds of expressions using all different kinds of data. There are however some restrictions around formulas and what they are able to achieve.

 + Formulas cannot change any part of a worksheet or contents of another cell.
 + Formulas can only return numbers precise up to 15 digits.
 + Formulas cannot contain more than 8,192 characters.
 + Formulas cannot reference more than 2,048 other ranges.
 + Formulas cannot use more than 255 arguments in any given function.
 + Dates used in calculations must be in the range of 1900-01-01 to 9999-12-31






## Example of an excel formula
| Column  | Column A|  Example formula| Example formula |
| ------ | ------ | ------ | ------ |
| Row 1| 2|  |  |
| Row 2| 3   |  |  |
| Row 3| 5   | =A1+A2| =Sum(A1,A2)|

## Installation or Setup
Detailed instructions on excel-formula(s).

There are many formulas that you can choose from. They are divided up into 7 different categories and are on the FORMULAS tab in Excel. The categories are:

1. Financial
2. Logical
3. Text
4. Date & Time
5. Lookup & Reference
6. Math & trig
7. More Functions

Then there are also sections for 
1. Recently used
2. AutoSum

If you are not sure which one to use you can use the `Insert Function` button to help with determining which one to use.

You do not have to use just one formula at a time, you can combine them to get the results that are needed or combine them with other things like `Named Ranges`.

`Named Ranges` are also on the FORMULAS tab in the *Define Name* section. You can use the *Name Manager* to create or modify the ranges and names of the ranges. There are also three other buttons there for *Define Name* to define and apply names, *Use in Formula* to choose a defined name and insert it into a formula, and *Create from Selection* which will create a named range with the name based on the top-left most cell in the range.

There is also the **Formula Auditing** section. This section will help in troubleshooting a formula. Here you can trace the formula and see what exactly it is doing.

Finally, there is the **Calculation** section which will allow you to turn on/off the automatic calculation of all formulas and to manually calculate any formulas whether auto-calculate is on or not.

## See Excel
There is no additional installation required for **excel-formula** on top of what is already required for **excel**. Please refer to [excel documentation](https://www.wikiod.com/excel/getting-started-with-excel).

## Excel cell references
You can use cell references without row numbers. 

For instance, the formula `=sum(C:C)` will add up all the values in column C of the active sheet. This is helpful when you are adding and removing rows but don't want to update your formulas each time. 

There are some instances when using this whole column reference is not a good idea. There is a good article [here][1]. It discusses many different variables and test cases to explain when it would be beneficial to use something like `=sum(C:C)` or to use something like `=sum(C1:C1000)`.

|A|B|C|D|E|
| ------ | ------ | ------ | ------ | ------ |
|1|Bob|4|the formula: =sum(C:C) = |20
|2|Pete|7| | |
|3|Mary|9| | |


  [1]: https://fastexcel.wordpress.com/2015/12/12/excel-full-column-references-and-used-range-good-idea-or-bad-idea/

