---
title: "Cell Formatting"
slug: "cell-formatting"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Each cell in an Excel spreadsheet can have unique formatting for things such as font, number formatting, color, borders, and alignment.  Conditional formatting allows formatting for cells to vary based on data in the spreadsheet.

See also https://www.wikiod.com/excel/excel-rounding-and-precision

Conditional Formatting allows the formatting of color, font effects, background color, etc. of cells on the spreadsheet to vary based on the cell's value, or other cell's values.  A range of cells can have mini-charts or different icons in the cells based on their values.

## Number Formatting
The most common use of formatting is to control display of numeric information contained in the cells in a consistent format, such as currency, a certain number of digits to the right of the decimal point, etc.  There are several categories for numbers, such as Currency, Accounting, Percentage, and more.  For each Category, there are various options available:
[![Sample Number Format Cells Dialog][1]][1]


The cell formatting does _not_ affect the internal value or formula results, just the displayed value.  If a cell contains the formula `=4*ACOT(1)` Excel can display that as 
* `3.14159265359` (Number with 11 decimal places)
* `$3.14` (US Currency format)
* `3 1/7` (Fraction with up to one digit)

You can create complex formats with different masks for positive, negative, and zero values.  One shortcut is to use the existing Category closest to your needs, set the options, then click on the "Custom" Category, and you will see the formatting codes used, and you can alter them as needed.  

The format code for "Fraction with up to one digit" is `# ?/?` and if you clicked Custom and entered `# "and" ?/?` for the format, the value displayed would be `3 and 1/7`


  [1]: https://i.stack.imgur.com/MVlid.png

