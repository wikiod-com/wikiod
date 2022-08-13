---
title: "Getting started with excel"
slug: "getting-started-with-excel"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Examples of some basic Formulas available in Excel
Excel has many Formulas built right in. Here are a few examples of some of the basic Formulas that might be handy to know when getting started with Excel:

Important Note : The name and Syntax of these Formulas vary on the language of your Excel installation! For example this Function here : 

in English : 
    `=left("Hello World",5)`

Same in German:
`=links("Hello World";5)`

Not only the name is different but also some minor parts of the syntax! For example in the German version `;` is used instead of `,` to separate the parameters.



All Formulas can be entered into any cell by first typing "=" then the name of the Formula. You can type the Formulas directly into the cell or you can select the cell and type the Formula into the formula bar. To show all the formulas on a sheet you can select the "Show Formulas" button in the "Formula Auditing" group of buttons on the Formulas tab:

[![Show Formulas][1]][1]

A lot of Formulas use a range of cells. To give a range you reference the first cell in the top left corner such as `A1` and the bottom right corner such as `B3` and place a colon ":" between them like this `A1:B3`. This will give you a range of 6 cells in two columns and three rows:

[![Range of Cells][2]][2]



**SUM()**

In cells D1 - D3 are the values 2,3 and 6
In cell D4 is the following Formula: `=SUM(D1:D3)`, The Result is 11 which shows in the cell. 

`Sum` Adds all numbers in a range of cells.

If you have that cell selected then the Formula will show in the Formula bar above the grid of cells:

[![Formula SUM][3]][3]

**COUNT()**/**COUNTA()**/**COUNTIF()**/**COUNTBLANK()**

In cells E1 - E10 are the values 2,3,5,6, blank,8,9, blank,11 and 12

In cell F2 is the Formula: `=count(E1:E10)`, the Result is 8 which shows in the cell.

In cell F3 is the Formula: `=counta(E1:E10)`, the Result is 8 which shows in the cell.

In cell F4 is the Formula: `=countif(E1:E10,">5")`, the Result is 5 which shows in the cell.

In cell F5 is the Formula: `=countblank(E1:E10)`, the Result is 2 which shows in the cell.

`Count()` Counts the number of cells in a range that contain numbers.

`Counta()` Counts the number of cells in a range that are not empty.

`CountIF()` Counts the number of cells in a range that meet a given condition.

`CountBlank()` Counts the number of empty cells in a specified range of cells.

[![Count][4]][4]

There are also many string Formulas.

**LEFT()**/**RIGHT()**/**MID()**

In cell D1 is "Hello World!".

In cell E1 is the Formula: `=left(D1,5)`, the Result is: "Hello"

In cell F1 is the Formula: `=right(D1,6)`, the Result is: "World!"

In cell G1 is the Formula: `=mid(D1,7,5)`, the Result is: "World"

`left` Returns the specified number of characters from the start of a text string. `=LEFT(String,Number of Characters)`

`right` Returns the specified number of characters from the end of a text string. `=RIGHT(String, Number of Characters)`

`mid` Returns the characters from the middle of a text string, given the starting position and length. `=MID(String, Start Position, Number of Characters)`

These three Formulas count the characters of the string starting with position 1 being the first character.

[![String Formulas Showing][5]][5]

[![String Formulas Results Showing][6]][6]


  [1]: https://i.stack.imgur.com/W7zdC.jpg
  [2]: https://i.stack.imgur.com/YpmTL.jpg
  [3]: https://i.stack.imgur.com/ElYMW.jpg
  [4]: https://i.stack.imgur.com/wByF7.jpg
  [5]: https://i.stack.imgur.com/x8Aie.jpg
  [6]: https://i.stack.imgur.com/mXQ1b.jpg

