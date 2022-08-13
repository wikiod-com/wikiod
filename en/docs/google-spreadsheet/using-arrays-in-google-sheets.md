---
title: "Using arrays in Google Sheets"
slug: "using-arrays-in-google-sheets"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
- ={item1,item2}  
- ={item2,item2;item3,item4}  
- ={{item1;item2},{item3,item4;item5,item6}} 


## Parameters
| Parameter| Details |
| ------ | ------ |
| itemN   | It could be a value, a cell reference, a range reference or a function   |

# Overview
An array of literals is written between curly brackets. Separators depends on the spreadsheet's regional configuration settings. 
- To separate columns, if the decimal separator is `.` use , but if the decimal separator is `,` then use `\`.
- To separate rows use `;`.

# Official Documentation
## Google Docs editors Help

- [Using arrays in Google Sheets](https://support.google.com/docs/answer/6208276?hl=en)

## Array of literals
Formula in A1

    ={"Item name","Quantity";"Apples",2;"Blueberries",5}

**Important:** In certain countries the comma is used as a decimal separator (e.g: €1,00). If that's your case, you would need to use backslashes ( \\ ) instead: ([Docs](https://support.google.com/docs/answer/6208276?hl=en))

    ={"Item name"\"Quantity";"Apples"\2;"Blueberries"\5}

Result

Row|A|B|
---|---|---|
1|Item name|Quantity|
2|Apples|2|
3|Blueberries|5

## Returning a range as an array
| Row | A | B |
| ------ | ------ | ------ |
| 1   | Fruit   | Apple |
| 2   | Weekday | Monday |
| 3   | Animal  | Dog |

Formula on C1

    ={A1:A3}

Result

| Row | C |
| ------ | ------ |
| 1   | Fruit   |
| 2   | Weekday  |
| 3   | Dog   |

Alternative formula

    =ARRAYFORMULA(A1:A3)


## Append column with row numbering
A1:A4 have A,B,C,D.  
B1 have the following formula:

    =ARRAYFORMULA({A1:A4,ROW(A1:A4)})

Result

|  | A | B | C | 
| ------ | ------ | ------ |------ |
| 1   | A   | A   | 1   |
| 2   | B   | B   | 2   |
| 3   | C   | C   | 3   |
| 4   | D   | D   | 4   |

