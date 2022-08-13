---
title: "Query Function"
slug: "query-function"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

# Official Documentation
## Google Docs editors Help

- [QUERY](https://support.google.com/docs/answer/3093343?hl=en)

## Google Charts on Google Developers

- [Query Language Reference (Version 0.7)](https://developers.google.com/chart/interactive/docs/querylanguage)

## Introduction into queries
**Source table**

|Row|A|B|C|D|
|---:| :------: | :------: | :------: | ----: |
|1|**Code**|**Product**|**Colour**|**Price**|
|2|1|pen|red|500|
|3|2|pen|blue|-50|
|4|3|pen|red|0|
|5|4|pencil|blue|17|
|6|5|pencil|green|-1.5|

to select all:

    = QUERY(A1:D5, "select *")
or 

    = QUERY(A1:D5, "select A, B, C, D")
or convert data range into array and use this formula:

    = QUERY({A1:D5}, "select Col1, Col2, Col3, Col4")

## Sorting with QUERY()
| A | B | C | D | 
| ------ | ------ | ------ | ------ |
| 1  | pen| red | 500 |
| 2 | pen | blue | -50 |
| 3 | pen | red | 0 |
| 4 | pencil | blue | 17 |
| 5 | pencil | green | -1.5 |

To sort by column D with "order by":

    =QUERY("A1:D6","select * order by D desc",1)

## Filtering with QUERY()
| A | B | C | D | 
| ------ | ------ | ------ | ------ |
| 1  | pen| red | 500 |
| 2 | pen | blue | -50 |
| 3 | pen | red | 0 |
| 4 | pencil | blue | 17 |
| 5 | pencil | green | -1.5 |

To only return "pencil" data:

    =QUERY("A1:D6","select * where B='pencil' ",1)
To only return rows that contain "pen" (all rows):

    =QUERY("A1:D6","select * where B contains 'pen' ",1)
To only return rows where the price is greater than 0:

    =QUERY("A1:D6","select * where D>0 ",1)
Note that text strings require apostrophes while numerical values do not.

## Filter a query by an aggregation result
   =QUERY(QUERY(A1:D6,"select C,SUM(D) group by C",1),"select Col2>0",1)


