---
title: "Aggregate Functions"
slug: "aggregate-functions"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

Aggregate functions in SQL Server run calculations on sets of values, returning a single value.

## Syntax
- AVG([ALL|DISTINCT]_expression_)
- COUNT([ALL|DISTINCT]_expression_)
- MAX([ALL|DISTINCT]_expression_)
- MIN([ALL|DISTINCT]_expression_)
- SUM([ALL|DISTINCT]_expression_)

## SUM()
Returns sum of numeric values in a given column. 

We have table as shown in figure that will be used to perform different aggregate functions. The table name is *Marksheet*.


[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/gUPGI.png



    Select SUM(MarksObtained) From Marksheet

The `sum` function doesn't consider rows with NULL value in the field used as parameter

In the above example if we have another row like this:
    
    106    Italian    NULL

This row will not be consider in sum calculation

## AVG()
Returns average of numeric values in a given column. 

We have table as shown in figure that will be used to perform different aggregate functions. The table name is *Marksheet*.


[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/gUPGI.png



    Select AVG(MarksObtained) From Marksheet

The `average` function doesn't consider rows with NULL value in the field used as parameter

In the above example if we have another row like this:
    
    106    Italian    NULL

This row will not be consider in average calculation

## MAX()
Returns the largest value in a given column. 

We have table as shown in figure that will be used to perform different aggregate functions. The table name is *Marksheet*.


[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/gUPGI.png



    Select MAX(MarksObtained) From Marksheet

## MIN()
Returns the smallest value in a given column. 

We have table as shown in figure that will be used to perform different aggregate functions. The table name is *Marksheet*.


[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/gUPGI.png



    Select MIN(MarksObtained) From Marksheet

## COUNT()
Returns the total number of values in a given column.

We have table as shown in figure that will be used to perform different aggregate functions. The table name is *Marksheet*.


[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/gUPGI.png



    Select COUNT(MarksObtained) From Marksheet

The `count` function doesn't consider rows with NULL value in the field used as parameter. Usually the count parameter is * (all fields) so only if all fields of row are NULLs this row will not be considered

In the above example if we have another row like this:
    
    106    Italian    NULL

This row will not be consider in count calculation

**NOTE**

The function <code>COUNT(*)</code> returns the number of rows in a table. This value can also be obtained by using a constant non-null expression that contains no column references, such as <code>COUNT(1)</code>.

Example
    
    Select COUNT(1) From Marksheet

## COUNT(Column_Name) with GROUP BY Column_Name
Most of the time we like to get the total number of occurrence of a column value in a table for example:

 TABLE NAME :  REPORTS

| ReportName| ReportPrice |
| --------- | ----------- |
| Test      | 10.00   $   |
| Test      | 10.00   $   |
| Test      | 10.00   $   |
| Test 2    | 11.00   $   |
| Test      | 10.00   $   |
| Test 3    | 14.00   $   |
| Test 3    | 14.00   $   |
| Test 4    | 100.00   $  |

    SELECT  
        ReportName AS REPORT NAME, 
        COUNT(ReportName) AS COUNT 
    FROM     
        REPORTS 
    GROUP BY 
        ReportName 



| REPORT NAME | COUNT      |
| ------      | ------     |
| Test        | 4          |
| Test 2      | 1          |
| Test 3      | 2          |
| Test 4      | 1          |
 






