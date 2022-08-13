---
title: "Access SQL"
slug: "access-sql"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Introduction to Access SQL
When using Access you can retrieve data using queries. These queries are built using Structured Query Language (SQL). Understanding SQL is important because it can help build better, more useful queries. 

When creating queries in Access, you can switch to "SQL View". An example of a "select" query is shown here:

[![enter image description here][1]][1]

  [1]: http://i.stack.imgur.com/2qWO3.jpg

## Union (Merge) Queries
When you wish to combine the results of multiple tables or queries with similar fields together into a single resulting data set without performing any relational joins (i.e. you want to list one dataset immediately after the other), you will use a `UNION` query. However, it is notable that **these queries must be manually created in SQL View.**

Syntax of a `UNION` query is

    SELECT
      floatingpoint_field AS floatptfld,
      text_field
    FROM first_table
      UNION
    SELECT
      integer_field,
      decimal_field
    FROM a_saved_query
      UNION
    SELECT
      1.0,
      "hi there Jack"

and will return a two-field dataset with field (column) names: `floatptfld` and `text_field`


It is critical that the data types (and data styles) for subsequently merged tables fields are compatible with the first query in the series. In other words, if the first `SELECT` query generates a number for the first column, the second query must also return a number in the first column. In addition to matching types of fields in order, the `SELECT` statements must return the same number of fields. Names for the fields of the resulting datasheet are inherited from the first table definition.

The following query would NOT be legal, as text cannot be turned into decimal data nor can floating point numbers be converted to integers (without explicit truncation or rounding and type-casting).

    SELECT
      integer_field AS this_really_wont_turn_out_well,
      decimal_field
    FROM a_saved_query
      UNION
    SELECT
      floatingpoint_field,
      text_field
    FROM first_table



## The COUNT() Function
You can use the COUNT() function to return the number of records that match a query. The following 'Employee' table contains employee ID numbers and their associated manager's ID number.

| Employee_ID| Manager_ID|
| ------ | ------ |
    12         |37
    22         |37
    37         |63
    42         |45
    45         |63
    57         |45
    59         |45
    63         |

A COUNT() statement can be used to find out how many employees have a specific manager:

    SELECT COUNT(*) AS CNT FROM Employees WHERE Employee.Manager_ID = 37;

returns

| CNT |
| ------ |
 2


The function can also be combined in more complicated queries. To find out how many employees are directly supervised by a specified person, the following can be applied:

    SELECT T1.Employee_ID,
        (SELECT COUNT(*) AS CNT FROM Employees AS T2 WHERE T2.Manager_ID =
            T1.Employee_ID) AS Supervised_Count
    FROM Employees AS T1;

returns:

| Employee_ID| Supervised_Count|
| ------ | ------ |
    12         |0
    22         |0
    37         |2
    42         |0
    45         |3
    57         |0
    59         |0
    63         |2

MSDN documentation may be found [here][1].


  [1]: https://msdn.microsoft.com/en-us/library/bb177890(v=office.12).aspx


