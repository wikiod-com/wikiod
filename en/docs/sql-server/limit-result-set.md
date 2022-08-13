---
title: "Limit Result Set"
slug: "limit-result-set"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

As database tables grow, it's often useful to limit the results of queries to a fixed number or percentage. This can be achieved using SQL Server's `TOP` keyword or `OFFSET FETCH` clause.

## Parameters
| Parameter | Details |
| --------- | ------- |
| `TOP`     | Limiting keyword. Use with a number. |
| `PERCENT` | Percentage keyword. Comes after `TOP` and limiting number. |

If `ORDER BY` clause is used, limiting applies to the ordered result set.

## Limiting With PERCENT
This example limits `SELECT` result to 15 percentage of total row count.

    SELECT TOP 15 PERCENT *
    FROM table_name

## Limiting With TOP
This example limits `SELECT` result to 100 rows. 

    SELECT TOP 100 *
    FROM table_name;

It is also possible to use a variable to specify the number of rows:

    DECLARE @CountDesiredRows int = 100;
    SELECT TOP (@CountDesiredRows) *
    FROM table_name;

## Limiting with FETCH
<!-- if version [gte SQL Server 2012] -->
`FETCH` is generally more useful for pagination, but can be used as an alternative to `TOP`:

    SELECT *
    FROM table_name
    ORDER BY 1
    OFFSET 0 ROWS
    FETCH NEXT 50 ROWS ONLY
  
<!-- end version if -->

