---
title: "SELECT statement"
slug: "select-statement"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

In SQL, `SELECT` statements return sets of results from data collections like tables or views. `SELECT` statements can be used with various other clauses like `WHERE`, `GROUP BY`, or `ORDER BY` to further refine the desired results.

## Basic SELECT from table
Select all columns from some table (system table in this case):

    SELECT *
    FROM sys.objects

Or, select just some specific columns:

    SELECT object_id, name, type, create_date
    FROM sys.objects

## Filter rows using WHERE clause
WHERE clause filters only those rows that satisfy some condition:

    SELECT *
    FROM sys.objects
    WHERE type = 'IT'

## Sort results using ORDER BY
ORDER BY clause sorts rows in the returned result set by some column or expression:

    SELECT *
    FROM sys.objects
    ORDER BY create_date

## Group result using GROUP BY
GROUP BY clause groups rows by some value:

    SELECT type, count(*) as c
    FROM sys.objects
    GROUP BY type

You can apply some function on each group (aggregate function) to calculate sum or count of the records in the group.

| type | c |
| ------ | ------ |
|SQ|3|
|S|72|
|IT|16|
|PK|1|
|U|5|

## Filter groups using HAVING clause
HAVING clause removes groups that do not satisfy condition:

    SELECT type, count(*) as c
    FROM sys.objects
    GROUP BY type
    HAVING count(*) < 10

| type | c |
| ------ | ------ |
|SQ|3|
|PK|1|
|U|5|

## Returning only first N rows
TOP clause returns only first N rows in the result:

    SELECT TOP 10 *
    FROM sys.objects

## Pagination using OFFSET FETCH
OFFSET FETCH clause is more advanced version of TOP. It enables you to skip N1 rows and take next N2 rows:

    SELECT *
    FROM sys.objects
    ORDER BY object_id
    OFFSET 50 ROWS FETCH NEXT 10 ROWS ONLY

You can use OFFSET without fetch to just skip first 50 rows:

    SELECT *
    FROM sys.objects
    ORDER BY object_id
    OFFSET 50 ROWS

## SELECT without FROM (no data souce)
SELECT statement can be executed without FROM clause:

    declare @var int = 17;
    
    SELECT @var as c1, @var + 2 as c2, 'third' as c3 

In this case, one row with values/results of expressions are returned.

