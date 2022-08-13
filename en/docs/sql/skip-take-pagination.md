---
title: "SKIP TAKE (Pagination)"
slug: "skip-take-pagination"
draft: false
images: []
weight: 9839
type: docs
toc: true
---

## Limiting amount of results
ISO/ANSI SQL:

    SELECT * FROM TableName FETCH FIRST 20 ROWS ONLY;

MySQL; PostgreSQL; SQLite:

    SELECT * FROM TableName LIMIT 20; 

Oracle:

    SELECT Id,
       Col1
    FROM (SELECT Id,
               Col1,
               row_number() over (order by Id) RowNumber
          FROM TableName)
    WHERE RowNumber <= 20

SQL Server:

    SELECT TOP 20 * 
    FROM dbo.[Sale]

## Skipping then taking some results (Pagination)
ISO/ANSI SQL:

    SELECT Id, Col1
    FROM TableName
    ORDER BY Id
    OFFSET 20 ROWS FETCH NEXT 20 ROWS ONLY;

MySQL:

    SELECT * FROM TableName LIMIT 20, 20; -- offset, limit

Oracle; SQL Server:

    SELECT Id,
       Col1
     FROM (SELECT Id,
               Col1,
               row_number() over (order by Id) RowNumber
          FROM TableName)
    WHERE RowNumber BETWEEN 21 AND 40

PostgreSQL; SQLite:

    SELECT * FROM TableName LIMIT 20 OFFSET 20;

## Skipping some rows from result
ISO/ANSI SQL:

    SELECT Id, Col1
    FROM TableName
    ORDER BY Id
    OFFSET 20 ROWS

MySQL:

    SELECT * FROM TableName LIMIT 20, 42424242424242;
    -- skips 20 for take use very large number that is more than rows in table

Oracle:

    SELECT Id,
       Col1
    FROM (SELECT Id,
               Col1,
               row_number() over (order by Id) RowNumber
          FROM TableName)
    WHERE RowNumber > 20

PostgreSQL:

    SELECT * FROM TableName OFFSET 20;

SQLite:

    SELECT * FROM TableName LIMIT -1 OFFSET 20;

