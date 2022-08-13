---
title: "Limiting the rows returned by a query (Pagination)"
slug: "limiting-the-rows-returned-by-a-query-pagination"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Get first N rows with row limiting clause
The `FETCH` clause was introduced in Oracle 12c R1:

    SELECT   val
    FROM     mytable
    ORDER BY val DESC
    FETCH FIRST 5 ROWS ONLY;

An example without FETCH that works also in earlier versions:
 
    SELECT * FROM (
       SELECT   val
       FROM     mytable
       ORDER BY val DESC
    ) WHERE ROWNUM <= 5;


## Pagination in SQL
    SELECT val 
    FROM   (SELECT val, rownum AS rnum
            FROM   (SELECT val
                    FROM   rownum_order_test
                    ORDER BY val)
            WHERE rownum <= :upper_limit)
    WHERE  rnum >= :lower_limit ;
this way we can paginate the table data , just like  web serch page


## Get N numbers of Records from table
We can limit no of rows from result using rownum clause 

    select * from 
    ( 
      select val from  mytable
    ) where rownum<=5

If we want first or last record then we want order by clause in inner query that will give result based on order.

**Last Five Record :**

    select * from 
    ( 
        select val from  mytable order by val desc
    ) where rownum<=5

**First Five Record**

    select * from 
    ( 
        select val from  mytable order by val
    ) where rownum<=5

## Get row N through M from many rows (before Oracle 12c)
Use the analytical function row_number():

    with t as (
      select col1
      , col2
      , row_number() over (order by col1, col2) rn
      from table
    )
    select col1
    , col2
    from t
    where rn between N and M; -- N and M are both inclusive

Oracle 12c handles this more easily with `OFFSET` and `FETCH`.



## Skipping some rows then taking some
In Oracle 12g+

    SELECT Id, Col1
    FROM TableName 
    ORDER BY Id
    OFFSET 20 ROWS FETCH NEXT 20 ROWS ONLY;

In earlier Versions

    SELECT Id, 
       Col1
     FROM (SELECT Id,
               Col1,
               row_number() over (order by Id) RowNumber
          FROM TableName)
    WHERE RowNumber BETWEEN 21 AND 40

## Skipping some rows from result
In Oracle 12g+

    SELECT Id, Col1
    FROM TableName 
    ORDER BY Id
    OFFSET 5 ROWS;

In earlier Versions

    SELECT Id, 
       Col1
     FROM (SELECT Id,
               Col1,
               row_number() over (order by Id) RowNumber
          FROM TableName)
    WHERE RowNumber > 20

