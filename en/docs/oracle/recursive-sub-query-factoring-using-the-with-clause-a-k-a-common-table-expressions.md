---
title: "Recursive Sub-Query Factoring using the WITH Clause (A.K.A. Common Table Expressions)"
slug: "recursive-sub-query-factoring-using-the-with-clause-aka-common-table-expressions"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Recursive sub-query factoring is available in Oracle 11g R2.

## Splitting a Delimited String
**Sample Data**:

    CREATE TABLE table_name ( value VARCHAR2(50) );
    
    INSERT INTO table_name ( value ) VALUES ( 'A,B,C,D,E' );

**Query**:

    WITH items ( list, item, lvl ) AS (
      SELECT value,
             REGEXP_SUBSTR( value, '[^,]+', 1, 1 ),
             1
      FROM   table_name
    UNION ALL
      SELECT value,
             REGEXP_SUBSTR( value, '[^,]+', 1, lvl + 1 ),
             lvl + 1
      FROM   items
      WHERE  lvl < REGEXP_COUNT( value, '[^,]+' )
    )
    SELECT * FROM items;

**Output**:

    LIST      ITEM LVL
    --------- ---- ---
    A,B,C,D,E    A   1
    A,B,C,D,E    B   2
    A,B,C,D,E    C   3
    A,B,C,D,E    D   4
    A,B,C,D,E    E   5



## A Simple Integer Generator
**Query**:

    WITH generator ( value ) AS (
      SELECT 1 FROM DUAL
    UNION ALL
      SELECT value + 1
      FROM   generator
      WHERE  value < 10
    )
    SELECT value
    FROM   generator;

**Output**:

    VALUE
    -----
        1
        2
        3
        4
        5
        6
        7
        8
        9
       10


