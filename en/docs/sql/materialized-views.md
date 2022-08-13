---
title: "Materialized Views"
slug: "materialized-views"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

A materialized view is a view whose results are physically stored and must be periodically refreshed in order to remain current. They are therefore useful for storing the results of complex, long-running queries when realtime results are not required.

Materialized views can be created in Oracle and PostgreSQL. Other database systems offer similar functionality, such as SQL Server's indexed views or DB2's materialized query tables.

## PostgreSQL example
    CREATE TABLE mytable (number INT);
    INSERT INTO mytable VALUES (1);

    CREATE MATERIALIZED VIEW myview AS SELECT * FROM mytable;

    SELECT * FROM myview;
     number 
    --------
          1
    (1 row)

    INSERT INTO mytable VALUES(2);

    SELECT * FROM myview;
     number 
    --------
          1
    (1 row)

    REFRESH MATERIALIZED VIEW myview;

    SELECT * FROM myview;
     number 
    --------
          1
          2
    (2 rows)

