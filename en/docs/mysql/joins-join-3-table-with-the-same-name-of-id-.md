---
title: "JOINS Join 3 table with the same name of id."
slug: "joins-join-3-table-with-the-same-name-of-id"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Join 3 tables on a column with the same name
    CREATE TABLE Table1 (
        id INT UNSIGNED NOT NULL,
        created_on DATE NOT NULL,
        PRIMARY KEY (id)
    )
    CREATE TABLE Table2 (
        id INT UNSIGNED NOT NULL,
        personName VARCHAR(255) NOT NULL,
        PRIMARY KEY (id)
    )
    CREATE TABLE Table3 (
        id INT UNSIGNED NOT NULL,
        accountName VARCHAR(255) NOT NULL,
        PRIMARY KEY (id)
    )

after creating the tables you could do a select query to get the id's of all three tables that are the same

    SELECT 
        t1.id AS table1Id, 
        t2.id AS table2Id, 
        t3.id AS table3Id 
    FROM Table1 t1
    LEFT JOIN Table2 t2 ON t2.id = t1.id
    LEFT JOIN Table3 t3 ON t3.id = t1.id

