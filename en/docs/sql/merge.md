---
title: "MERGE"
slug: "merge"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

MERGE (often also called UPSERT for "update or insert") allows to insert new rows or, if a row already exists, to update the existing row. The point is to perform the whole set of operations atomically (to guarantee that the data remain consistent), and to prevent communication overhead for multiple SQL statements in a client/server system.

## MERGE to make Target match Source
    MERGE INTO targetTable t
        USING sourceTable s
            ON t.PKID = s.PKID
        WHEN MATCHED AND NOT EXISTS (
                SELECT s.ColumnA, s.ColumnB, s.ColumnC
                INTERSECT
                SELECT t.ColumnA, t.ColumnB, s.ColumnC
                )
            THEN UPDATE SET
                t.ColumnA = s.ColumnA
                ,t.ColumnB = s.ColumnB
                ,t.ColumnC = s.ColumnC
        WHEN NOT MATCHED BY TARGET
            THEN INSERT (PKID, ColumnA, ColumnB, ColumnC)
            VALUES (s.PKID, s.ColumnA, s.ColumnB, s.ColumnC)
        WHEN NOT MATCHED BY SOURCE
            THEN DELETE
        ;

Note: The `AND NOT EXISTS` portion prevents updating records that haven't changed. Using the `INTERSECT` construct allows nullable columns to be compared without special handling.

## MySQL: counting users by name


## PostgreSQL: counting users by name


