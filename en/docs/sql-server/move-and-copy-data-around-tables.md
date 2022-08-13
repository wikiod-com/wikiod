---
title: "Move and copy data around tables"
slug: "move-and-copy-data-around-tables"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Copy data from one table to another
This code selects data out of a table and displays it in the query tool (usually SSMS)

    SELECT Column1, Column2, Column3 FROM MySourceTable;

This code inserts that data into a table:

    INSERT INTO MyTargetTable (Column1, Column2, Column3)
    SELECT Column1, Column2, Column3 FROM MySourceTable;





## Copy data into a table, creating that table on the fly
This code selects data out of a table:

    SELECT Column1, Column2, Column3 FROM MySourceTable;

This code creates a new table called `MyNewTable` and puts that data into it

    SELECT Column1, Column2, Column3 
    INTO MyNewTable
    FROM MySourceTable;

## Move data into a table (assuming unique keys method)
To _move_ data you first insert it into the target, then delete whatever you inserted from the source table. This is not a normal SQL operation but it may be enlightening

What did you insert? Normally in databases you need to have one or more columns that you can use to uniquely identify rows so we will assume that and make use of it.

This statement selects some rows

    SELECT Key1, Key2, Column3, Column4 FROM MyTable;

First we insert these into our target table:

    INSERT INTO TargetTable (Key1, Key2, Column3, Column4)
    SELECT Key1, Key2, Column3, Column4 FROM MyTable;



Now _assuming records in both tables are unique_ on `Key1`,`Key2`, we can use that to find and delete data out of the source table

    DELETE MyTable
    WHERE EXISTS (
        SELECT * FROM TargetTable 
        WHERE TargetTable.Key1 = SourceTable.Key1
        AND TargetTable.Key2 = SourceTable.Key2
    );

This will only work correctly if `Key1`, `Key2` are unique in both tables


Lastly, we don't want the job half done. If we wrap this up in a transaction then either all data will be moved, or nothing will happen. This ensures we don't insert the data in then find ourselves unable to delete the data out of the source.

    BEGIN TRAN;

    INSERT INTO TargetTable (Key1, Key2, Column3, Column4)
    SELECT Key1, Key2, Column3, Column4 FROM MyTable;


    DELETE MyTable
    WHERE EXISTS (
        SELECT * FROM TargetTable 
        WHERE TargetTable.Key1 = SourceTable.Key1
        AND TargetTable.Key2 = SourceTable.Key2
    );

    COMMIT TRAN;

