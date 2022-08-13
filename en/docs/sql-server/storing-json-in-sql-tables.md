---
title: "Storing JSON in SQL tables"
slug: "storing-json-in-sql-tables"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## JSON stored as text column
JSON is textual format, so it is stored in standard NVARCHAR columns. NoSQL collection is equivalent to two column key value table:

    CREATE TABLE ProductCollection (
      Id int identity primary key,
      Data nvarchar(max)
    )

Use `nvarchar(max)` as you are not sure what would be the size of your JSON documents. `nvarchar(4000)` and `varchar(8000)` have better performance but with size limit to 8KB.

## Ensure that JSON is properly formatted using ISJSON
Since JSON is stored textual column, you might want to ensure that it is properly formatted. You can add CHECK constraint on JSON column that checks is text properly formatted JSON:

    CREATE TABLE ProductCollection (
      Id int identity primary key,
      Data nvarchar(max)
           CONSTRAINT [Data should be formatted as JSON]
           CHECK (ISJSON(Data) > 0)
    )

If you already have a table, you can add check constraint using the ALTER TABLE statement:

    ALTER TABLE ProductCollection
        ADD CONSTRAINT [Data should be formatted as JSON]
            CHECK (ISJSON(Data) > 0)



## Expose values from JSON text as computed columns
You can expose values from JSON column as computed columns:

    CREATE TABLE ProductCollection (
      Id int identity primary key,
      Data nvarchar(max),
      Price AS JSON_VALUE(Data, '$.Price'),
      Color JSON_VALUE(Data, '$.Color') PERSISTED
    )

If you add PERSISTED computed column, value from JSON text will be materialized in this column. This way your queries can faster read value from JSON text because no parsing is needed. Each time JSON in this row changes, value will be re-calculated.

## Adding index on JSON path
Queries that filter or sort data by some value in JSON column usually use full table scan.

    SELECT * FROM ProductCollection
    WHERE JSON_VALUE(Data, '$.Color') = 'Black'

To optimize these kind of queries, you can add non-persisted computed column that exposes JSON expression used in filter or sort (in this example JSON_VALUE(Data, '$.Color')), and create index on this column:

    ALTER TABLE ProductCollection
    ADD vColor as JSON_VALUE(Data, '$.Color')
    
    CREATE INDEX idx_JsonColor
    ON ProductCollection(vColor)

Queries will use the index instead of plain table scan.

## JSON stored in in-memory tables
If you can use memory-optimized tables, you can store JSON as text:

    CREATE TABLE ProductCollection (
      Id int identity primary key nonclustered,
      Data nvarchar(max)
    ) WITH (MEMORY_OPTIMIZED=ON)

Advantages of JSON in in-memory:
 - JSON data is always in memory so there is no disk access
 - There are no locks and latches while working with JSON


