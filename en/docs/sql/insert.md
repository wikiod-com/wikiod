---
title: "INSERT"
slug: "insert"
draft: false
images: []
weight: 9898
type: docs
toc: true
---

## Syntax
 - INSERT INTO table_name (column1,column2,column3,...)
       VALUES (value1,value2,value3,...);
- INSERT INTO table_name (column1, column2...) SELECT value1, value2... from other_table

## INSERT data from another table using SELECT
    INSERT INTO Customers (FName, LName, PhoneNumber)
    SELECT FName, LName, PhoneNumber FROM Employees

This example will insert all [Employees][1] into the [Customers][2] table. Since the two tables have different fields and you don't want to move all the fields over, you need to set which fields to insert into and which fields to select. The correlating field names don't need to be called the same thing, but then need to be the same data type. This example is assuming that the Id field has an Identity Specification set and will auto increment.

If you have two tables that have exactly the same field names and just want to move all the records over you can use:

    INSERT INTO Table1
    SELECT * FROM Table2

[1]: https://www.wikiod.com/sql/example-databases-and-tables#Auto Shop Database
[2]: https://www.wikiod.com/sql/example-databases-and-tables

## Insert New Row
    INSERT INTO Customers
    VALUES ('Zack', 'Smith', 'zack@example.com', '7049989942', 'EMAIL');

This statement will insert a new row into the [`Customers`][1] table. Note that a value was not specified for the `Id` column, as it will be added automatically. However, all other column values must be specified.


[1]: https://www.wikiod.com/sql/example-databases-and-tables

## Insert Only Specified Columns
    INSERT INTO Customers (FName, LName, Email, PreferredContact)
    VALUES ('Zack', 'Smith', 'zack@example.com', 'EMAIL');

This statement will insert a new row into the [`Customers`][1] table. Data will only be inserted into the columns specified - note that no value was provided for the `PhoneNumber` column. Note, however, that all columns marked as `not null` must be included. 


[1]: https://www.wikiod.com/sql/example-databases-and-tables

## Insert multiple rows at once
Multiple rows can be inserted with a single insert command:

`INSERT INTO tbl_name
    (field1, field2, field3)`

`VALUES
    (1,2,3),
    (4,5,6),
    (7,8,9);`

For inserting large quantities of data (bulk insert) at the same time, DBMS-specific features and recommendations exist.

MySQL - [LOAD DATA INFILE][1]

MSSQL - [BULK INSERT][2]


  [1]: http://dev.mysql.com/doc/refman/5.7/en/load-data.html
  [2]: https://msdn.microsoft.com/en-us/library/ms188365.aspx

