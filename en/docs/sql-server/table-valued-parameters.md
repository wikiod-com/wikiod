---
title: "Table Valued Parameters"
slug: "table-valued-parameters"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Table valued parameters (TVP for short) are parameters passed to a stored procedure or function that contains data that is table structured. 
Using table valued parameters requires creating a [user defined table type][1] for the parameter being used.

Tabled valued parameters are readonly parameters. 


  [1]: https://www.wikiod.com/sql-server/user-defined-table-types

## Using a table valued parameter to insert multiple rows to a table
First, define a [used defined table type][1] to use:

    CREATE TYPE names as TABLE
    (
        FirstName varchar(10),
        LastName varchar(10)
    )
    GO

Create the stored procedure:

    CREATE PROCEDURE prInsertNames
    (
        @Names dbo.Names READONLY -- Note: You must specify the READONLY
    )
    AS

    INSERT INTO dbo.TblNames (FirstName, LastName)
    SELECT FirstName, LastName
    FROM @Names 
    GO

Executing the stored procedure:

    DECLARE @names dbo.Names
    INSERT INTO @Names VALUES
    ('Zohar', 'Peled'),
    ('First', 'Last')

    EXEC dbo.prInsertNames @Names


  [1]: https://www.wikiod.com/sql-server/user-defined-table-types

