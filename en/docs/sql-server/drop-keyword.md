---
title: "Drop Keyword"
slug: "drop-keyword"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

The Drop keyword can be used with various SQL objects, this topic provides quick examples of different usage with database objects.

Links to MSDN.

 - [DROP TABLE (Transact-SQL)][1]
  
 - [DROP PROCEDURE (Transact-SQL)][2]
   
 - [DROP DATABASE (Transact-SQL)][3]

  [1]: https://msdn.microsoft.com/en-us/library/ms173790.aspx
  [2]: https://msdn.microsoft.com/en-us/library/ms174969.aspx
  [3]: https://msdn.microsoft.com/en-us/library/ms178613.aspx

## Drop temporary tables
In SQL server we have 2 types of temporary tables: 
 1. `##GlobalTempTable` is a type of temporary table that is sheered between all user's sessions.
 2. `#LocalTempTable` temp tab - it is a type of temporary table that only exists in current scope (only in actual process - you can get id of your current process by `SELECT @@SPID`)

Droping process of temporary tables is the same as for normal table:

    DROP TABLE [ database_name . [ schema_name ] . | schema_name . ] table_name   


> BEFORE SQL Server 2016:

    IF(OBJECT_ID('tempdb..#TempTable') is not null)
        DROP TABLE #TempTable;


> SQL Server 2016:

    DROP TABLE IF EXISTS #TempTable
    

## Drop tables
The **DROP TABLE** command remove the table definitions and all data, indexes, triggers, constraints and related permissions.
 
Before you drop a table, you should check if there are any object (views, stored procedures, other tables) that reference the table.
 
You cannot drop a table referenced by another table by FOREIGN KEY. You must first drop the FOREIGN KEY referencing it.
 
You can drop a table referenced by a view or stored procedure, but after dropping the table, the view or stored procedure is no longer usable.
 
The Syntax
 
    DROP TABLE [ IF EXISTS ] [ database_name . [ schema_name ] . | schema_name . ]
    table_name [ ,...n ] [ ; ]
 
- `IF EXISTS` - Drop the table only if exists
- `database_name` - Specify the name of the database where the table is contained
- `schema_name` - Specify the name of the schema where the table is under
- `table_name` - Specify the name of the table to be dropped
 
**Examples**
 
Remove the table with name **TABLE_1** from current database and default schema dbo
 
    DROP TABLE Table_1;
 
Remove the table with **TABLE_1** from database **HR** and default schema dbo
 
    DROP TABLE HR.Table_1;
 
Remove the table with **TABLE_1** from database **HR** and schema **external**
 
    DROP TABLE HR.external.TABLE_1;

## Drop Databases

The **DROP DATABASE** command removes a database catalog, regardless of its state (offline, read-only, suspect, etc.), from the current SQL Server instance. 

A database cannot be dropped if there are any database snapshots associated with it, as the database snapshots must be dropped first.

A database drop removes all of the physical disk files (unless it's offline) used by the database unless you use the Stored Procedure 'sp_detach_db'.

A database snapshot drop deletes the snapshot from the SQL Server instance and deletes the physical files also used by it.

A dropped database can only be re-created by restoring a backup (not from a database snapshot either).

The Syntax

    DROP DATABASE [ IF EXISTS ] { database_name | database_snapshot_name } [ ,...n ] [;]  

 - `IF EXISTS` - Drop the table only if exists
 - `database_name` - Specifies the name of the database to drop
 - `database_snapshot_name` - Specifies the database snapshot to remove
 - 

**Examples**

Remove a single database;

    DROP DATABASE Database1;

Removing multiple databases

    DROP DATABASE Database1, Database2;

Removing a snapshot

    DROP DATABASE Database1_snapshot17;

Removing if database exists
    
    DROP DATABASE IF EXISTS Database1;

