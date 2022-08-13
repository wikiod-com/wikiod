---
title: "DROP or DELETE Database"
slug: "drop-or-delete-database"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
- MSSQL Syntax:
- DROP DATABASE [ IF EXISTS ] { database_name | database_snapshot_name } [ ,...n ] [;] 
- MySQL Syntax:
- DROP {DATABASE | SCHEMA} [IF EXISTS] db_name

`DROP DATABASE` is used for dropping a database from SQL. Be sure to create a backup of your database before dropping it to prevent accidental loss of information.

## DROP Database
Dropping the database is a simple one-liner statement. Drop database will delete the database, hence always ensure to have a backup of the database if required.

Below is the command to drop Employees Database

    DROP DATABASE [dbo].[Employees]
    

