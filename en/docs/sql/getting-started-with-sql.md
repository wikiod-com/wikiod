---
title: "Getting started with SQL"
slug: "getting-started-with-sql"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Overview
Structured Query Language (SQL) is a special-purpose programming language designed for managing data held in a Relational Database Management System (RDBMS).
SQL-like languages can also be used in Relational Data Stream Management Systems (RDSMS), or in "not-only SQL" (NoSQL) databases.

SQL comprises of 3 major sub-languages:
1. Data Definition Language (DDL): to create and modify the structure of the database;
2. Data Manipulation Language (DML): to perform Read, Insert, Update and Delete operations on the data of the database;
3. Data Control Language (DCL): to control the access of the data stored in the database.

[SQL article on Wikipedia](https://en.wikipedia.org/wiki/SQL)

The core DML operations are Create, Read, Update and Delete (CRUD for short) which are performed by the statements `INSERT`, `SELECT`, `UPDATE` and `DELETE`.  
There is also a (recently added) `MERGE` statement which can perform all 3 write operations (INSERT, UPDATE, DELETE).

[CRUD article on Wikipedia](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete)

---

Many SQL databases are implemented as client/server systems; the term "SQL server" describes such a database.  
At the same time, Microsoft makes a database that is named "SQL Server". While that database speaks a dialect of SQL, information specific to that database is not on topic in this tag but belongs into the [SQL Server documentation](https://www.wikiod.com/docs/sql-server).

