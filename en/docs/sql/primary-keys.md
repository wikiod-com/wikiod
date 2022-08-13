---
title: "Primary Keys"
slug: "primary-keys"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

## Syntax
 - MySQL:
    CREATE TABLE Employees (
    Id int NOT NULL,
    PRIMARY KEY (Id),
    ...
);

 - Others: CREATE TABLE Employees (
    Id int NOT NULL PRIMARY KEY,
    ...
);

## Creating a Primary Key
    CREATE TABLE Employees (
        Id int NOT NULL,
        PRIMARY KEY (Id),
        ...
    );

This will create the Employees table with 'Id' as its primary key. The primary key can be used to uniquely identify the rows of a table. Only one primary key is allowed per table.

A key can also be composed by one or more fields, so called composite key, with the following syntax:

    CREATE TABLE EMPLOYEE (
        e1_id INT,
        e2_id INT,
        PRIMARY KEY (e1_id, e2_id)
    ) 



## Using Auto Increment
Many databases allow to make the primary key value automatically increment when a new key is added. This ensures that every key is different.

[**MySQL**](https://dev.mysql.com/doc/refman/5.7/en/create-table.html#create-table-types-attributes)

    CREATE TABLE Employees (
        Id int NOT NULL AUTO_INCREMENT,
        PRIMARY KEY (Id)
    );

[**PostgreSQL**](https://www.postgresql.org/docs/current/static/datatype-numeric.html#DATATYPE-SERIAL)

    CREATE TABLE Employees (
        Id SERIAL PRIMARY KEY
    );

[**SQL Server**](https://msdn.microsoft.com/en-us/library/ms186775.aspx)

    CREATE TABLE Employees (
        Id int NOT NULL IDENTITY,
        PRIMARY KEY (Id)
    );

[**SQLite**](http://www.sqlite.org/autoinc.html)

    CREATE TABLE Employees (
        Id INTEGER PRIMARY KEY    
    );

