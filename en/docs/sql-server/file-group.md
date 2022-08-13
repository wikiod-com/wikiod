---
title: "File Group"
slug: "file-group"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Create filegroup in database
We can create it by two way. First from database properties designer mode:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/IxpHU.png


And by sql scripts:


    USE master;
    GO
    -- Create the database with the default data
    -- filegroup and a log file. Specify the
    -- growth increment and the max size for the
    -- primary data file.
    
    CREATE DATABASE TestDB ON PRIMARY
    (
        NAME = 'TestDB_Primary',
        FILENAME = 'C:\Program Files\Microsoft SQL Server\MSSQL12.MSSQLSERVER\MSSQL\DATA\TestDB_Prm.mdf',
        SIZE = 1 GB,
        MAXSIZE = 10 GB,
        FILEGROWTH = 1 GB
    ), FILEGROUP TestDB_FG1
    (
        NAME = 'TestDB_FG1_1',
        FILENAME = 'C:\Program Files\Microsoft SQL Server\MSSQL12.MSSQLSERVER\MSSQL\DATA\TestDB_FG1_1.ndf',
        SIZE = 10 MB,
        MAXSIZE = 10 GB,
        FILEGROWTH = 1 GB
    ),
    (
        NAME = 'TestDB_FG1_2',
        FILENAME = 'C:\Program Files\Microsoft SQL Server\MSSQL12.MSSQLSERVER\MSSQL\DATA\TestDB_FG1_2.ndf',
        SIZE = 10 MB,
        MAXSIZE = 10 GB,
        FILEGROWTH = 1 GB
    ) LOG ON
    (
        NAME = 'TestDB_log',
        FILENAME = 'C:\Program Files\Microsoft SQL Server\MSSQL12.MSSQLSERVER\MSSQL\DATA\TestDB.ldf',
        SIZE = 10 MB,
        MAXSIZE = 10 GB,
        FILEGROWTH = 1 GB
    );
    
    go 
    ALTER DATABASE TestDB MODIFY FILEGROUP TestDB_FG1 DEFAULT;
    go
    
    -- Create a table in the user-defined filegroup.
    USE TestDB;
    Go
    
    CREATE TABLE MyTable
    (
        col1 INT PRIMARY KEY,
        col2 CHAR(8)
    )
    ON TestDB_FG1;
    GO



