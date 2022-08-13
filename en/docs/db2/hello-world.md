---
title: "Hello World;"
slug: "hello-world"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Creating a database in DB2
    CREATE DATABASE SAMPLEDB;

This will create a new database called sampledb.


## Connecting to a database in DB2
    CONNECT TO SAMPLEDB;

From the command line (db2clp, terminal, db2cmd) you can write:

    db2 CONNECT TO SAMPLEDB

## Create a table in DB2 called "employee"
The following statement will create a new table called employee:

    CREATE TABLE EMPLOYEE (
           EMPNO      CHAR(6)        NOT NULL,
           FIRSTNME   VARCHAR(12)    NOT NULL,
           LASTNAME   VARCHAR(15)    NOT NULL,
           SALARY     DECIMAL(9,2)           ,
           PRIMARY KEY (EMPNO)      
           )

This will create a new table called employee. The table will have a primary key on `EMPNO` column. The first three columns cannot have a null value and they are text. The fourth one can have nulls and it is a number.

You can create this table from db2clp (Linux, UNIX, MacOS) like this (by surrounding the statement into quotes):

    db2 "CREATE TABLE EMPLOYEE (
           EMPNO      CHAR(6)        NOT NULL,
           FIRSTNME   VARCHAR(12)    NOT NULL,
           LASTNAME   VARCHAR(15)    NOT NULL,
           SALARY     DECIMAL(9,2)           ,
           PRIMARY KEY (EMPNO)      
           )"

In Linux/UNIX, you can also escape the special characters with back-slash, but this could be more difficult to write:

    db2    CREATE TABLE EMPLOYEE \( \
           EMPNO      CHAR\(6\)        NOT NULL, \
           FIRSTNME   VARCHAR\(12\)    NOT NULL, \
           LASTNAME   VARCHAR\(15\)    NOT NULL, \
           SALARY     DECIMAL\(9,2\)           , \
           PRIMARY KEY \(EMPNO\)                 \
           \)


## Inserting a row into a DB2 table
Let's suppose we are going to insert rows in the previously created table.

We can explicitly name the columns we are going to out values is and its order:

    INSERT INTO EMPLOYEE (EMPNO, FIRSTNME, LASTNAME, SALARY)
      VALUES ( '123456', 'Ali', 'Veli', 100000);

If we know the order and we are going to put values for all columns we can write:

    INSERT INTO EMPLOYEE
      VALUES ( '123456', 'Ali', 'Veli', 100000);

When using the db2clp, we need to put quotes because of the parenthesis (without semicolon at the end):

    db2 "INSERT INTO EMPLOYEE (EMPNO, FIRSTNME, LASTNAME, SALARY)
      VALUES ( '123456', 'Ali', 'Veli', 100000)"


## sample select query;
    SELECT 'HELLO WORLD' FROM SYSIBM.SYSDUMMY1;    

    1          
    -----------
    Hello World
    
      1 record(s) selected.



"The SYSIBM.SYSDUMMY1 table contains one row. The table is used for SQL statements in which a table reference is required, but the contents of the table are not important"

this table has only one column. Column Name is IBMREQD. Default value is Y. 

    SELECT * FROM SYSIBM.SYSDUMMY1;
    IBMREQD
    -------
    Y      
    
      1 record(s) selected.


