---
title: "Last Inserted Identity"
slug: "last-inserted-identity"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## SCOPE_IDENTITY()
    CREATE TABLE dbo.logging_table(log_id INT IDENTITY(1,1) PRIMARY KEY, 
                                   log_message VARCHAR(255))
    
    CREATE TABLE dbo.person(person_id INT IDENTITY(1,1) PRIMARY KEY, 
                            person_name VARCHAR(100) NOT NULL)
    GO;
    
    CREATE TRIGGER dbo.InsertToADifferentTable ON dbo.person  
    AFTER INSERT  
    AS
        INSERT INTO dbo.logging_table(log_message)
        VALUES('Someone added something to the person table')
    GO;
    
    INSERT INTO dbo.person(person_name)
    VALUES('John Doe')  

    SELECT SCOPE_IDENTITY();

This will return the most recently added identity value produced on the same connection, within the current scope. In this case, 1, for the first row in the dbo.person table.

## @@IDENTITY
    CREATE TABLE dbo.logging_table(log_id INT IDENTITY(1,1) PRIMARY KEY, 
                                   log_message VARCHAR(255))

    CREATE TABLE dbo.person(person_id INT IDENTITY(1,1) PRIMARY KEY, 
                            person_name VARCHAR(100) NOT NULL)
    GO;

    CREATE TRIGGER dbo.InsertToADifferentTable ON dbo.person  
    AFTER INSERT  
    AS
        INSERT INTO dbo.logging_table(log_message)
        VALUES('Someone added something to the person table')
    GO;
    
    INSERT INTO dbo.person(person_name)
    VALUES('John Doe')    
    
    SELECT @@IDENTITY;

This will return the most recently-added identity on the same connection, regardless of scope. In this case, whatever the current value of the identity column on logging_table is, assuming no other activity is occurring on the instance of SQL Server and no other triggers fire from this insert.

## IDENT_CURRENT('tablename')
    SELECT IDENT_CURRENT('dbo.person');

This will select the most recently-added identity value on the selected table, regardless of connection or scope.

## @@IDENTITY and MAX(ID)


