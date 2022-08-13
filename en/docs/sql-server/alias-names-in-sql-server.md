---
title: "Alias Names in Sql Server"
slug: "alias-names-in-sql-server"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Here is some of different ways to provide alias names to columns in Sql Server

## Using AS
This is ANSI SQL method works in all the RDBMS. Widely used approach. 

    CREATE TABLE AliasNameDemo (id INT,firstname VARCHAR(20),lastname VARCHAR(20)) 
    
    INSERT INTO AliasNameDemo
    VALUES      (1,'MyFirstName','MyLastName') 
    
    SELECT FirstName +' '+ LastName As FullName
    FROM   AliasNameDemo



## Using =
This is my preferred approach. Nothing related to performance just a personal choice. It makes the code to look clean. You can see the resulting column names easily instead of scrolling the code if you have a big expression.

    CREATE TABLE AliasNameDemo (id INT,firstname VARCHAR(20),lastname VARCHAR(20)) 
    
    INSERT INTO AliasNameDemo
    VALUES      (1,'MyFirstName','MyLastName') 
    
    SELECT FullName = FirstName +' '+ LastName
    FROM   AliasNameDemo

## Giving alias after Derived table name
This is a weird approach most of the people don't know this even exist. 

    CREATE TABLE AliasNameDemo(id INT,firstname VARCHAR(20),lastname VARCHAR(20)) 
    
    INSERT INTO AliasNameDemo
    VALUES      (1,'MyFirstName','MyLastName') 
    
    SELECT * 
    FROM   (SELECT firstname + ' ' + lastname 
            FROM   AliasNameDemo) a (fullname) 

 - **[Demo][1]**


  [1]: http://rextester.com/YEWR15418

## Without using AS
This syntax will be similar to using `AS` keyword. Just we don't have to use `AS` keyword

    CREATE TABLE AliasNameDemo (id INT,firstname VARCHAR(20),lastname VARCHAR(20)) 
    
    INSERT INTO AliasNameDemo
    VALUES      (1,'MyFirstName','MyLastName') 
    
    SELECT FirstName +' '+ LastName FullName
    FROM   AliasNameDemo

