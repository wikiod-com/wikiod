---
title: "Database References"
slug: "database-references"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

SQL Server includes a feature called delayed naming which means that in many cases you can deploy procedure and code that references objects that do not exist. It is also possible to drop or alter an object in such a way that any pieces of referencing code no longer run when called. 

When you get either of theses two situations, you only know you have an issue when the code executes and fails.

SSDT helps with this by verifying that references to objects are valid when the project builds. This is one of the main benefits of SSDT and means that errors can be found at compile time rather than run time.

There are three types of `Database Reference` source:

 - Other projects in the same visual studio solution
 - Pre-built / supplied dacpacs for system databases (msdb and master)
 - Pre-built dacpacs for other databases / ssdt projects you create

Once you have a reference there are three different ways to use them in SSDT which map to the different ways we can reference objects in SQL Server:

- Same Database
- Different Database, Same Server
- Different Database, Different Server

This allows us to use these names:

- schema.table
- database.schema.table
- server.database.schema.table

This supports allowing:

- Different projects to make one database
- Cross database calls on the same server
- Cross database calls via linked servers

Database references are key to getting SSDT up and running, understand the different ways that they can be used



## Same Database Reference

The `Same Database` reference allows you to split a single database into multiple projects. This is useful for cases where a project is very large or where different teams manage different parts of the database.

If you consider that you have two .sqlproj SSDT database projects with the following structure:

Project1 - table_a
Project2 - proc_a

proc_a reads from table_a using the code:

    select column from table_a

If table_a is not in the same project, SSDT can not validate that `column` exists on the table. In this case a `same database` reference can be added to `Project2` that references the dacpac that is created by `Project1`

To add a `Same Database` reference you right click on the `References` folder in Solution Explorer and choose to add a `Database Reference`, you are then presented with the following dialog:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/aT3LD.png

Choose the source of the reference, i.e. another project in the same solution or a dacpac. Note a system database cannot be added as a `Same Database` reference but because of how SQL Server resolves objects you can still call those using the two part, schema and table name.

Once you have added the reference you can call objects in the referenced project using the standard 1 or 2 part name such as:

    select column from table_a

or

    select column from schema.table_a



## 3rd Party Database References
- Create a new SSDT project 
- Import 3rd party DB 
- Build project to create a dacpac 
- Reference dacpac in other projects

