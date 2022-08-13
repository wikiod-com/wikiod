---
title: "T-SQL script for creating Alfresco database in SQLServer 2008 - 2014"
slug: "t-sql-script-for-creating-alfresco-database-in-sqlserver-2008---2014"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

It's useful to have a T-SQL script for creating and configuring a new database and user for Alfresco Installation purposes.
It's boring and time-consuming to jump around many pages on MSDN.

I provide the script hereafter:

Guidelines for preparing Alfresco Instalation to a SQLServer database, can be read here:
- [http://docs.alfresco.com/3.4/tasks/sqlserver-config.html][1]


  [1]: http://docs.alfresco.com/3.4/tasks/sqlserver-config.html

## T-SQL_script_4_alfresco
    /* creates a database for Alfresco, on SQLServer 2008- 2014 */
    use master;
    GO
    CREATE DATABASE alfresco;
    GO
    /* creates a new LOGIN and associated User
    use alfresco;
    GO
    CREATE LOGIN alfresco WITH PASSWORD = 'alfresco';  
    GO
    use alfresco;
    go
    CREATE USER alfresco FOR LOGIN alfresco;  
    GO
    
    /* Now try to add alfresco user to the db_owner Role 
       NOTICE: coinnect to alfresco database before 
       you can also connect as a local Windows user,
       in order to successfully execute the followings:  */
    use alfresco;
    GO
    EXEC sp_addrolemember N'db_owner', N'alfresco';
    GO
    
    /* sets Isolation level */
    ALTER DATABASE alfresco SET ALLOW_SNAPSHOT_ISOLATION ON; 
    GO
    
    /* creates and sets the alfesco schema as the default one */
    use alfresco;
    go
    CREATE SCHEMA alfresco AUTHORIZATION alfresco;
    GO
    ALTER USER alfresco WITH DEFAULT_SCHEMA = alfresco;
    GO
    
    /* tests table creation */
    drop table _buttamiVia_; 
    GO 
    create table _buttamiVia_ 
    ( id int not null ); 
    GO



