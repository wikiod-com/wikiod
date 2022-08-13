---
title: "Retrieve Information about your Instance"
slug: "retrieve-information-about-your-instance"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Retrieve Local and Remote Servers
To retrieve a list of all servers registered on the instance:

    EXEC sp_helpserver;

## Get information on current sessions and query executions
    
    sp_who2

This procedure can be used to find information on current SQL server sessions.  Since it is a procedure, it's often helpful to store the results into a temporary table or table variable so one can order, filter, and transform the results as needed.

The below can be used for a queryable version of `sp_who2`:

    -- Create a variable table to hold the results of sp_who2 for querying purposes

    DECLARE @who2 TABLE (
          SPID INT NULL,
          Status VARCHAR(1000) NULL,
          Login SYSNAME NULL,
          HostName SYSNAME NULL,
          BlkBy SYSNAME NULL,
          DBName SYSNAME NULL,
          Command VARCHAR(8000) NULL,
          CPUTime INT NULL,
          DiskIO INT NULL,
          LastBatch VARCHAR(250) NULL,
          ProgramName VARCHAR(250) NULL,
          SPID2 INT NULL, -- a second SPID for some reason...?
          REQUESTID INT NULL
    )
    
    INSERT INTO @who2
    EXEC sp_who2
    
    SELECT    *
    FROM    @who2 w
    WHERE    1=1

Examples:

    -- Find specific user sessions:
    SELECT  *
    FROM    @who2 w
    WHERE   1=1
        and  login = 'userName'

    -- Find longest CPUTime queries:
    SELECT  top 5 *
    FROM    @who2 w
    WHERE   1=1
    order   by CPUTime desc

## Retrieve Edition and Version of Instance
    SELECT    SERVERPROPERTY('ProductVersion') AS ProductVersion,  
              SERVERPROPERTY('ProductLevel') AS ProductLevel,  
              SERVERPROPERTY('Edition') AS Edition,  
              SERVERPROPERTY('EngineEdition') AS EngineEdition;  



## Retrieve Instance Uptime in Days
    SELECT  DATEDIFF(DAY, login_time, getdate()) UpDays
    FROM    master..sysprocesses 
    WHERE   spid = 1

## Information about SQL Server version 
To discover SQL Server's edition, product level and version number as well as the host machine name and the server type:

    SELECT    SERVERPROPERTY('MachineName') AS Host,
              SERVERPROPERTY('InstanceName') AS Instance,
              DB_NAME() AS DatabaseContext,
              SERVERPROPERTY('Edition') AS Edition, 
              SERVERPROPERTY('ProductLevel') AS ProductLevel, 
              CASE SERVERPROPERTY('IsClustered') 
                WHEN 1 THEN 'CLUSTERED' 
                ELSE 'STANDALONE' END AS ServerType,
              @@VERSION AS VersionNumber;

## General Information about Databases, Tables, Stored procedures and how to search them.

**Query to  search last executed sp's in db**

    SELECT execquery.last_execution_time AS [Date Time], execsql.text AS [Script]
    FROM sys.dm_exec_query_stats AS execquery
    CROSS APPLY sys.dm_exec_sql_text(execquery.sql_handle) AS execsql
    ORDER BY execquery.last_execution_time DESC

**Query to  search through Stored procedures**

    SELECT o.type_desc AS ROUTINE_TYPE,o.[name] AS ROUTINE_NAME,
    m.definition AS ROUTINE_DEFINITION
    FROM sys.sql_modules AS m INNER JOIN sys.objects AS o
    ON m.object_id = o.object_id WHERE m.definition LIKE '%Keyword%'
    order by ROUTINE_NAME


**Query to Find Column From All Tables of Database**

    SELECT t.name AS table_name,
    SCHEMA_NAME(schema_id) AS schema_name,
    c.name AS column_name
    FROM sys.tables AS t
    INNER JOIN sys.columns c ON t.OBJECT_ID = c.OBJECT_ID
    where c.name like 'Keyword%'
    ORDER BY schema_name, table_name;

**Query to  to check restore details**

    WITH LastRestores AS
    (
    SELECT
        DatabaseName = [d].[name] ,
        [d].[create_date] ,
        [d].[compatibility_level] ,
        [d].[collation_name] ,
        r.*,
        RowNum = ROW_NUMBER() OVER (PARTITION BY d.Name ORDER BY r.[restore_date] DESC)
    FROM master.sys.databases d
    LEFT OUTER JOIN msdb.dbo.[restorehistory] r ON r.[destination_database_name] = d.Name
    )
    SELECT *
    FROM [LastRestores]
    WHERE [RowNum] = 1

**Query to  to find the log**

    select top 100 * from databaselog
    Order by Posttime desc

**Query to  to check the Sps details**

    SELECT name, create_date, modify_date
    FROM sys.objects
    WHERE type = 'P'
    Order by modify_date desc



