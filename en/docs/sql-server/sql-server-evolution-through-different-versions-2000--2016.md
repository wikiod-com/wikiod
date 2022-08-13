---
title: "SQL Server Evolution through different versions (2000 - 2016)"
slug: "sql-server-evolution-through-different-versions-2000---2016"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

I am using SQL Server since 2004. I started with 2000 and now I am going to use SQL Server 2016. I created tables, views, functions, triggers, stored procedures and wrote many SQL queries but I did not use many new features from subsequent versions. I googled it but unfortunately, I did not find all the features in one place. So I gathered and validated these information from different sources and put here. I am just adding the high level information for all the versions starting from 2000 to 20

## SQL Server Version 2000 - 2016
**The following features added in SQL Server 2000 from its previous version:**


1.    New data types were added (BIGINT, SQL_VARIANT, TABLE)
2.    Instead of and for Triggers were introduced as advancement to the DDL.
3.    Cascading referential integrity.
4.    XML support
5.    User defined functions and partition views.
6.    Indexed Views (Allowing index on views with computed columns).

**The following features added in version 2005 from its previous version:**

1.    Enhancement in TOP clause with “WITH TIES” option.
2.    Data Manipulation Commands (DML) and OUTPUT clause to get INSERTED and DELETED values
3.    The PIVOT and UNPIVOT operators.
4.    Exception Handling with TRY/CATCH block
5.    Ranking functions
6.    Common Table Expressions (CTE)
7.    Common Language Runtime (Integration of .NET languages to build objects like stored procedures, triggers, functions etc.)
8.    Service Broker (Handling message between a sender and receiver in a loosely coupled manner)
9.    Data Encryption (Native capabilities to support encryption of data stored in user defined databases)
10.    SMTP mail
11.    HTTP endpoints (Creation of endpoints using simple T-SQL statement exposing an object to be accessed over the internet)
12.    Multiple Active Result Sets (MARS).This allows a persistent database connection from a single client to have more than one active request per connection.
13.    SQL Server Integration Services (Will be used as a primary ETL (Extraction, Transformation and Loading) Tool
14.    Enhancements in Analysis Services and Reporting Services.
15.    Table and index partitioning. Allows partitioning of tables and indexes based on partition boundaries as specified by a PARTITION FUNCTION with individual partitions mapped to file groups via a PARTITION SCHEME.

**The following features added in version 2008 from its previous version:**

1.    Enhancement in existing DATE and TIME Data Types
2.    New functions like – SYSUTCDATETIME() and SYSDATETIMEOFFSET()
3.    Spare Columns – To save a significant amount of disk space.
4.    Large User Defined Types (up to 2 GB in size)
5.    Introduced a new feature to pass a table datatype into stored procedures and functions
6.    New MERGE command for INSERT, UPDATE and DELETE operations
7.    New HierarchyID datatype
8.    Spatial datatypes - To represent the physical location and shape of any geometric object.
9.    Faster queries and reporting with GROUPING SETS - An extension to the GROUP BY clause.
10.    Enhancement to FILESTREAM storage option

**The following features added in version 2008 R2 from its previous version:**

1.    PowerPivot – For processing large data sets.
2.    Report Builder 3.0
3.    Cloud ready
4.    StreamInsight
5.    Master Data Services
6.    SharePoint Integration
7.    DACPAC (Data-tier Application Component Packages)
8.    Enhancement in other features of SQL Server 2008

**The following features added in version 2012 from its previous version:**

1.    Column store indexes - reduces I/O and memory utilization on large queries.
2.    Pagination - pagination can be done by using “OFFSET” and “FETCH’ commands.
3.    Contained database – Great feature for periodic data migrations.
4.    AlwaysOn Availability Groups
5.    Windows Server Core Support
6.    User-Defined Server Roles
7.    Big Data Support
8.    PowerView
9.    SQL Azure Enhancements
10.    Tabular Model (SSAS)
11.    DQS Data quality services
12.    File Table - an enhancement to the FILESTREAM feature which was introduced in 2008.
13.    Enhancement in Error Handling including THROW statement
14.    Improvement to SQL Server Management Studio Debugging
a.    SQL Server 2012 introduces more options to control breakpoints.
b.    Improvements to debug-mode windows    
c.    Enhancement in IntelliSense - like Inserting Code Snippets.

**The following features added in version 2014 from its previous version:**

1.    In-Memory OLTP Engine – Improves performance up to 20 times.
2.    AlwaysOn Enhancements
3.    Buffer Pool Extension
4.    Hybrid Cloud Features
5.    Enhancement in Column store Indexes (like Updatable Column store Indexes)
6.    Query Handling Enhancements (like parallel SELECT INTO)
7.    Power BI for Office 365 Integration
8.    Delayed durability
9.    Enhancements for Database Backups

**The following features added in version 2016 from its previous version:**

1.    Always Encrypted - Always Encrypted is designed to protect data at rest or in motion.
2.    Real-time Operational Analytics
3.    PolyBase into SQL Server
4.    Native JSON Support
5.    Query Store
6.    Enhancements to AlwaysOn
7.    Enhanced In-Memory OLTP
8.    Multiple TempDB Database Files
9.    Stretch Database
10.    Row Level Security
11.    In-Memory Enhancements

> T-SQL Enhancements or new additions in SQL Server 2016

 1. TRUNCATE TABLE with PARTITION
 2. DROP IF EXISTS
 3. STRING_SPLIT and STRING_ESCAPE Functions
 4. ALTER TABLE can now alter many columns while the table remains online, using WITH (ONLINE = ON | OFF).

 5. MAXDOP for DBCC CHECKDB, DBCC CHECKTABLE and DBCC CHECKFILEGROUP
 6. ALTER DATABASE SET AUTOGROW_SINGLE_FILE    
 7. ALTER DATABASE SET AUTOGROW_ALL_FILES
 8. COMPRESS and DECOMPRESS Functions
 9. FORMATMESSAGE Statement
 10. 2016 introduces 8 more properties with SERVERPROPERTY 

   a.    InstanceDefaultDataPath

   b.    InstanceDefaultLogPath

   c.    ProductBuild

   d.    ProductBuildType

   e.    ProductMajorVersion

   f.    ProductMinorVersion

   g.    ProductUpdateLevel

   h.    ProductUpdateReference



