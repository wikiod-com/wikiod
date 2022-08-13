---
title: "System database - TempDb"
slug: "system-database---tempdb"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Identify TempDb usage
Following query will provide information about TempDb usage. Analyzing the counts you can identify which thing is impacting TempDb  

    SELECT
     SUM (user_object_reserved_page_count)*8 as usr_obj_kb,
     SUM (internal_object_reserved_page_count)*8 as internal_obj_kb,
     SUM (version_store_reserved_page_count)*8  as version_store_kb,
     SUM (unallocated_extent_page_count)*8 as freespace_kb,
     SUM (mixed_extent_page_count)*8 as mixedextent_kb
    FROM sys.dm_db_file_space_usage

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/JsNnB.png

## TempDB database details
Below query can be used to get TempDB database details:

    USE [MASTER]
    SELECT * FROM sys.databases WHERE database_id = 2
OR

    USE [MASTER]
    SELECT * FROM sys.master_files WHERE database_id = 2

With the help of below DMV, you can check how much TempDb space does your session is using. This query is quite helpful while debugging TempDb issues

    SELECT * FROM sys.dm_db_session_space_usage WHERE session_id = @@SPID

