---
title: "SQLCMD"
slug: "sqlcmd"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

You either need to be in the path where SQLCMD.exe exists or add it to your PATH environment variable.

## SQLCMD.exe called from a batch file or command line
    echo off
    
    cls
    
    sqlcmd.exe -S "your server name" -U "sql user name" -P "sql password" -d "name of databse" -Q "here you may write your query/stored procedure"

Batch files like these can be used to automate tasks, for example to make backups of databases at a specified time (can be scheduled with Task Scheduler) for a SQL Server Express version where Agent Jobs can't be used.


