---
title: "Advanced options"
slug: "advanced-options"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

## Enable and show advanced options
    Exec sp_configure 'show advanced options' ,1
    RECONFIGURE
    GO
    -- Show all configure
    sp_configure

## Enable backup compression default
    Exec sp_configure 'backup compression default',1
    GO  
    RECONFIGURE;

## Enable cmd permission 
    EXEC sp_configure 'xp_cmdshell', 1
    GO
    RECONFIGURE
 

## Set default fill factor percent 
    sp_configure 'fill factor', 100;  
    GO  
    RECONFIGURE;  

The server must be restarted before the change can take effect.

## Set system recovery interval
    USE master;  
    GO 
    -- Set recovery every 3 min
    EXEC sp_configure 'recovery interval', '3';  
    RECONFIGURE WITH OVERRIDE;  

## Set max server memory size
    USE master
    EXEC sp_configure 'max server memory (MB)', 64
    RECONFIGURE WITH OVERRIDE

## Set number of checkpoint tasks
    
    EXEC sp_configure "number of checkpoint tasks", 4

