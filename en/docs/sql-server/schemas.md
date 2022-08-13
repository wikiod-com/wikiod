---
title: "Schemas"
slug: "schemas"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Purpose
Schema refers to a specific database tables and how they are related to each other. It provides an organisational blueprint of how the database is constructed. Additional benefits of implementing database schemas is that schemas can be used as a method restricting / granting access to specific tables within a database.  

## Creating a Schema
    CREATE SCHEMA dvr AUTHORIZATION Owner
        CREATE TABLE sat_Sales (source int, cost int, partid int)
        GRANT SELECT ON SCHEMA :: dvr TO  User1
        DENY SELECT ON SCHEMA :: dvr to User 2
    GO

## Alter Schema
    ALTER SCHEMA dvr
        TRANSFER dbo.tbl_Staging;
    GO

This would transfer the tbl_Staging table from the dbo schema to the dvr schema


## Dropping Schemas
    DROP SCHEMA dvr

