---
title: "DROP Table"
slug: "drop-table"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

DROP TABLE removes the table definition from the schema along with the rows, indexes, permissions, and triggers.

## Check for existence before dropping
<!-- if version <MySQL> [gte 3.19] -->

    DROP TABLE IF EXISTS MyTable;

<!-- end version if -->

<!-- if version <PostgreSQL> [gte 8.x] -->

    DROP TABLE IF EXISTS MyTable;

<!-- end version if -->

<!-- if version <SQL Server> [gte 2005] -->

    If Exists(Select * From Information_Schema.Tables
              Where Table_Schema = 'dbo'
                And Table_Name = 'MyTable')
      Drop Table dbo.MyTable

<!-- end version if -->

<!-- if version <SQLite> [gte 3.0] -->

    DROP TABLE IF EXISTS MyTable;

<!-- end version if -->

## Simple drop
    Drop Table MyTable;

