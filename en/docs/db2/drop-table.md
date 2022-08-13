---
title: "Drop table"
slug: "drop-table"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Basic instructions to drop a table in DB2.

## Basic Drop Table Syntax
    db2 connect to {databaseName}
    db2 drop table {schema}.{table}
    db2 connect reset

The schema is not necessary if it matches the current user name.
The "db2" prefix is not necessary if you are already in a DB2 command prompt.

