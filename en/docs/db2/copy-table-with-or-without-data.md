---
title: "Copy table with or without data"
slug: "copy-table-with-or-without-data"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Example of how to copy existing table structure with/without data

## Syntax
 1. CREATE TABLE schemaName.table AS (SELECT columns FROM
    schemaName.table) WITH DATA

## Copy Table With Data
    CREATE TABLE myschema.tableNew AS (
        SELECT *
        FROM myschema.tableOld
    ) WITH  DATA

## Copy Table without data
    CREATE TABLE myschema.tableNew AS (
        SELECT *
        FROM myschema.tableOld
    ) WITHOUT  DATA

## Copy Table with where clause
    CREATE TABLE myschema.tableNew AS (
        SELECT *
        FROM myschema.tableOld
    WHERE column1 = 'myCriteria'
    ) WITH  DATA

