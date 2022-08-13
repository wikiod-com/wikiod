---
title: "DUAL table"
slug: "dual-table"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

`DUAL` table has one column `DUMMY`, defined to be `VARCHAR2(1)` and only one row with a value `x`.

`DUAL` table is automatically created in `SYS` schema when database is created. You can access it from any schema.

You can not change `DUAL` table.

You can use `DUAL` table to call any function from SQL statement. It is useful because it has only one row and oracle optimizer knows everything about it. 

## The following example returns the current operating system date and time
    select sysdate from dual 

## The following example generates numbers between  start_value and end_value
    select :start_value + level -1 n
    from dual       
    connect by level <= :end_value  - :start_value + 1  

