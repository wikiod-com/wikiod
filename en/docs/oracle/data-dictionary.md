---
title: "Data Dictionary"
slug: "data-dictionary"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

The data dictionary views, also known as catalog views, let you monitor the state of the database in real time:

The views prefixed with `USER_`, `ALL_`, and `DBA_`, show information about schema objects that are owned by you (`USER_`), accessible by you (`ALL_`) or accessible by a user with SYSDBA privilege (`DBA_`).  For example, the view `ALL_TABLES` shows all tables that you have privileges on.

The `V$` views show performance-related information.

The `_PRIVS` views show privilege information for different combinations of users, roles, and objects.

[Oracle documentation: Catalog Views / Data Dictionary Views][1]


  [1]: http://docs.oracle.com/cd/B28359_01/nav/catalog_views.htm

## To see all the data dictionary views to which you have access
    select * from dict

## Text source of the stored objects
`USER_SOURCE` describes the text source of the stored objects owned by the current user. This view does not display the `OWNER` column.

    select * from user_source where type='TRIGGER' and lower(text) like '%order%' 
    
`ALL_SOURCE` describes the text source of the stored objects accessible to the current user.

    select * from all_source where owner=:owner
    
`DBA_SOURCE` describes the text source of all stored objects in the database.

    select * from dba_source

## Get list of all tables in Oracle
    select owner, table_name
    from all_tables

`ALL_TAB_COLUMNS` describes the columns of the tables, views, and clusters accessible to the current user. `COLS` is a synonym for `USER_TAB_COLUMNS`.


    select *
    from all_tab_columns
    where table_name = :tname



## Privilege information

All roles granted to user.

    select * 
    from dba_role_privs 
    where grantee= :username

Privileges granted to user:
1) system privileges


    select * 
    from dba_sys_privs 
    where grantee = :username

2) object grants


    select * 
    from dba_tab_privs 
    where grantee = :username

**Permissions granted to roles.**

Roles granted to other roles.

    select * 
    from role_role_privs 
    where role in (select granted_role from dba_role_privs where grantee= :username)

1) system privileges


    select * 
    from role_sys_privs  
    where role in (select granted_role from dba_role_privs where grantee= :username)

2) object grants


    select * 
    from role_tab_privs  
    where role in (select granted_role from dba_role_privs where grantee= :username)

## Oracle version
    select * 
    from v$version

## Describes all objects in the database.
    select * 
    from dba_objects

