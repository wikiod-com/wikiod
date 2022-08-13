---
title: "GRANT and REVOKE"
slug: "grant-and-revoke"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Syntax
 - GRANT [privilege1] [, [privilege2] ... ] ON [table] TO [grantee1] [, [grantee2] ... ] [ WITH GRANT OPTION ]
- REVOKE [privilege1] [, [privilege2] ... ] ON [table] FROM [grantee1] [, [grantee2] ... ] 

Grant permissions to users. If the `WITH GRANT OPTION` is specified, the grantee additionally gains the privilege to grant the given permission or revoke previously granted permissions.

## Grant/revoke privileges
    GRANT SELECT, UPDATE
    ON Employees
    TO User1, User2;

Grant `User1` and `User2` permission to perform `SELECT` and `UPDATE` operations on table `Employees`. 

----

    REVOKE SELECT, UPDATE
    ON Employees
    FROM User1, User2;

Revoke from `User1` and `User2` the permission to perform `SELECT` and `UPDATE` operations on table Employees.

