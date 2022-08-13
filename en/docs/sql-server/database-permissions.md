---
title: "Database permissions"
slug: "database-permissions"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Basic Syntax:

    {GRANT| REVOKE | DENY} {PERMISSION_NAME} [ON {SECURABLE}] TO {PRINCIPAL};

 - {GRANT| REVOKE | DENY} - What you're trying to accomplish
   - Grant: "Give this permission to the stated principal"
   - Revoke: "Take this permission away from the stated principal"
   - Deny: "Make sure the stated principal never has this permission (i.e. "`DENY SELECT`" means that regardless of any other permissions, `SELECT` will fail for this principal)
 - PERMISSION_NAME - The operation that you're attempting to affect. This will depend on the securable. For instance, it doesn't make sense to `GRANT SELECT` on a stored procedure.
 - SECURABLE - The name of the thing on which you're trying to affect permissions on. This is *optional*. Saying `GRANT SELECT TO [aUser];` is perfectly acceptable; it means "for any securable for which the `SELECT` permission makes sense, `GRANT` that permission".
 - PRINCIPAL - For whom you are trying to affect permissions. At a database level, this can be a role (application or database) or user (mapped to a login or not) for example.

## Changing permissions
    GRANT SELECT ON [dbo].[someTable] TO [aUser];

    REVOKE SELECT ON [dbo].[someTable] TO [aUser];
    --REVOKE SELECT [dbo].[someTable] FROM [aUser]; is equivalent
    
    DENY SELECT ON [dbo].[someTable] TO [aUser];





## CREATE USER
    --implicitly map this user to a login of the same name as the user
    CREATE USER [aUser];

    --explicitly mapping what login the user should be associated with
    CREATE USER [aUser] FOR LOGIN [aUser];

## CREATE ROLE
    CREATE ROLE [myRole];

## Changing role membership
    -- SQL 2005+
    exec sp_addrolemember @rolename = 'myRole', @membername = 'aUser';
    exec sp_droprolemember @rolename = 'myRole', @membername = 'aUser';
    
    -- SQL 2008+
    ALTER ROLE [myRole] ADD MEMBER [aUser];
    ALTER ROLE [myRole] DROP MEMBER [aUser];

Note: role members can be any database-level principal. That is, you can add a role as a member in another role. Also, adding/dropping role members is idempotent. That is, attempting to add/drop will result in their presence/absence (respectively) in the role regardless of the current state of their role membership.

