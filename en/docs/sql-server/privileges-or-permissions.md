---
title: "Privileges or Permissions"
slug: "privileges-or-permissions"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Simple rules
**Granting permission to create tables**

    USE AdventureWorks;  
    GRANT CREATE TABLE TO MelanieK;  
    GO 

**Granting SHOWPLAN permission to an application role**

    USE AdventureWorks2012;  
    GRANT SHOWPLAN TO AuditMonitor;  
    GO  

 **Granting CREATE VIEW with GRANT OPTION**

    USE AdventureWorks2012;  
    GRANT CREATE VIEW TO CarmineEs WITH GRANT OPTION;  
    GO 

**Granting all rights to a user on a specific database**

    use YourDatabase
    go
    exec sp_addrolemember 'db_owner', 'UserName'
    go

 


