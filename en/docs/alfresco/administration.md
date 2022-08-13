---
title: "Administration"
slug: "administration"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Starting and Stopping
To start Alfresco:

1. Switch to the alfresco user
2. Change to the $ALFRESCO_HOME directory
3. Run `./alfresco.sh start`

To stop Alfresco:

1. Switch to the alfresco user
2. Change to the $ALFRESCO_HOME directory
3. Run `./alfresco.sh start`



## Logging
Alfresco logs live in $ALFRESCO_HOME/tomcat/logs/catalina.out.

## Backups
There are many ways to backup an Alfresco system. It is important that you backup the database as well as the content store. You may also want to back up the Solr indices.

Assuming you installed using the binary installer, and everything lives in $ALRESCO_HOME, you can backup the database like this:

1. Stop Alfresco
2. Switch to the alfresco user
3. Change to the $ALFRESCO_HOME/postgresql/bin directory
4. Dump the database with `./pg_dump alfresco --user alfresco > $ALFRESCO_HOME/alf_data/db-backup.sql`. You might be prompted for a password. It should be the same thing as the admin password you provided during installation. If not, check $ALFRESCO_HOME/tomcat/shared/classes/alfresco-global.properties for the database password.

Now you have your database backed up. It is important to do that first. The next step is to backup the content store.

1. Change to the $ALFRESCO_HOME/alf_data/contentstore.deleted directory.
2. Delete everything in here. There's no reason to keep those files around and no reason to back them up.
3. Change to the $ALFRESCO_HOME/alf_data directory.
4. Tar up the whole thing, which will also include the database backup you created in the previous step assuming you placed it in the alf_data directory. Run `tar czvf ~/alfresco-backup.tar.gz .`.

The content of that TAR file now has everything you need to restore your working system.


## Auditing
Auditing is an Alfresco feature that allows the user to trace and log some specific events during ECM platform usage.

**Enable auditing**

To enable auditing you have to add some lines of configuration to the `alfresco-global.properties` file, which resides in `tomcat/shared/classes/`

 

    audit.enabled = true
    audit.alfresco-access.enabled=true

You have to save changes to the `alfresco-global.properties` file and restart the Alfresco server, in order to enable auditing.

**Auditing default configuration**

Here the complete list of configuration properties that can be overridden modifying the `alfresco-global.properties` file:

    # Audit configuration                                                                                                                                                                       
    audit.enabled=true
    audit.tagging.enabled=true
    audit.alfresco-access.enabled=false
    audit.alfresco-access.sub-actions.enabled=false
    audit.cmischangelog.enabled=false
    audit.dod5015.enabled=false
    
    # Setting this flag to true will force startup failure when invalid audit configurations are detected                                                                                       
    audit.config.strict=false
    
    # Audit map filter for AccessAuditor - restricts recorded events to user driven events. In this case it neglect events issued by a System or a null user, the content or folder path is under /sys:archivedItem or under /ver: and the node type is not cm:folder, cm:content or st:site                                                                                                     
    audit.filter.alfresco-access.default.enabled=false
    audit.filter.alfresco-access.transaction.user=~System;~null;.*
    audit.filter.alfresco-access.transaction.type=cm:folder;cm:content;st:site
    audit.filter.alfresco-access.transaction.path=~/sys:archivedItem;~/ver:;.*

    #The default to preserve all cm:auditable data on a node when the process is not directly driven by a user action                                                                                        
    system.auditableData.preserve=${system.preserve.modificationData}

    #Specific control of how the FileFolderService treats cm:auditable data when performing moves                                                                                                            
    system.auditableData.FileFolderService=${system.auditableData.preserve}

    #Specific control of whether ACL changes on a node trigger the cm:auditable aspect                                                                                                                       
    system.auditableData.ACLs=${system.auditableData.preserve}   

   As usual you have to save changes to the alfresco-global.properties file and restart the Alfresco server, in order to enable these modifications.



**Audit filters**

Audit filters are properties that specify the strategy used to filter audit events by using particular regular expression to include or exclude events. Both custom and default audit filters can be added as overrides in the `alfresco-global.properties` configuration file.

The anatomy of an audit filter property is the following:

    audit.filter.<data_producer>.<path>

where `<data-producer>` is one of the Alfresco built-in data producers:

 1. `alfresco-access`: a wide group of high level events such as logins (both successful and failed), property updates, CRUD on nodes, content reads/updates, aspect addition and removal, versioning, check-in/check-out operations
 2. `alfresco-node`
 3. `alfresco-api`: events issued by the call of low level API methods and services. For example it can be used to list SearchServices search list parameters, properties listing using PropertyServices, operations on nodes using NodeServices and so on.

and `path` is the real path value to filter against.

Property names have an audit.filter.* prefix and use '.' as a separator where as components of rootPath and keys in the audit map use '/'.

Lists are evaluated from left to right and if no match is made by the end of the list the value is rejected. If there is not a property for a given value or an empty list is defined any value is accepted.

Each regular expression in the list is separated by a semicolon (';'). Expressions that include a semicolon can be escaped using a '\'.

*Note that if the `audit.config.strict` flag is set to true Alfresco startup will fail in case of invalid audit configurations detection.*

An expression that starts with a `'~'` indicates that any matching value should be rejected. If the first character of an expression needs to be a `'~'`, it can be escaped with a `'\'`.

Adding `.*` at the end of a filter will include all values that have not been specifically excluded

Filters can be one of the following:

`transaction.user` - specifies what user(s) actions will/will not be audited. For
example: Actions from all users except for 'System' will be audited

`transaction.type` - actions that are performed against the specified document type will be audited.

`default.path` - actions that occur on documents within the specified path will be audited

`transaction.action` - specifies what actions will and won't be audited. Some of the auditing events that can be enables or disabled using this property are: READ, MOVE, COPY, CHECK IN, CHECK OUT, CANCEL CHECK OUT, CREATE VERSION, readContent, addNodeAspect, deleteNodeAspect, updateNodeProperties.


For more information about audit filters:

https://github.com/tsgrp/OpenContent/wiki/Alfresco-Audit-Configuration<BR>

http://docs.alfresco.com/5.1/concepts/audit-example-filter.html 

