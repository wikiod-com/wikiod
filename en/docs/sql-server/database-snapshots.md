---
title: "Database Snapshots"
slug: "database-snapshots"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

A database snapshot is a read-only, static view of a SQL Server database which is transactionally consistent with the source database as of the moment of the snapshot's creation. 

A database snapshot always resides on the same server instance as its source database. As the source database is updated, the database snapshot is updated. 

A snapshot differs from a backup since the process of snapshot creation is instantaneous and the snapshot occupies space only as changes in the source database are applied. A backup on the other hand stores a full copy of the data as on the time of backup creation. 

Additionally, a snapshot gives an instant read only copy of the database, while a backup needs to be restored to a server in order to be readable (and once restored can be written to as well)

Database snapshots are only available in the Enterprise and Developer editions.

## Create a database snapshot
A database snapshot is a read-only, static view of a SQL Server database (the source database). It is similar to backup, but it is available as any other database so client can query snapshot database.

<!-- language: lang-sql -->
```sql
CREATE DATABASE MyDatabase_morning -- name of the snapshot
ON (
     NAME=MyDatabase_data, -- logical name of the data file of the source database
     FILENAME='C:\SnapShots\MySnapshot_Data.ss' -- snapshot file; 
) 
AS SNAPSHOT OF MyDatabase; -- name of source database
```
You can also create snapshot of database with multiple files:

<!-- language: lang-sql -->
```sql
CREATE DATABASE MyMultiFileDBSnapshot ON
    (NAME=MyMultiFileDb_ft, FILENAME='C:\SnapShots\MyMultiFileDb_ft.ss'),
    (NAME=MyMultiFileDb_sys, FILENAME='C:\SnapShots\MyMultiFileDb_sys.ss'),
    (NAME=MyMultiFileDb_data, FILENAME='C:\SnapShots\MyMultiFileDb_data.ss'),
    (NAME=MyMultiFileDb_indx, FILENAME='C:\SnapShots\MyMultiFileDb_indx.ss')
AS SNAPSHOT OF MultiFileDb;
```

## Restore a database snapshot
If data in a source database becomes damaged or some wrong data is written into database, in some cases, reverting the database to a database snapshot that predates the damage might be an appropriate alternative to restoring the database from a backup.

<!-- language: lang-sql -->
```sql
RESTORE DATABASE MYDATABASE FROM DATABASE_SNAPSHOT='MyDatabase_morning';
```

> **Warning:** This will *delete all changes* made to the source database since the snapshot was taken!

## DELETE Snapshot
You can delete existing snapshots of database using DELETE DATABASE statement:

    DROP DATABASE Mydatabase_morning

In this statement you should reference name of the database snapshot.

