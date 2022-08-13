---
title: "Backup script for a production DB"
slug: "backup-script-for-a-production-db"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
- The script allows you to create a backup directory for each execution with the following syntax : Name of database backup directory + date and time of execution
- Example : prodDir22-11-2016-19h55
- After it's created, it creates two backup files with the following syntax : **Name of database **+** date and time of execution**
- Example : 
- dbprod22-11-2016-19h55.backup    **(dump file)**
- dbprod22-11-2016-19h55.sql       **(sql file)**
- At the end of one execution at **22-11-2016 @ 19h55**, we get :
- /save_bd/prodDir22-11-2016-19h55/dbprod22-11-2016-19h55.backup
- /save_bd/prodDir22-11-2016-19h55/dbprod22-11-2016-19h55.sql

## Parameters
| parameter | details                   |
| ------ | ------ |
| save_db   | The main backup directory |
| dbProd | The secondary backup directory |
| DATE | The date of the backup in the specified format |
| dbprod | The name of the database to be saved |
| /opt/postgres/9.0/bin/pg_dump | The path to the pg_dump binary |
| -h | Specifies the host name of the machine on which the server is running, Example : localhost |
| -p | Specifies the TCP port or local Unix domain socket file extension on which the server is listening for connections, Example 5432 |
| -U | User name to connect as. |

 1. If there is a backup tool such as [HDPS][1], or [Symantec Backup][2], ...
It is necessary to empty the backup directory **before each launch**. 


To avoid cluttering the backup tool because the backup of old files is supposed to be done.

> To enable this feature please uncomment line N° 3.

    rm -R / save_db / *
 2. In the case where the budget does not allow to have a tool of backup, one can always use the tasks planner ([cron command][3]).

> The following command is used to edit the cron table for the current user.

    crontab -e

> Schedule the launch of the script with the calendar at 11pm.

    0 23 * * * /saveProdDb.sh


  [1]: https://www.hds.com/en-us/products-solutions/data-protection/data-protection-suite.html
  [2]: https://www.symantec.com/fr/fr/page.jsp?id=introducing-backup-exec-15
  [3]: https://fr.wikipedia.org/wiki/Cron

## saveProdDb.sh
In general, we tend to back up the DB with the pgAdmin client. The following is a sh script used to save the database (under linux) in two formats:

 - **SQL file**: for a possible resume of data on any version of PostgreSQL.
    
 - **Dump file**: for a higher version than the current version.

 
   

    #!/bin/sh
    cd /save_db
    #rm -R /save_db/*
    DATE=$(date +%d-%m-%Y-%Hh%M)
    echo -e "Sauvegarde de la base du ${DATE}"
    mkdir prodDir${DATE}
    cd prodDir${DATE}
    
    #dump file
    /opt/postgres/9.0/bin/pg_dump -i -h localhost -p 5432 -U postgres -F c -b -w -v -f "dbprod${DATE}.backup" dbprod
    
    #SQL file
    /opt/postgres/9.0/bin/pg_dump -i -h localhost -p 5432 -U postgres --format plain --verbose  -f "dbprod${DATE}.sql" dbprod




