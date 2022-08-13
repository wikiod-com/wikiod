---
title: "PostgreSQL High Availability"
slug: "postgresql-high-availability"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Replication in PostgreSQL
 - **Configuring the Primary Server**

    - **Requirements:**

        + Replication User for replication activities
        + Directory to store the WAL archives

    - **Create Replication user**

        `createuser -U postgres replication -P -c 5 --replication`
        
            + option -P will prompt you for new password
            + option -c is for maximum connections. 5 connections are enough for replication
            + -replication will grant replication privileges to the user

    + **Create a archive directory in data directory**
            
        `mkdir $PGDATA/archive`

    + **Edit the pg_hba.conf file**
        
        This is host base authentication file, contains the setting for client autherntication. Add below entry:
        
                #hosttype    database_name     user_name       hostname/IP      method
                 host        replication       replication     <slave-IP>/32    md5

   + **Edit the postgresql.conf file**
      
        This is the configuration file of PostgreSQL.
        
        `wal_level = hot_standby`

        This parameter decides the behavior of slave server.
        
            `hot_standby` logs what is required to accept read only queries on slave server.
        
            `streaming` logs what is required to just apply the WAL's on slave.
        
            `archive` which logs what is required for archiving.
        
        `archive_mode=on`
              
        This parameters allows to send WAL segments to archive location using `archive_command` parameter.

        `archive_command = 'test ! -f /path/to/archivedir/%f && cp %p /path/to/archivedir/%f'`
        
        Basically what above `archive_command` does is it copies the WAL segments to archive directory.

        `wal_senders = 5`
        This is maximum number of WAL sender processes.

        Now restart the primary server.
    
    
+ **Backing up the primay server to the slave server**

     Before making changes on the server stop the primary server.

> Important: Don't start the service again until all configuration and
> backup steps are complete. You must bring up the standby server in a
> state where it is ready to be a backup server. This means that all
> configuration settings must be in place and the databases must be
> already synchronized. Otherwise, streaming replication will fail to
> start`

   + **Now run the pg_basebackup utility**

        `pg_basebackup` utility copies the data from primary server data directory to slave data directory.

        `$ pg_basebackup -h <primary IP> -D /var/lib/postgresql/<version>/main -U replication -v -P --xlog-method=stream`

    

        -D: This is tells pg_basebackup where to the initial backup
    
        -h: Specifies the system where to look for the primary server

        -xlog-method=stream: This will force the pg_basebackup to open another connection and stream enough xlog while backup is running.
                             It also ensures that fresh backup can be started without failing back to using an archive.

+ **Configuring the standby server**

    To configure the standby server, you'll edit postgresql.conf and create a new configuration file named recovery.conf.

    `hot_standby = on`

    This specifies whether you are allowed to run queries while recovering

    + **Creating recovery.conf file**

        `standby_mode = on`

        Set the connection string to the primary server. Replace <primary-external-IP> with the external IP address of the primary server. Replace <password> with the password for the user named replication

        `primary_conninfo = 'host=<primary-external-IP> port=5432 user=replication password=<password>'

        (Optional) Set the trigger file location:

        `trigger_file = '/tmp/postgresql.trigger.5432'`

        The `trigger_file` path that you specify is the location where you can add a file when you want the system to fail over to the standby server. The presence of the file "triggers" the failover. Alternatively, you can use the pg_ctl promote command to trigger failover.
          
+ **Start the standby server**
        
     You now have everything in place and are ready to bring up the standby server
                
**Attribution**

This article is substantially derived from and attributed to [How to Set Up PostgreSQL for High Availability and Replication with Hot Standby][1], with minor changes in formatting and examples and some text deleted. The source was published under the [Creative Commons Public License 3.0][2], which is maintained here.


  [1]: https://cloud.google.com/solutions/setup-postgres-hot-standby#create_a_user_for_replication
  [2]: https://creativecommons.org/licenses/by/3.0/

