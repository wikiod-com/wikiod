---
title: "Backup and Restore"
slug: "backup-and-restore"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Backing up the filesystem instead of using `pg_dumpall` and `pg_dump`

It's very important that if you use this, you call the `pg_start_backup()` function before and `pg_stop_backup()` function after. Doing filesystem backups is not safe otherwise; even a ZFS or FreeBSD snapshot of the filesystem backed up without those function calls will place the database in recovery mode and may lose transactions.

I would avoid doing filesystem backups instead of regular Postgres backups, both for this reason, and because Postgres backup files (especially in the custom format) are extremely versatile in supporting alternate restores. Since they're single files, they're also less hassle to manage.

## Backing up one database
    pg_dump -Fc -f DATABASE.pgsql DATABASE

The `-Fc` selects the "custom backup format" which gives you more power than raw SQL; see `pg_restore` for more details. If you want a vanilla SQL file, you can do this instead:

    pg_dump -f DATABASE.sql DATABASE

or even

    pg_dump DATABASE > DATABASE.sql


  [1]: https://www.postgresql.org/docs/9.5/static/app-pgrestore.html

## Restoring backups
    psql < backup.sql

A safer alternative uses `-1` to wrap the restore in a transaction. The `-f` specifies the filename rather than using shell redirection.

    psql -1f backup.sql

Custom format files must be restored using `pg_restore` with the `-d` option to specify the database:

    pg_restore -d DATABASE DATABASE.pgsql

The custom format can also be converted back to SQL:

    pg_restore backup.pgsql > backup.sql

Usage of the custom format is recommended because you can choose which things to restore and optionally enable parallel processing.

You may need to do a pg_dump followed by a pg_restore if you upgrade from one postgresql release to a newer one.
 

## Backing up the whole cluster
    $ pg_dumpall -f backup.sql

This works behind the scenes by making multiple connections to the server once for each database and executing `pg_dump` on it.

Sometimes, you might be tempted to set this up as a cron job, so you want to see the date the backup was taken as part of the filename:

    $ postgres-backup-$(date +%Y-%m-%d).sql

However, please note that this could produce large files on a daily basis. Postgresql has a much better mechanism for regular backups - [WAL archives][1]

The output from pg_dumpall is sufficient to restore to an identically-configured Postgres instance, but the configuration files in `$PGDATA` (`pg_hba.conf` and `postgresql.conf`) are not part of the backup, so you'll have to back them up separately.

    postgres=# SELECT pg_start_backup('my-backup');
    postgres=# SELECT pg_stop_backup();


To take a filesystem backup, you must use these functions to help ensure that Postgres is in a consistent state while the backup is prepared.


  [1]: https://www.postgresql.org/docs/9.2/static/continuous-archiving.html

## Using psql to export data
Data can be exported using copy command or by taking use of command line options of psql command.

**To Export csv data from table user to csv file:**

    psql -p \<port> -U \<username> -d \<database> -A -F<delimiter> -c\<sql to execute> \> \<output filename with path>

    psql -p 5432 -U postgres -d test_database -A -F, -c "select * from user" > /home/user/user_data.csv

Here combination of -A and -F does the trick. 

`-F` is to specify delimiter

    -A or --no-align

Switches to unaligned output mode. (The default output mode is otherwise aligned.)

## Using Copy to import
To Copy Data from a CSV file to a table
===================================

    COPY <tablename> FROM '<filename with path>';

**To insert into table `user` from a file named `user_data.csv` placed inside `/home/user/`:**

    COPY user FROM '/home/user/user_data.csv';

To Copy data from pipe separated file to table
=====================================

    COPY user FROM '/home/user/user_data' WITH DELIMITER '|';

Note: In absence of the option `with delimiter`, the default delimiter is comma `,` 

To ignore header line while importing file
==========================================

Use the Header option:

    COPY user FROM '/home/user/user_data' WITH DELIMITER '|' HEADER;

Note: If data is quoted, by default data quoting characters are double quote. If the data is quoted using any other character use the `QUOTE` option; however, this option is allowed only when using CSV format.

 


## Using Copy to export


