---
title: "Backup using mysqldump"
slug: "backup-using-mysqldump"
draft: false
images: []
weight: 9683
type: docs
toc: true
---

## Syntax
- mysqldump -u [username] -p[password] [other options] db_name > dumpFileName.sql   /// To Backup single database
- mysqldump -u [username] -p[password] [other options] db_name [tbl_name1 tbl_name2  tbl_name2 ...] > dumpFileName.sql   /// To Backup one or more tables
- mysqldump -u [username] -p[password] [other options] --databases db_name1 db_name2 db_name3 ... > dumpFileName.sql   /// To Backup one or more complete databases
- mysqldump -u [username] -p[password] [other options] --all-databases  > dumpFileName.sql   /// To Backup entire MySQL server

## Parameters
Option|Effect
---|---
--|# Server login options
`-h` (`--host`)|Host (IP address or hostname) to connect to. Default is `localhost` (`127.0.0.1`) Example: `-h localhost`
`-u` (`--user`)|MySQL user
`-p` (`--password`)|MySQL password. **Important**: When using `-p`, there must not be a space between the option and the password. Example: `-pMyPassword`
|--|# Dump options
`--add-drop-database`|Add a `DROP DATABASE` statement before each `CREATE DATABASE` statement. Useful if you want to replace databases in the server.
`--add-drop-table`|Add a `DROP TABLE` statement before each `CREATE TABLE` statement. Useful if you want to replace tables in the server.
`--no-create-db`|Suppress the `CREATE DATABASE` statements in the dump. This is useful when you're sure the database(s) you're dumping already exist(s) in the server where you'll load the dump.
`-t` (`--no-create-info`)|Suppress all `CREATE TABLE` statements in the dump. This is useful when you want to dump only the data from the tables and will use the dump file to populate identical tables in another database / server.
`-d` (`--no-data`)|Do not write table information. This will only dump the `CREATE TABLE` statements. Useful for creating "template" databases
`-R` (`--routines`)|Include stored procedures / functions in the dump.
`-K` (`--disable-keys`)|Disable keys for each table before inserting the data, and enable keys after the data is inserted. This speeds up inserts only in MyISAM tables with non-unique indexes.

The output of a `mysqldump` operation is a lightly commented file containing sequential SQL statements that are compatible with the version of MySQL utilities that was used to generate it (with attention paid to compatibility with previous versions, but no guarantee for future ones). Thus, the restoration of a `mysqldump`ed database comprises execution of those statements. Generally, this file
- `DROP`s the first specified table or view
- `CREATE`s that table or view
- For tables dumped with data (i.e. without the `--no-data` option)
   - `LOCK`s the table
   - `INSERT`s all of the rows from the original table in one statement
- `UNLOCK TABLES`
- Repeats the above for all other tables and views
- `DROP`s the first included routine
- `CREATE`s that routine
- Repeats the same for all other routines

The presence of the `DROP` before `CREATE` for each table means that if the schema is present, whether or not it is empty, using a `mysqldump` file for its restoration will populate or overwrite the data therein.

## Specifying username and password
    > mysqldump -u username -p [other options]
    Enter password:

If you need to specify the password on the command line (e.g. in a script), you can add it after the `-p` option *without* a space:

    > mysqldump -u username -ppassword [other options]

If you password contains spaces or special characters, remember to use escaping depending on your shell / system.

Optionally the extended form is:

    > mysqldump --user=username --password=password [other options]

 (Explicity specifying the password on the commandline is Not Recommended due to security concerns.) 

## Creating a backup of a database or table
Create a snapshot of a whole database:

<!-- language: lang-bash -->

    mysqldump [options] db_name > filename.sql

Create a snapshot of multiple databases:

<!-- language: lang-bash -->

    mysqldump [options] --databases db_name1 db_name2 ... > filename.sql
    mysqldump [options] --all-databases > filename.sql


Create a snapshot of one or more tables:

<!-- language: lang-bash -->

    mysqldump [options] db_name table_name... > filename.sql


Create a snapshot *excluding* one or more tables:

<!-- language: lang-bash -->

    mysqldump [options] db_name --ignore-table=tbl1 --ignore-table=tbl2 ... > filename.sql


The file extension `.sql` is fully a matter of style. Any extension would work.

## Restoring a backup of a database or table
    mysql [options] db_name < filename.sql

Note that:
 - `db_name` needs to be an existing database;
 - your authenticated user has sufficient privileges to execute all the commands inside your `filename.sql`;
 - The file extension `.sql` is fully a matter of style. Any extension would work.
 - You cannot specify a table name to load into even though you could specify one to dump from. This must be done within `filename.sql`.

Alternatively, when in the **MySQL Command line tool**, you can restore (or run any other script) by using the source command:

    source filename.sql
or

    \. filename.sql

## mysqldump from a remote server with compression
In order to use compression over the wire for a faster transfer, pass the `--compress` option to `mysqldump`. Example:

    mysqldump -h db.example.com -u username -p --compress dbname > dbname.sql

Important: If you don't want to lock up the *source* db, you should also include `--lock-tables=false`. But you may not get an internally consistent db image that way.

To also save the file compressed, you can pipe to `gzip`.

    mysqldump -h db.example.com -u username -p --compress dbname | gzip --stdout > dbname.sql.gz

## restore a gzipped mysqldump file without uncompressing
    gunzip -c dbname.sql.gz | mysql dbname -u username -p

Note: `-c` means write output to stdout.

## Backup direct to Amazon S3 with compression
If you wish to make a complete backup of a large MySql installation and do not have sufficient local storage, you can dump and compress it directly to an Amazon S3 bucket. It's also a good practice to do this without having the DB password as part of the command:

    mysqldump -u root -p --host=localhost --opt --skip-lock-tables --single-transaction \
            --verbose --hex-blob --routines --triggers --all-databases |
        gzip -9 | s3cmd put - s3://s3-bucket/db-server-name.sql.gz

You are prompted for the password, after which the backup starts.

## Tranferring data from one MySQL server to another
If you need to copy a database from one server to another, you have two options:

**Option 1:** 

1. Store the dump file in the source server
1. Copy the dump file to your destination server
1. Load the dump file into your destination server

On the source server:

<!-- language: lang-sh -->

    mysqldump [options] > dump.sql

On the destination server, copy the dump file and execute:

<!-- language: lang-sh -->

    mysql [options] < dump.sql

**Option 2:**

If the destination server can connect to the host server, you can use a pipeline to copy the database from one server to the other:

On the destination server

<!-- language: lang-sh -->

    mysqldump [options to connect to the source server] | mysql [options]

Similarly, the script could be run on the source server, pushing to the destination.  In either case, it is likely to be significantly faster than Option 1.

## Backup database with stored procedures and functions
By default stored procedures and functions or not generated by `mysqldump`, you will need to add the parameter `--routines` (or `-R`):

    mysqldump -u username -p -R db_name > dump.sql

When using [`--routines`][1] the creation and change time stamps are not maintained, instead you should dump and reload the contents of `mysql.proc`.

[1]: https://dev.mysql.com/doc/refman/5.6/en/mysqldump.html#option_mysqldump_routines


