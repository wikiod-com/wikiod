---
title: "Converting from MyISAM to InnoDB"
slug: "converting-from-myisam-to-innodb"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Basic conversion
    ALTER TABLE foo ENGINE=InnoDB;

This converts the table, but does not take care of any differences between the engines.  Most differences will not matter, especially for small tables.  But for busier tables, other considerations should be considered.  [_Conversion considerations_](http://mysql.rjweb.org/doc.php/myisam2innodb)

## Converting All Tables in one Database
To easily convert all tables in one database, use the following:

    SET @DB_NAME = DATABASE();

    SELECT  CONCAT('ALTER TABLE `', table_name, '` ENGINE=InnoDB;') AS sql_statements
    FROM    information_schema.tables
    WHERE   table_schema = @DB_NAME
    AND     `ENGINE` = 'MyISAM'
    AND     `TABLE_TYPE` = 'BASE TABLE';

> **NOTE:** You should be connected to your database for `DATABASE()` function to work, otherwise it will return `NULL`. This mostly applies to standard mysql client shipped with server as it allows to connect without specifying a database.

Run this SQL statement to retrieve all the `MyISAM` tables in your database.

Finally, copy the output and execute SQL queries from it.

