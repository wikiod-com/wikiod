---
title: "Sqoop Import"
slug: "sqoop-import"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Syntax
- `<rdbms-jdbc-url>` // RDBMS JDBC URL
- `<username>` // Username of the RDBMS database
- `<password>` // Password of the RDBMS database
- `<table-name>` // RDBMS database table
- `<hdfs-home-dir>` // HDFS home directory
- `<condition>` // Condition that can be expressed in the form of a SQL query with a WHERE clause.
- `<sql-query>` // SQL Query
- `<target-dir>` // HDFS Target Directory

Sqoop is a Hadoop Command Line tool that imports table from an RDBMS data source to HDFS and vice versa. It generates a Java class which allows us to interact with the imported data. Each row from a table is saved as a separate record in HDFS. Records can be stored as text files or in binary representation as Avro or Sequence Files. There are 2 versions of sqoop :

> Sqoop1 and Sqoop2

Sqoop1 is the widely accepted tool and is recommended for production environments.
Find the comparison between Sqoop1 and Sqoop2 as stated on [Cloudera's website][1].

  [1]: https://www.cloudera.com/documentation/enterprise/5-4-x/topics/cdh_ig_sqoop_vs_sqoop2.html

## Import RDBMS Table to HDFS
    sqoop import \
    --connect <rdbms-jdbc-url> \
    --username <username> \
    --password <password> \
    --table <table-name>

Example with Mysql:

    sqoop import \
    --connect jdbc:mysql://mysql.example.com/testdb \
    --username root \
    --password root \
    --table employees


CSV file with the imported data will be created under **employees** directory under home directory.

Inspect using command:

    hadoop fs -cat <hdfs-home-dir>/employees/part-m-* 

Import to a particular directory in HDFS
----------------------------------------

    sqoop import \
    --connect jdbc:mysql://mysql.example.com/testdb \
    --username root \
    --password root \
    --table emplyoees \
    --target-dir /dev/data/employees

This will generate CSV file under `/dev/data/employees` directory.

Specify parent HDFS directory for Sqoop job
-------------------------------------------

    sqoop import \
    --connect jdbc:mysql://mysql.example.com/testdb \
    --username root \
    --password root \
    --table emplyoees \
    --warehouse-dir /dev/warehouse/

`--warehouse-dir` tag in above command will change your home directory to `/dev/warehouse/`


## Import subset of RDBMS Table to HDFS
Using `--where` tag
---------------

 
    sqoop import \
    --connect <rdbms-jdbc-url> \
    --username <username> \
    --password <password> \
    --table <table-name> \
    --where "<condition>"
    


Example with Mysql:

    sqoop import \
    --connect jdbc:mysql://mysql.example.com/testdb \
    --username root \
    --password root \
    --table employees \
    --where "country = 'INDIA'"


Any special functions, conversions, or even user-defined functions can be used in `--where` clause.

Using Free Form Query
---------------------
    sqoop import \
    --connect <rdbms-jdbc-url> \
    --username <username> \
    --password <password> \
    --query <sql-query> \
    --target-dir <target-dir>


Example with Mysql:

    sqoop import \
    --connect jdbc:mysql://mysql.example.com/testdb \
    --username root \
    --password root \
    --query "SELECT emplyoees.name, \
                    address.city \
                    FROM emplyoees \
                    JOIN address USING(emp_id) \
                    WHERE \$CONDITION" \
    --taget-dir /dev/data/employees

   

## Import-All-Tables
SQOOP provides facility to import all tables

    sqoop import-all-tables \
    --connect <rdbms-jdbc-url> \
    --username <username> \
    --password <password> \
    --hive-import \
    --create-hive-table \
    --hive-database <dbname> \
    --warehouse-dir <warehouse-dir>

Important points to note on differences between import and import-all-tables:

Need to provide --warehouse-dir=/<exampledirectory>/stage.db database name as input parameter to download all tables into a database. In sqoop import we will be providing only --target-dir not the --warehouse-dir

Example:

    sqoop import-all-tables --connect="jdbc:mysql://serverip:3306/dbname" 
    --username=xxx --password=yyy 
    -m 1 --hive-import 
    --hive-overwrite 
    --create-hive-table 
    --hive-database dbname 
    --hive-home /user/hive/warehouse
    --warehouse-dir=/user/hive/warehouse/retail_stage.db

