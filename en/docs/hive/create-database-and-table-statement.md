---
title: "Create Database and Table Statement"
slug: "create-database-and-table-statement"
draft: false
images: []
weight: 9769
type: docs
toc: true
---

## Syntax
 - CREATE [TEMPORARY] [EXTERNAL] TABLE [IF NOT EXISTS] [db_name.]table_name    
    
      [(col_name data_type [COMMENT col_comment], ...)]
  [COMMENT table_comment]
  [PARTITIONED BY (col_name data_type [COMMENT col_comment], ...)]
  [CLUSTERED BY (col_name, col_name, ...) [SORTED BY (col_name [ASC|DESC], ...)] INTO num_buckets BUCKETS]
  [SKEWED BY (col_name, col_name, ...)                  -- (Note: Available in Hive 0.10.0 and later)]
     ON ((col_value, col_value, ...), (col_value, col_value, ...), ...)
     [STORED AS DIRECTORIES]
  [
   [ROW FORMAT row_format] 
   [STORED AS file_format]
     | STORED BY 'storage.handler.class.name' [WITH SERDEPROPERTIES (...)] 
  ]
  [LOCATION hdfs_path]
  [TBLPROPERTIES (property_name=property_value, ...)]  
  [AS select_statement];   
 - CREATE [TEMPORARY] [EXTERNAL] TABLE [IF NOT EXISTS] [db_name.]table_name
  LIKE existing_table_or_view_name
  [LOCATION hdfs_path];
 - data_type : primitive_type,array_type,map_type,struct_type,union_type
 - primitive_type: TINYINT, SMALLINT, INT , BIGINT, BOOLEAN, FLOAT, DOUBLE, STRING, BINARY, TIMESTAMP, DECIMAL, DECIMAL(precision, scale), DATE, VARCHAR, CHAR
 - array_type: ARRAY < data_type >
 - map_type: MAP < primitive_type, data_type >
 - struct_type: STRUCT < col_name : data_type [COMMENT col_comment], ...>
 - union_type: UNIONTYPE < data_type, data_type, ... >  
 - row_format: DELIMITED [FIELDS TERMINATED BY char [ESCAPED BY char]] [COLLECTION ITEMS TERMINATED BY char]
        [MAP KEYS TERMINATED BY char] [LINES TERMINATED BY char]
        [NULL DEFINED AS char]   
  , SERDE serde_name [WITH SERDEPROPERTIES (property_name=property_value, property_name=property_value, ...)]
 - file_format:
  : SEQUENCEFILE
  , TEXTFILE  , RCFILE      , ORC         , PARQUET     , AVRO        , INPUTFORMAT input_format_classname OUTPUTFORMAT output_format_classname
 - CREATE (DATABASE|SCHEMA) [IF NOT EXISTS] database_name
  [COMMENT database_comment]
  [LOCATION hdfs_path]
  [WITH DBPROPERTIES (property_name=property_value, ...)];

When working with tables and databases in HIVE. Below points can be usefull.

 - We can switch database using 
    `use database;` command
 - To know the current working database we can get using `SELECT current_database()` 
 - To see the DDL used for create table statement we can use `SHOW CREATE TABLE tablename`
 - To see all columns of table use `DESCRIBE tablename` to show extended details like location serde used and others `DESCRIBE FORMATTED tablename`. DESCRIBE can also be abbrevated as DESC.

## Create Table
Creating a **managed** table with partition and stored as a sequence file. The data format in the files is assumed to be field-delimited by `Ctrl-A (^A)` and row-delimited by newline. The below table is created in hive warehouse directory specified in value for the key `hive.metastore.warehouse.dir` in the Hive config file `hive-site.xml`.

    CREATE TABLE view
    (time INT, 
    id BIGINT,
    url STRING, 
    referrer_url STRING,
    add STRING COMMENT 'IP of the User')
    COMMENT 'This is view table'
    PARTITIONED BY(date STRING, region STRING)
    ROW FORMAT DELIMITED
    FIELDS TERMINATED BY '\001'
    STORED AS SEQUENCEFILE;



Creating a **external** table with partitions and stored as a sequence file. The data format in the files is assumed to be field-delimited by `ctrl-A` and row-delimited by newline. The below table is created in the location specified and it comes handy when we already have data. One of the advantages of using an external table is that we can drop the table without deleting the data. For instance, if we create a table and realize that the schema is wrong, we can safely drop the table and recreate with the new schema without worrying about the data.Other advantage is that if we are using other tools like pig on same files, we can continue using them even after we delete the table.

    CREATE EXTERNAL TABLE view
    (time INT, 
    id BIGINT,
    url STRING, 
    referrer_url STRING,
    add STRING COMMENT 'IP of the User')
    COMMENT 'This is view table'
    PARTITIONED BY(date STRING, region STRING)
    ROW FORMAT DELIMITED
    FIELDS TERMINATED BY '\001'
    STORED AS SEQUENCEFILE
    LOCATION '<hdfs_location>';

   Creating a table using select query and populating results from query,
    these statements are known as **CTAS(Create Table As Select)**.

   There are two parts in CTAS, the SELECT part can be any SELECT statement supported by HiveQL. The CREATE part of the CTAS takes the resulting schema from the SELECT part and creates the target table with other table properties such as the SerDe and storage format.
    
   CTAS has these restrictions:
    
   - The target table cannot be a partitioned table.
   - The target table cannot be an external table.
   - The target table cannot be a list bucketing table.


    CREATE TABLE new_key_value_store
    ROW FORMAT SERDE "org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe"
    STORED AS RCFile
    AS
    SELECT * FROM page_view
    SORT BY url, add;

Create Table Like:

The **LIKE form of CREATE TABLE** allows you to copy an existing table definition exactly (without copying its data). In contrast to CTAS, the statement below creates a new table whose definition exactly matches the existing table in all particulars other than table name. The new table contains no rows.

    CREATE TABLE empty_page_views
    LIKE page_views;



## Create Database
Creating a database in a particular location. If we dont specify any location for database its created in warehouse directory.

    CREATE DATABASE IF NOT EXISTS db_name 
    COMMENT 'TEST DATABASE'
    LOCATION /PATH/HDFS/DATABASE/;

## HIVE_HBASE Integration
Hive-Hbase integration is supported since below versions.
Hive: 0.11.0
HBase: 0.94.2
Hadoop: 0.20.2


    CREATE TABLE hbase_hive
    (id string,
     col1 string,
     col2 string,
     col3 int) 
    STORED BY 'org.apache.hadoop.hive.hbase.HBaseStorageHandler'
    WITH SERDEPROPERTIES 
    ("hbase.columns.mapping" = ":key,cf1:col1,cf1:col2,cf1:col3")
    TBLPROPERTIES ("hbase.table.name" = "hive_hbase");

Note: 1st column should be the key column.

## Create table using existing table properties.
    CREATE TABLE new_table_name LIKE existing_table_name;

## Hive ACID table creation.
ACID tables are supported since hive 0.14 version. 
Below table supports UPDATE/DELETE/INSERT

Below configuration changes required in hive-site.xml.

     hive.support.concurrency = true
     hive.enforce.bucketing = true
     hive.exec.dynamic.partition.mode = nonstrict
     hive.txn.manager =org.apache.hadoop.hive.ql.lockmgr.DbTxnManager
     hive.compactor.initiator.on = true
     hive.compactor.worker.threads = 1

Currently only orc file is format supported.

Table create statement.

     create table Sample_Table(
     col1 Int,
     col2 String,
     col3 String) 
     clustered by (col3) into 3 buckets 
     stored as orc 
     TBLPROPERTIES ('transactional'='true');

