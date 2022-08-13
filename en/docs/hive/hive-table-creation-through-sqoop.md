---
title: "Hive Table Creation Through Sqoop"
slug: "hive-table-creation-through-sqoop"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

If we have a Hive meta-store associated with our HDFS cluster, Sqoop can import the data into Hive by generating and executing a CREATE TABLE statement to define the data’s layout in Hive. Importing data into Hive is as simple as adding the --hive-import option to your Sqoop command line.

Importing data directly from RDBMS to HIVE can solve lots of time. Also we can run a freeform query(a join or some simple query) and populate it in a table of our choice directly into Hive.

>--hive-import tells Sqoop that the final destination is Hive and not HDFS.

>--hive-table option helps in importing the data to the table in hive chosen by us, otherwise it will be named as the source table being imported from RDBMS.

## Hive import with Destination table name in hive

    $ sqoop import --connect jdbc:mysql://10.0.0.100/hadooptest 
    --username hadoopuser -P
    --table table_name --hive-import --hive-table hive_table_name



