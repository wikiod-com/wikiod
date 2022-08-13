---
title: "Table partitioning"
slug: "table-partitioning"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

Partitioning is a functionality to split tables and indexes into smaller pieces. It is used to improve performance and to manage the smaller pieces individually.

The partition key is a column or a set of columns that defines in which partition each row is going to be stored.

[Partitioning Overview in official Oracle documentation](http://www.oracle.com/technetwork/database/options/partitioning/overview/index.html)

Partitioning is an extra cost option and only available for the Enterprise Edition.

## Select existing partitions
Check existing partitions on Schema

    SELECT * FROM user_tab_partitions;






## Drop partition
    ALTER TABLE table_name DROP PARTITION partition_name;

## Select data from a partition
Select data from a partition

    SELECT * FROM orders PARTITION(partition_name);

## Hash partitioning
This creates a table partitioned by hash, in this example on store id.

    CREATE TABLE orders (
        order_nr NUMBER(15),
        user_id VARCHAR2(2),
        order_value NUMBER(15),
        store_id NUMBER(5)
    ) 
    PARTITION BY HASH(store_id) PARTITIONS 8;

You should use a power of 2 for the number of hash partitions, so that you get an even distribution in partition size.

## Range partitioning 
This creates a table partitioned by ranges, in this example on order values.

    CREATE TABLE orders (
        order_nr NUMBER(15),
        user_id VARCHAR2(2),
        order_value NUMBER(15),
        store_id NUMBER(5)
    ) 
    PARTITION BY RANGE(order_value) (
        PARTITION p1 VALUES LESS THAN(10), 
        PARTITION p2 VALUES LESS THAN(40), 
        PARTITION p3 VALUES LESS THAN(100),
        PARTITION p4 VALUES LESS THAN(MAXVALUE)
    ); 

## List partitioning
This creates a table partitioned by lists, in this example on store id.

    CREATE TABLE orders (
        order_nr NUMBER(15),
        user_id VARCHAR2(2),
        order_value NUMBER(15),
        store_id NUMBER(5)
    ) 
    PARTITION BY LIST(store_id) (
        PARTITION p1 VALUES (1,2,3), 
        PARTITION p2 VALUES(4,5,6),
        PARTITION p3 VALUES(7,8,9),
        PARTITION p4 VALUES(10,11)
    );

## Truncate a partition
    ALTER TABLE table_name TRUNCATE PARTITION partition_name;

## Rename a partition
    ALTER TABLE table_name RENAME PARTITION p3 TO p6;

## Move partition to different tablespace
    ALTER TABLE table_name 
    MOVE PARTITION partition_name TABLESPACE tablespace_name;

## Add new partition
    ALTER TABLE table_name 
    ADD PARTITION new_partition VALUES LESS THAN(400);

## Split Partition
Splits some partition into two partitions with another high bound.

    ALTER TABLE table_name SPLIT PARTITION old_partition 
        AT (new_high_bound) INTO (PARTITION new_partition TABLESPACE new_tablespace,
        PARTITION old_partition)

## Merge Partitions
Merge two partitions into single one

    ALTER TABLE table_name
      MERGE PARTITIONS first_partition, second_partition
      INTO  PARTITION  splitted_partition TABLESPACE new_tablespace

## Exchange a partition
Exchange/convert a partition to a non-partitioned table and vice versa. This facilitates a fast "move" of data between the data segments (opposed to doing something like "insert...select" or "create table...as select") as the operation is DDL (the partition exchange operation is a data dictionary update without moving the actual data) and not DML (large undo/redo overhead).

Most basic examples :

1. Convert a non-partitioned table (table "B") to a partition (of table "A") :

Table "A" doesn't contain data in partition "OLD_VALUES" and table "B" contains data

    ALTER TABLE "A" EXCHANGE PARTITION "OLD_VALUES" WITH TABLE "B";

Result : data is "moved" from table "B" (contains no data after operation) to partition "OLD_VALUES"

2. Convert a partition to a non-partitioned table :

Table "A" contains data in partition "OLD_VALUES" and table "B" doesn't contain data

    ALTER TABLE "A" EXCHANGE PARTITION "OLD_VALUES" WITH TABLE "B";

Result : data is "moved" from partition "OLD_VALUES" (contains no data after operation)  to table "B"

Note : there is a quite a few additional options, features and restrictions for this operation


Further info can be found on this link ---> "https://docs.oracle.com/cd/E11882_01/server.112/e25523/part_admin002.htm#i1107555" (section "Exchanging Partitions")


