---
title: "Insert Statement"
slug: "insert-statement"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Syntax
- **Standard syntax:**
- INSERT OVERWRITE TABLE tablename1 [PARTITION (partcol1=val1, partcol2=val2 ...) [IF NOT EXISTS]] select_statement1 FROM from_statement;
- INSERT INTO TABLE tablename1 [PARTITION (partcol1=val1, partcol2=val2 ...)] select_statement1 FROM from_statement;
- INSERT INTO TABLE tablename1 [PARTITION (partcol1=val1, partcol2=val2 ...)] (z,y) select_statement1 FROM from_statement;
 
- **Hive extension (multiple inserts):**
- FROM from_statement  
INSERT OVERWRITE TABLE tablename1 [PARTITION (partcol1=val1, partcol2=val2 ...) [IF NOT EXISTS]] select_statement1  
[INSERT OVERWRITE TABLE tablename2 [PARTITION ... [IF NOT EXISTS]] select_statement2]  
[INSERT INTO TABLE tablename2 [PARTITION ...] select_statement2] ...;
- FROM from_statement  
INSERT INTO TABLE tablename1 [PARTITION (partcol1=val1, partcol2=val2 ...)] select_statement1  
[INSERT INTO TABLE tablename2 [PARTITION ...] select_statement2]  
[INSERT OVERWRITE TABLE tablename2 [PARTITION ... [IF NOT EXISTS]] select_statement2] ...;  
 
- **Hive extension (dynamic partition inserts):**
- INSERT OVERWRITE TABLE tablename PARTITION (partcol1[=val1], partcol2[=val2] ...) select_statement FROM from_statement;
- INSERT INTO TABLE tablename PARTITION (partcol1[=val1], partcol2[=val2] ...) select_statement FROM from_statement;

**insert overwrite**  
An insert overwrite statement deletes any existing files in the target table or partition before adding new files based off of the select statement used. Note that when there are structure changes to a table or to the DML used to load the table that sometimes the old files are not deleted. When loading to a table using dynamic partitioning only partitions defined by the select statement will be overwritten. Any preexisting partitions in the target will remain and will not be deleted.

**insert into**  
An insert into statement appends new data into a target table based off of the select statement used.

## insert overwrite
    insert overwrite table yourTargetTable select * from yourSourceTable;


## Insert into table
INSERT INTO will append to the table or partition, keeping the existing data intact.
   

        INSERT INTO table yourTargetTable SELECT * FROM yourSourceTable;

If a table is partitioned then we can insert into that particular partition in static fashion as shown below.

        INSERT INTO TABLE yourTargetTable PARTITION (state=CA, city=LIVERMORE) 
        select * FROM yourSourceTable;

If a table is partitioned then we can insert into that particular partition in dynamic fashion as shown below. To perfom dynamic partition inserts we must set below below properties.


 

        Dynamic Partition inserts are disabled by default. These are the relevant configuration properties for dynamic partition inserts:
        SET hive.exec.dynamic.partition=true;
        SET hive.exec.dynamic.partition.mode=non-strict

        INSERT INTO TABLE yourTargetTable PARTITION (state=CA, city=LIVERMORE) (date,time)
        select * FROM yourSourceTable;

Multiple Inserts into from a table.


Hive extension (multiple inserts):

        FROM table_name
    
        INSERT OVERWRITE TABLE table_one SELECT table_name.column_one,table_name.column_two
    
        INSERT OVERWRITE TABLE table_two SELECT table_name.column_two WHERE table_name.column_one == 'something'



