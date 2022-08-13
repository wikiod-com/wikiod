---
title: "Indexing"
slug: "indexing"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Structure
    CREATE INDEX index_name
    ON TABLE base_table_name (col_name, ...)
    AS 'index.handler.class.name'
    [WITH DEFERRED REBUILD]
    [IDXPROPERTIES (property_name=property_value, ...)]
    [IN TABLE index_table_name]
    [PARTITIONED BY (col_name, ...)]
    [
     [ ROW FORMAT ...] STORED AS ...
     | STORED BY ...
    ]
    [LOCATION hdfs_path]
    [TBLPROPERTIES (...)]


**Example:**

    CREATE INDEX inedx_salary ON TABLE employee(salary) AS 'org.apache.hadoop.hive.ql.index.compact.CompactIndexHandler' WITH DEFERRED REBUILD;

**Alter Index**

ALTER INDEX index_name ON table_name [PARTITION (...)] REBUILD

**Drop Index**

    DROP INDEX <index_name> ON <table_name>


If WITH DEFERRED REBUILD is specified on CREATE INDEX, then the newly created index is initially empty (regardless of whether the table contains any data).

The ALTER INDEX REBUILD command can be used to build the index structure for all partitions or a single partition.


