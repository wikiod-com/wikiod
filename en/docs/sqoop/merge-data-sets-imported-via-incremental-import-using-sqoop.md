---
title: "merge data-sets imported via incremental import using Sqoop"
slug: "merge-data-sets-imported-via-incremental-import-using-sqoop"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Sqoop incremental import comes into picture because of a phenomenon called CDC i.e. **Change Data Capture.** Now what is CDC?

> CDC is a design pattern that captures individual data changes instead of dealing with the entire data. Instead of dumping our entire database, using CDC, we could capture just the data changes made to the master database.

For example : If we are dealing with a data problem, say, 1 lakh data entries coming into the RDBMS daily and we have to get this data in Hadoop on a daily basis then we would want to just get the newly added data, as importing the complete RDBMS data daily to Hadoop will be an overhead and delays the availability of data also. For a detailed explanation go through this [link.][1]


  [1]: https://www.flydata.com/blog/what-change-data-capture-cdc-is-and-why-its-important/

## Import New Data - append mode
If you are only adding new rows in your RDBMS (*not updating existing data*)

You need two additional parameters: 

- `--check-column` : A column name that should be checked for newly appended data. `Integer` would be a suitable data type for this column.
- `--last-value` : The last value that successfully imported into Hadoop. All the newly added data after this value will be imported.


    sqoop import \
    --connect jdbc:mysql://mysql.example.com/testdb \
    --username sqoop \
    --password sqoop \
    --table employee \
    --incremental append \
    --check-column id \
    --last-value 100

## Import New as well as Updated Data - lastmodified mode
If you are adding new rows and updating existing data.

You need two additional parameters:

- `--check-column` : A column name that should be checked for newly appended and updated data. `date`, `time`, `datetime` and `timestamp` are suitable data types for this column.
- `--last-value` : The last value that successfully imported into Hadoop. All the newly added and updated data after this value will be imported.


    sqoop import \
    --connect jdbc:mysql://mysql.example.com/testdb \
    --username sqoop \
    --password sqoop \
    --table employee \
    --incremental lastmodified \
    --check-column last_update_date \
    --last-value "2015-10-20 06:00:01"



