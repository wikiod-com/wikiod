---
title: "Sqoop Export"
slug: "sqoop-export"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Sqoop Export basic example
The export tool exports a set of files from HDFS back to an RDBMS. The target table must already exist in the database. The input files are read and parsed into a set of records according to the user-specified delimiters.

Example : 

    sqoop export \
    --connect="jdbc:<databaseconnector>" \
    --username=<username> \
    --password=<password> \
    --export-dir=<hdfs export directory> \
    --table=<tablename> 



