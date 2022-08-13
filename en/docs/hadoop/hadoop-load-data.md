---
title: "Hadoop load data"
slug: "hadoop-load-data"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Load data into hadoop hdfs
STEP 1: CREATE A DIRECTORY IN HDFS, UPLOAD A FILE AND LIST CONTENTS

Let’s learn by writing the syntax. You will be able to copy and paste the following example commands into your terminal:

# hadoop fs -mkdir:
Takes the path URI’s as an argument and creates a directory or multiple directories.
# Usage:
        # hadoop fs -mkdir <paths>
# Example:
        hadoop fs -mkdir /user/hadoop
        hadoop fs -mkdir /user/hadoop/dir1 /user/hadoop/dir2 /user/hadoop/dir3

# hadoop fs -put:

Copies single src file or multiple src files from local file system to the Hadoop Distributed File System.
# Usage:
        # hadoop fs -put <local-src> ... <HDFS_dest_path>
# Example:
        hadoop fs -put popularNames.txt /user/hadoop/dir1/popularNames.txt

# hadoop fs -copyFromLocal:

Copies single src file or multiple src files from local file system to the Hadoop Distributed File System.
# Usage:
        # hadoop fs -copyFromLocal <local-src> ... <HDFS_dest_path>
# Example:
        hadoop fs -copyFromLocal popularNames.txt /user/hadoop/dir1/popularNames.txt

# hadoop fs -moveFromLocal:

Similar to put command, except that the source localsrc is deleted after it’s copied.
# Usage:
        # hadoop fs -moveFromLocal <local-src> ... <HDFS_dest_path>
# Example:
        hadoop fs -moveFromLocal popularNames.txt /user/hadoop/dir1/popularNames.txt
**SQOOP DATA TRANSFER TOOL:**

We can also load data into HDFS directly from Relational databases using Sqoop(a command line tool for data transfer from RDBMS to HDFS and vice versa).

# Usage:

    $ sqoop import --connect CONNECTION_STRING --username USER_NAME --table TABLE_NAME

# Example:

    $ sqoop import --connect jdbc:mysql://localhost/db --username foo --table TEST

