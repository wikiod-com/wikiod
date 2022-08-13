---
title: "What is HDFS?"
slug: "what-is-hdfs"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

A good explanation of HDFS and how it works.

Syntax should contain the commands which maybe use in HDFS.

## Finding files in HDFS
To find a file in the Hadoop Distributed file system:

    hdfs dfs -ls -R / | grep [search_term]

In the above command,
 
 `-ls` is for listing files
 
 `-R` is for recursive(iterate through sub directories)

 `/` means from the root directory

 `|` to pipe the output of first command to the second

 `grep` command to extract matching strings

 `[search_term]` file name to be searched for in the list of all files in the hadoop file system.

Alternatively the below command can also be used find and also apply some expressions:

`hadoop fs -find / -name test -print`

Finds all files that match the specified expression and applies selected actions to them. If no path is specified then defaults to the current working directory. If no expression is specified then defaults to -print.

The following primary expressions are recognised:

 - `name pattern`
 - `iname pattern`

  Evaluates as true if the basename of the file matches the pattern using standard file system globbing. If -iname is used then the match is case insensitive.

 - `print`
 - `print0Always`

Evaluates to true. Causes the current pathname to be written to standard output. If the `-print0` expression is used then an ASCII NULL character is appended.

The following operators are recognised:

    expression -a expression
    expression -and expression
    expression expression

## HDFS - Hadoop Distributed File System
Hadoop Distributed File System (HDFS) is a Java-based file system that provides scalable and reliable data storage that is designed to span large clusters of commodity servers. HDFS, MapReduce, and YARN form the core of Apache™ Hadoop®.

HDFS is designed to be highly fault-tolerant, which is achieved by saving multiple copies(3 by default) of a given data block across multiple nodes.

## Blocks and Splits HDFS
1) **Block Size and Blocks in HDFS** : HDFS has the concept of storing data in blocks whenever a file is loaded. Blocks are the physical partitions of data in HDFS ( or in any other filesystem, for that matter ).

    Whenever a file is loaded onto the HDFS, it is splitted physically (yes, the file is divided) into different parts known as blocks. The number of blocks depend upon the value of `dfs.block.size` in `hdfs-site.xml`
    
    Ideally, the block size is set to a large value such as 64/128/256 MBs (as compared to 4KBs in normal FS). The default block size value on most distributions of Hadoop 2.x is 128 MB. The reason for a higher block size is because Hadoop is made to deal with PetaBytes of data with each file ranging from a few hundred MegaBytes to the order of TeraBytes.
        
    Say for example you have a file of size 1024 MBs. if your block size is 128 MB, you will get 8 blocks of 128MB each. This means that your namenode will need to store metadata of `8 x 3  = 24` files (3 being the replication factor). 

    Consider the same scenario with a block size of 4 KBs. It will result in `1GB / 4KB = 250000` blocks and that will require the namenode to save the metadata for `750000` blocks for just a 1GB file. Since all these metadata related information is stored in-memory, larger block size is preferred to save that bit of extra load on the NameNode.

    Now again, the block size is not set to an extremely high value like 1GB etc because, ideally, 1 mapper is launched for each block of data. So if you set the block size to 1GB, you might lose parallelism which might result in a slower throughput overall.


2.) **Split Size in HDFS** :
    Splits in Hadoop Processing are the logical chunks of data. When files are divided into blocks, hadoop doesn't respect any file bopundaries. It just splits the data depending on the block size.
    Say if you have a file of 400MB, with 4 lines, and each line having 100MB of data, you will get 3 blocks of `128 MB x 3` and `16 MB x 1`. But when input splits are calculated while the prceossing of data, file/record boundaries are kept in mind and in this case we will have 4 input splits of 100 MB each, if you are using, say, `NLineInputFormat`.

   Split Size can also be set per job using the property `mapreduce.input.fileinputformat.split.maxsize`

   A very good explanation of Blocks vs Splits can be found in this [SO Answer](http://stackoverflow.com/questions/17727468/hadoop-input-split-size-vs-block-size)/

