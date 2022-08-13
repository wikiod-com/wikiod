---
title: "Text files and operations in Scala"
slug: "text-files-and-operations-in-scala"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

Reading Text files and performing operations on them.

## Example usage
Read text file from path:

    val sc: org.apache.spark.SparkContext = ???
    sc.textFile(path="/path/to/input/file") 

Read files using wildcards:

    sc.textFile(path="/path/to/*/*") 

Read files specifying minimum number of partitions:

    sc.textFile(path="/path/to/input/file", minPartitions=3)

## Join two files read with textFile()
Joins in Spark:

- Read textFile 1

      val txt1=sc.textFile(path="/path/to/input/file1") 

   Eg: 

        A B
        1 2
        3 4

- Read textFile 2

      val txt2=sc.textFile(path="/path/to/input/file2") 

   Eg: 

        A C
        1 5
        3 6

- Join and print the result.

      txt1.join(txt2).foreach(println)

     Eg: 

        A B C
        1 2 5
        3 4 6

The join above is based on the first column.

