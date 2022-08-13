---
title: "Configuration Apache Spark SQL"
slug: "configuration-apache-spark-sql"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

In this topic Spark Users can find different configurations of Spark SQL, which is the most used component of Apache Spark framework.

## Controlling Spark SQL Shuffle Partitions
In Apache Spark while doing shuffle operations like `join` and `cogroup` a lot of data gets transferred across network. Now, to control the number of partitions over which shuffle happens can be controlled by configurations given in Spark SQL. That configuration is as follows:

    spark.sql.shuffle.partitions

Using this configuration we can control the number of partitions of shuffle operations. By default, its value is `200`. But, 200 partitions does not make any sense if we have files of few GB(s). So, we should change them according to the amount of data we need to process via Spark SQL. Like as follows:

In this scenario we have two tables to be joined `employee` and `department`. Both tables contains only few records only, but we need to join them to get to know the department of each employee. So, we join them using Spark DataFrames like this:

    val conf = new SparkConf().setAppName("sample").setMaster("local")
    val sc = new SparkContext(conf)
    
    val employee = sc.parallelize(List("Bob", "Alice")).toDF("name")
    val department = sc.parallelize(List(("Bob", "Accounts"), ("Alice", "Sales"))).toDF("name", "department")
    
    employeeDF.join(departmentDF, "employeeName").show()

Now, the number of partitions that gets created while doing join are 200 by default which is of course too much for this much amount of data.

So, lets change this value so that we can reduce the number of shuffle operations.

    val conf = new SparkConf().setAppName("sample").setMaster("local").set("spark.sql.shuffle.partitions", 2)
    val sc = new SparkContext(conf)
    
    val employee = sc.parallelize(List("Bob", "Alice")).toDF("name")
    val department = sc.parallelize(List(("Bob", "Accounts"), ("Alice", "Sales"))).toDF("name", "department")
    
    employeeDF.join(departmentDF, "employeeName").show()

Now, the number of shuffle partitions are reduced to only 2, which will not only reduce the number of shuffling operations but also reduce the time taken to join the DataFrames from `0.878505 s` to `0.077847 s`.

So, always configure the number of partitions for shuffle operations according to the data being processed.

