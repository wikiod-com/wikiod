---
title: "Getting started with spark-dataframe"
slug: "getting-started-with-spark-dataframe"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting spark-dataframe set up or installed.

## Loading Data Into A DataFrame
In Spark (scala) we can get our data into a DataFrame in several different ways, each for different use cases.

**Create DataFrame From CSV**

The easiest way to load data into a DataFrame is to load it from CSV file. An example of this (taken from the [official documentation][1]) is: 

    import org.apache.spark.sql.SQLContext
    
    val sqlContext = new SQLContext(sc)
    val df = sqlContext.read
        .format("com.databricks.spark.csv")
        .option("header", "true") // Use first line of all files as header
        .option("inferSchema", "true") // Automatically infer data types
        .load("cars.csv")

**Create DataFrame From RDD Implicitly**

Quite often in spark applications we have data in an RDD, but need to convert this into a DataFrame. The easiest way to do this is to use the `.toDF()` RDD function, which will implicitly determine the data types for our DataFrame:

    val data = List(
       ("John", "Smith", 30), 
       ("Jane", "Doe", 25)
    )

    val rdd = sc.parallelize(data)

    val df = rdd.toDF("firstname", "surname", "age")

**Create DataFrame From RDD Explicitly**

In some scenarios using the `.toDF()` approach is not the best idea, since we need to explicitly define the schema of our DataFrame. This can be achieved using a StructType containing an Array of StructField.

    import org.apache.spark.sql.types._
    import org.apache.spark.sql.Row
    
    val data = List(
       Array("John", "Smith", 30), 
       Array("Jane", "Doe", 25)
    )
    
    val rdd = sc.parallelize(data)
    
    val schema = StructType(
       Array(
          StructField("firstname", StringType,  true),
          StructField("surname",   StringType,  false),
          StructField("age",       IntegerType, true)
       )
    )
    
    val rowRDD = rdd.map(arr => Row(arr : _*))
    
    val df = sqlContext.createDataFrame(rowRDD, schema)


  [1]: https://github.com/databricks/spark-csv

