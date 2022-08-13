---
title: "Spark DataFrame"
slug: "spark-dataframe"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

A DataFrame is an abstraction of data organized in rows and typed columns. It is similar to the data found in relational SQL-based databases. Although it has been transformed into just a type alias for Dataset[Row] in Spark 2.0, it is still widely used and useful for complex processing pipelines making use of its schema flexibility and SQL-based operations.

## Creating DataFrames in Scala
There are many ways of creating DataFrames. They can be created from local lists, distributed RDDs or reading from datasources.

# Using toDF

<!-- language-all: scala -->

By importing spark sql implicits, one can create a DataFrame from a local Seq, Array or RDD, as long as the contents are of a Product sub-type (tuples and case classes are well-known examples of Product sub-types). For example:

    import sqlContext.implicits._
    val df = Seq(
      (1, "First Value", java.sql.Date.valueOf("2010-01-01")),
      (2, "Second Value", java.sql.Date.valueOf("2010-02-01"))
    ).toDF("int_column", "string_column", "date_column")

# Using createDataFrame

Another option is using the [`createDataFrame`][1] method present in SQLcontext. This option also allows the creation from local lists or RDDs of Product sub-types as with `toDF`, but the names of the columns are not set in the same step. For example:

    val df1 = sqlContext.createDataFrame(Seq(
      (1, "First Value", java.sql.Date.valueOf("2010-01-01")),
      (2, "Second Value", java.sql.Date.valueOf("2010-02-01"))
    ))
 
Additionally, this approach allows creation from RDDs of `Row` instances, as long as a `schema` parameter is passed along for the definition of the resulting DataFrame's schema. Example:

    import org.apache.spark.sql.types._
    val schema = StructType(List(
        StructField("integer_column", IntegerType, nullable = false),
        StructField("string_column", StringType, nullable = true),
        StructField("date_column", DateType, nullable = true)
    ))

    val rdd = sc.parallelize(Seq(
      Row(1, "First Value", java.sql.Date.valueOf("2010-01-01")),
      Row(2, "Second Value", java.sql.Date.valueOf("2010-02-01"))
    ))

    val df = sqlContext.createDataFrame(rdd, schema)

# Reading from sources

Maybe the most common way to create DataFrame is from datasources. One can create it from a parquet file in hdfs, for example:

    val df = sqlContext.read.parquet("hdfs:/path/to/file")

  [1]: http://spark.apache.org/docs/latest/api/scala/index.html#org.apache.spark.sql.SQLContext

