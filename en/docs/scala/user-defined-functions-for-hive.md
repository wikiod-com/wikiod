---
title: "User Defined Functions for Hive"
slug: "user-defined-functions-for-hive"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## A simple Hive UDF within Apache Spark
    import org.apache.spark.sql.functions._

    // Create a function that uses the content of the column inside the dataframe
    val code = (param: String) => if (param == "myCode") 1 else 0
    // With that function, create the udf function
    val myUDF = udf(code)
    // Apply the udf to a column inside the existing dataframe, creating a dataframe with the additional new column
    val newDataframe = aDataframe.withColumn("new_column_name", myUDF(col(inputColumn)))

