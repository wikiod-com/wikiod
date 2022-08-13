---
title: "Spark API (SparkR)"
slug: "spark-api-sparkr"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

The `SparkR` package let's you work with distributed data frames on top of a [Spark cluster][1]. These allow you to do operations like selection, filtering, aggregation on very large datasets.
[SparkR overview][2]
[SparkR package documentation][3]


  [1]: http://spark.apache.org/
  [2]: https://spark.apache.org/docs/latest/sparkr.html
  [3]: https://spark.apache.org/docs/1.5.1/api/R/

## Setup Spark context
## Setup Spark context in R ##

To start working with Sparks distributed dataframes, you must connect your R program with an existing Spark Cluster.

 

    library(SparkR)
    sc <- sparkR.init() # connection to Spark context
    sqlContext <- sparkRSQL.init(sc) # connection to SQL context


[Here are infos][1] how to connect your IDE to a Spark cluster.

## Get Spark Cluster ##

There is an [Apache Spark introduction topic][2] with install instructions. Basically, you can employ a Spark Cluster locally via java ([see instructions][3]) or use (non-free) cloud applications (e.g. [Microsoft Azure][4] [ \[topic site\]][5], [IBM][6]).


  [1]: https://spark.apache.org/docs/1.6.0/sparkr.html#starting-up-from-rstudio
  [2]: https://www.wikiod.com/apache-spark/getting-started-with-apache-spark
  [3]: http://spark.apache.org/docs/latest/
  [4]: https://azure.microsoft.com/en-us/services/hdinsight/apache-spark/
  [5]: https://www.wikiod.com/azure
  [6]: http://www.ibm.com/analytics/us/en/technology/spark/

## Cache data
What: 

Caching can optimize computation in Spark. Caching stores data in memory and is a special case of persistence. [Here is explained][1] what happens when you cache an RDD in Spark.

Why:

Basically, caching saves an interim partial result - usually after transformations - of your original data. So, when you use the cached RDD, the already transformed data from memory is accessed without recomputing the earlier transformations.

How:

Here is an example how to quickly access large data *(here 3 GB big csv)* from in-memory storage when accessing it more then once:


    library(SparkR)
    # next line is needed for direct csv import:
    Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.4.0" "sparkr-shell"')
    sc <- sparkR.init()
    sqlContext <- sparkRSQL.init(sc)

    # loading 3 GB big csv file:  
    train <- read.df(sqlContext, "/train.csv", source = "com.databricks.spark.csv", inferSchema = "true")
    cache(train)
    system.time(head(train))
    # output: time elapsed: 125 s. This action invokes the caching at this point.
    system.time(head(train))
    # output: time elapsed: 0.2 s (!!)


  [1]: http://stackoverflow.com/a/28983767/3889242

## Create RDDs (Resilient Distributed Datasets)
From dataframe:
---------------

    mtrdd <- createDataFrame(sqlContext, mtcars)

From csv:
---------
For csv's, you need to add the [csv package][1] to the environment before initiating the Spark context: 

    Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.4.0" "sparkr-shell"') # context for csv import read csv -> 
    sc <- sparkR.init()
    sqlContext <- sparkRSQL.init(sc)


Then, you can load the csv either by infering the data schema of the data in the columns:

    train <- read.df(sqlContext, "/train.csv", header= "true", source = "com.databricks.spark.csv", inferSchema = "true")

Or by specifying the data schema beforehand:

  

     customSchema <- structType(
        structField("margin", "integer"),
        structField("gross", "integer"),
        structField("name", "string"))

     train <- read.df(sqlContext, "/train.csv", header= "true", source = "com.databricks.spark.csv", schema = customSchema)


  [1]: https://github.com/databricks/spark-csv

