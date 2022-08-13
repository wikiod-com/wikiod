---
title: "Partitions"
slug: "partitions"
draft: false
images: []
weight: 9850
type: docs
toc: true
---

The number of partitions is critical for an application's performance and/or successful termination.

A Resilient Distributed Dataset (RDD) is Spark's main abstraction. An RDD is split into partitions, that means that a partition is a part of the dataset, a slice of it, or in other words, a chunk of it.

The greater the number of partitions is, the smaller the size of each partition is.

However, notice that a large number of partitions puts a lot of pressure on Hadoop Distributed File System (HDFS), which has to keep a significant amount of metadata.

The number of partitions is related to the memory usage, and a memoryOverhead issue can be related to this number ([personal experience][1]).

---

A **common pitfall** for new users is to transform their RDD into an RDD with only one partition, which usually looks like that:

    data = sc.textFile(file)
    data = data.coalesce(1) 

That's usually a very bad idea, since you are telling Spark to put **all the data** is just one partition! Remember that:

>A stage in Spark will operate on one partition at a time (and load the data in that partition into memory).

As a result, you tell Spark to handle all the data at once, which usually results in memory related errors (Out of Memory for example), or even a null pointer exception.

So, unless you know what you are doing, avoid repartitioning your RDD in just one partition!


  [1]: https://gsamaras.wordpress.com/code/memoryoverhead-issue-in-spark/

## Partitions Intro
> How does an RDD gets partitioned?

By default a partition is created for each HDFS partition, which by default is 64MB. Read more [here][1].

> How to balance my data across partitions?

First, take a look at the three ways one can *repartition* his data:

1) Pass a second parameter, the desired *minimum* number of partitions
    for your RDD, into [textFile()][2], but be careful:

    In [14]: lines = sc.textFile("data")
    
    In [15]: lines.getNumPartitions()
    Out[15]: 1000
    
    In [16]: lines = sc.textFile("data", 500)
    
    In [17]: lines.getNumPartitions()
    Out[17]: 1434
    
    In [18]: lines = sc.textFile("data", 5000)
    
    In [19]: lines.getNumPartitions()
    Out[19]: 5926

As you can see, `[16]` doesn't do what one would expect, since the number of partitions the RDD has, is already greater than the minimum number of partitions we request.

2) Use [repartition()][3], like this:

    In [22]: lines = lines.repartition(10)
    
    In [23]: lines.getNumPartitions()
    Out[23]: 10

Warning: This will invoke a shuffle and should be used when you want to **increase** the number of partitions your RDD has.

From the [docs][4]:

>The shuffle is Spark’s mechanism for re-distributing data so that it’s grouped differently across partitions. This typically involves copying data across executors and machines, making the shuffle a complex and costly operation.

3) Use [coalesce()][5], like this:

    In [25]: lines = lines.coalesce(2)
    
    In [26]: lines.getNumPartitions()
    Out[26]: 2

Here, Spark knows that you will shrink the RDD and gets advantage of it. Read more about [repartition() vs coalesce()][6].

---

But will all this **guarantee** that your data will be perfectly balanced across your partitions? Not really, as I experienced in http://stackoverflow.com/questions/38799753/how-to-balance-my-data-across-the-partitions


  [1]: http://stackoverflow.com/questions/26368362/how-does-partitioning-work-in-spark
  [2]: http://spark.apache.org/docs/1.6.2/api/python/pyspark.html?highlight=textfile#pyspark.SparkContext.textFile
  [3]: http://spark.apache.org/docs/1.6.2/api/python/pyspark.html?highlight=repartition#pyspark.RDD.repartition
  [4]: http://spark.apache.org/docs/1.6.2/programming-guide.html#shuffle-operations
  [5]: http://spark.apache.org/docs/1.6.2/api/python/pyspark.html?highlight=coalesce#pyspark.RDD.coalesce
  [6]: http://stackoverflow.com/questions/31610971/spark-repartition-vs-coalesce

## Repartition an RDD
Sometimes we want to repartition an RDD, for example because it comes from a file that wasn't created by us, and the number of partitions defined from the creator is not the one we want.

The two most known functions to achieve this are:

    repartition(numPartitions)

and:

    coalesce(numPartitions, shuffle=False)

As a rule of thumb, use the first when you want to repartition your RDD in a greater number of partitions and the second to reduce your RDD, in a smaller number of partitions. [Spark - repartition() vs coalesce()](http://stackoverflow.com/questions/31610971/spark-repartition-vs-coalesce).

For example:

    data = sc.textFile(file)
    data = data.coalesce(100) // requested number of #partitions

will decrease the number of partitions of the RDD called 'data' to 100, given that this RDD has more than 100 partitions when it got read by `textFile()`.

And in a similar way, if you want to have more than the current number of partitions for your RDD, you could do (given that your RDD is distributed in 200 partitions for example):

    data = sc.textFile(file)
    data = data.repartition(300) // requested number of #partitions



## Partitions of an RDD
As mentioned in "Remarks", a partition is a part/slice/chunk of an RDD. Below is a minimal example on how to request a minimum number of partitions for your RDD:

    In [1]: mylistRDD = sc.parallelize([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 2)
    
    In [2]: mylistRDD.getNumPartitions()
    Out[2]: 2

Notice in [1] how we passed 2 as a second parameter of `parallelize()`. That parameter says that we want our RDD to has at least 2 partitions.

## Rule of Thumb about number of partitions
As rule of thumb, one would want his RDD to have as many partitions as the product of the number of executors by the number of used cores by 3 (or maybe 4). Of course, that's a heuristic and it really depends on your application, dataset and cluster configuration.

Example:

    In [1]: data  = sc.textFile(file)
    
    In [2]: total_cores = int(sc._conf.get('spark.executor.instances')) * int(sc._conf.get('spark.executor.cores'))
    
    In [3]: data = data.coalesce(total_cores * 3)      



## Show RDD contents
To show contents of an RDD, it have to be printed:

    myRDD.foreach(println)

To limit number of rows printed:

    myRDD.take(num_of_rows).foreach(println)

