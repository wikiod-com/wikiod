---
title : bigdata Tutorial
slug : bigdata-tutorial
weight : 9974
draft : false
images : []
type : docs
---

This section provides an overview of what bigdata is, and why a developer might want to use it.

Big data is the data characterized by the 4 V's. These are Volume, Velocity, Variety and Veracity.

 1. Volume - When the amount of data is in huge volume like Terabytes or Petabytes. As a report says, we have generated world's 90 % data over the last 2 or 3 years.
 2. Velocity - The speed at which data is flowing in the system. For instance, millions of users uploading their content on Social networking Sites at the same time generates data as high as in the range of Terabytes/sec.
 3. Variety - Different types of data based on its nature. It can be Structured(which most of the old RDBMS deals with), Semi-Structured(email, XML etc.) and Unstructured(Videos, Audios, Sensor Data etc.).
 4. Veracity - It is the means with which we get a meaningful insight in our available data. This can be considered as the most important aspect of data as most of the business decision depends on the usefulness of data.

The most general platform used to store and process big data is the **Hadoop Framework.** It consists of 2 things:

 1. Hadoop Distributed File System(HDFS) - Data is stored on Hadoop Distributed File System(HDFS) which is actually a cluster of commodity hardware unlike the primitive way of storing on servers.The data resides on HDFS and maybe processed to derive insights using various tools and frameworks.
 2. MapReduce(MR) - This is the default processing framework for Hadoop.[MapReduce](https://www.wikiod.com/docs/mapreduce) (is a part of Apache Hadoop)

With an advancement in Hadoop , new processing tools started emerging in the Hadoop Community.Few of the most popular tools/frameworks:
1. [Apache Spark](https://www.wikiod.com/docs/apache-spark)
2. [Apache Storm]()
3. [Apache Flink](https://www.wikiod.com/docs/apache-flink)

   And many more..

Few of the storage mechanisms other than plain HDFS:
1. [Hive](https://www.wikiod.com/docs/hive)
2. [HBase](https://www.wikiod.com/docs/hbase)
3. [Cassandra](https://www.wikiod.com/docs/cassandra)

  And many more..

A developer might be interested in the processing capabilities of big data so that it can prove to be a major difference in how we look at our data. In a parallel universe, we can also call big data as Rich-untamed-Data. We have to tame this huge data.With big data we might be able to process the hidden potential of already existing data.
----------
*A best example can be cited in the customer click behavior over the shopping websites wherein their views, clicks and the amount of time spent on that website, tells the online retailer to procure product and send recommendations based on user behavior.*

