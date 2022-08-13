---
title: "Joins"
slug: "joins"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

One thing to note is your resources versus the size of data you are joining. This is where your Spark Join code might fail giving you memory errors. For this reason make sure you configure your Spark jobs really well depending on the size of data. Following is an example of a configuration for a join of 1.5 million to 200 million. 

> Using Spark-Shell

  
    spark-shell   --executor-memory 32G   --num-executors 80  --driver-memory 10g --executor-cores 10   

> Using Spark Submit  

    spark-submit   --executor-memory 32G   --num-executors 80  --driver-memory 10g --executor-cores 10 code.jar 

 

## Broadcast Hash Join in Spark
A broadcast join copies the small data to the worker nodes which leads to a highly efficient and super-fast join. When we are joining two datasets and one of the datasets is much smaller than the other (e.g when the small dataset can fit into memory), then we should use a Broadcast Hash Join.

The following image visualizes a Broadcast Hash Join whre the the small dataset is broadcasted to each partition of the Large Dataset.  

[![enter image description here][1]][1]  
  
Following is code sample which you can easily implement if you have a similar scenario of a large and small dataset join.

    case class SmallData(col1: String, col2:String, col3:String, col4:Int, col5:Int)
     
    val small = sc.textFile("/datasource")
     
    val df1 = sm_data.map(_.split("\\|")).map(attr => SmallData(attr(0).toString, attr(1).toString, attr(2).toString, attr(3).toInt, attr(4).toInt)).toDF()
     
    val lg_data = sc.textFile("/datasource")
     
    case class LargeData(col1: Int, col2: String, col3: Int)
     
    val LargeDataFrame = lg_data.map(_.split("\\|")).map(attr => LargeData(attr(0).toInt, attr(2).toString, attr(3).toInt)).toDF()
     
     
    val joinDF = LargeDataFrame.join(broadcast(smallDataFrame), "key")


  [1]: https://i.stack.imgur.com/wOo4T.jpg

