---
title: "Calling scala jobs from pyspark"
slug: "calling-scala-jobs-from-pyspark"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

This document will show you how to call Scala jobs from a pyspark application. 

This approach can be useful when the Python API is missing some existing features from the Scala API or even to cope with performance issues using python. 

In some use cases, using Python is inevitable e.g you are building models with `scikit-learn`.

## How to call spark-submit
To call this code you should create the jar of your scala code. Than you have to call your spark submit like this:

    spark-submit --master yarn-client --jars ./my-scala-code.jar --driver-class-path ./my-scala-code.jar main.py

This will allow you to call any kind of scala code that you need in your pySpark jobs

## Creating a Scala functions that receives a python RDD
Creating a Scala function that receives an python RDD is easy. What you need to build is a function that get a JavaRDD[Any]

    import org.apache.spark.api.java.JavaRDD
    
    def doSomethingByPythonRDD(rdd :JavaRDD[Any]) = {
        //do something
        rdd.map { x => ??? }
    }

## Serialize and Send python RDD to scala code
This part of development you should serialize the python RDD to the JVM. This process uses the main development of Spark to call the jar function.

    from pyspark.serializers import PickleSerializer, AutoBatchedSerializer
    
    
    rdd = sc.parallelize(range(10000))
    reserialized_rdd = rdd._reserialize(AutoBatchedSerializer(PickleSerializer()))
    rdd_java = rdd.ctx._jvm.SerDe.pythonToJava(rdd._jrdd, True)
    
    _jvm = sc._jvm #This will call the py4j gateway to the JVM.
    _jvm.myclass.apps.etc.doSomethingByPythonRDD(rdd_java)

