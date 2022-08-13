---
title: "Spark Launcher"
slug: "spark-launcher"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

Spark Launcher can help developer to poll status of spark job submitted.
There are basically eight statuses that can be polled.They are listed below with there meaning::

    /** The application has not reported back yet. */
    UNKNOWN(false),
    /** The application has connected to the handle. */
    CONNECTED(false),
    /** The application has been submitted to the cluster. */
    SUBMITTED(false),
    /** The application is running. */
    RUNNING(false),
    /** The application finished with a successful status. */
    FINISHED(true),
    /** The application finished with a failed status. */
    FAILED(true),
    /** The application was killed. */
    KILLED(true),
    /** The Spark Submit JVM exited with a unknown status. */
    LOST(true);


 

## SparkLauncher
Below code is basic example of spark launcher.This can be used if spark job has to be launched through some application.


    val sparkLauncher = new SparkLauncher
    //Set Spark properties.only Basic ones are shown here.It will be overridden if properties are set in Main class.
    sparkLauncher.setSparkHome("/path/to/SPARK_HOME")
      .setAppResource("/path/to/jar/to/be/executed")
      .setMainClass("MainClassName")
      .setMaster("MasterType like yarn or local[*]")
      .setDeployMode("set deploy mode like cluster")
      .setConf("spark.executor.cores","2")
  
    // Lauch spark application
    val sparkLauncher1 = sparkLauncher.startApplication()

    //get jobId
    val jobAppId = sparkLauncher1.getAppId
    
    //Get status of job launched.THis loop will continuely show statuses like RUNNING,SUBMITED etc.
    while (true) {
        println(sparkLauncher1.getState().toString)
    }

