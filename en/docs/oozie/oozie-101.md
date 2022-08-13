---
title: "Oozie 101"
slug: "oozie-101"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Oozie Architecture
Oozie is developed on a client-server architecture. Oozie server is a Java web application that runs Java servlet container within an embedded Apache Tomcat. Oozie provides three different type of clients to interact with the Oozie server: Command Line, Java Client API and HTTP REST API.

Oozie server does not store any in-memory information of the running jobs. It relies on RDBMS to store states and data of all the Oozie jobs. Every time it retrieves the job information from the database and stores updated information back into the database.

Oozie Server (can) sits outside of the Hadoop cluster and performs orchestration of the Hadoop jobs defined in a Oozie Workflow job.


## Oozie Application Deployment
A simplest Oozie application is consists of a workflow logic file (workflow.xml), workflow properties file (job.properties/job.xml) and required JAR files, scripts and configuration files. Except the workflow properties file, all the other files should to be stored in a HDFS location. The workflow properties file should be available locally, from where Oozie application is submitted and started.

The HDFS directory, where workflow.xml is stored along with other scripts and configuration files, is called Oozie workflow application directory. All the JAR files should be stored under a /lib directory in the oozie application directory.

The more complex Oozie applications can consist of coordinators (coordinator.xml) and bundle (bundle.xml) logic files. These files are also stored in the HDFS into a respective Oozie application directory.

## How to pass configuration with Oozie Proxy Job submission
When using the Oozie Proxy job submission API for submitting the Oozie `Hive`, `Pig`, and `Sqoop` actions. To pass any configuration to the action, is required to be in below format.

**For Hive action:**

 - oozie.hive.options.size : The number of options you'll be passing to Hive action.
 - oozie.hive.options.n : An argument to pass to Hive, the 'n' should be an integer starting with zero (0) to indicate the option number.


    <property>
        <name>oozie.hive.options.1</name>
        <value>-Doozie.launcher.mapreduce.job.queuename=hive</value>
    </property>
    <property>
        <name>oozie.hive.options.0</name>
        <value>-Dmapreduce.job.queuename=hive</value>
    </property>
    <property>
        <name>oozie.hive.options.size</name>
        <value>2</value>
    </property>


**For Pig Action:**

 - oozie.pig.options.size : The number of options you'll be passing to Pig action.
 - oozie.pig.options.n : An argument to pass to Pig, the 'n' should be an integer starting with zero (0) to indicate the option number.


    <property>
        <name>oozie.pig.options.1</name>
        <value>-Doozie.launcher.mapreduce.job.queuename=pig</value>
    </property>
    <property>
        <name>oozie.pig.options.0</name>
        <value>-Dmapreduce.job.queuename=pig</value>
    </property>
    <property>
        <name>oozie.pig.options.size</name>
        <value>2</value>
    </property>

**For Sqoop Action:**

 - oozie.sqoop.options.size : The number of options you'll be passing to Sqoop Hadoop job.
 - oozie.sqoop.options.n : An argument to pass to Sqoop.
   hadoop job conf, the 'n' should be an integer starting with zero(0)
   to indicate the option number.


    <property>
        <name>oozie.sqoop.options.1</name>
        <value>-Doozie.launcher.mapreduce.job.queuename=sqoop</value>
    </property>
    <property>
        <name>oozie.sqoop.options.0</name>
        <value>-Dmapreduce.job.queuename=sqoop</value>
    </property>
    <property>
        <name>oozie.sqoop.options.size</name>
        <value>2</value>
    </property>

