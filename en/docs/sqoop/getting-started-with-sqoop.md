---
title: "Getting started with sqoop"
slug: "getting-started-with-sqoop"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup

    

Sqoop ships as one binary package however it’s compound from two separate parts client and server. 
    You need to install server on single node in your cluster. 
    This node will then serve as an entry point for all connecting Sqoop clients. 
    Server acts as a mapreduce client and therefore Hadoop must be installed and configured on machine hosting Sqoop server. 
    Clients can be installed on any arbitrary number of machines. 
    Client is not acting as a mapreduce client and thus you do not need to install Hadoop on nodes that will act only as a Sqoop client.

  

  Copy Sqoop artifact on machine where you want to run Sqoop server. 
    This machine must have installed and configured Hadoop. 
    You don’t need to run any Hadoop related services there, however the machine must be able to act as an Hadoop client. 
    

    # Extract Sqoop  tar
    tar -xf sqoop-<version>-bin-hadoop<hadoop-version>.tar.gz

    # Move decompressed content to any location 
      (you can also setup soft links to sqoop directory)
    mv sqoop-<version>-bin-hadoop<hadoop version>.tar.gz /opt/apache/sqoop

    # Change working directory
    cd /opt/apache/sqoop


    

Install Dependencies for SQOOP
------------------------------

 You need to install Hadoop libraries into Sqoop server war file. Sqoop provides convenience script addtowar.sh to do so. 
    
   If you have installed Hadoop in usual location in /usr/lib and executable hadoop is in your path, you can use automatic Hadoop installation procedure:

    ./bin/addtowar.sh -hadoop-auto

In case that you have Hadoop installed in different location, you will need to manually specify Hadoop version and path to Hadoop libraries. You can use parameter -hadoop-version for specifying Hadoop major version,

    ./bin/addtowar.sh -hadoop-version 2.0 -hadoop-path /usr/lib/hadoop-common:/usr/lib/hadoop-hdfs:/usr/lib/hadoop-yarn

 - Installed required JDBC jars for sqoop to connect to database

`./bin/addtowar.sh -jars /path/to/jar/mysql-connector-java-*-bin.jar`

Start and Stop Sqoop Server Services
------------------------------------

    
    ./bin/sqoop.sh server start
    ./bin/sqoop.sh server stop

Sqoop Client Configuration steps
--------------------------------

Copy Sqoop distribution artifact on target machine and unzip it in desired location. You can start client with following command:

    bin/sqoop.sh client

Sqoop 2 client have ability to load resource files similarly as other command line tools. At the beginning of execution Sqoop client will check existence of file .sqoop2rc in home directory of currently logged user. If such file exists, it will be interpreted before any additional actions. This file is loaded in both interactive and batch mode. It can be used to execute any batch compatible commands.

Example resource file:

    # Configure our Sqoop 2 server automatically
    set server --host sqoop2.company.net

    # Run in verbose mode by default
    set option --name verbose --value true



