---
title: "Getting started with hbase"
slug: "getting-started-with-hbase"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing HBase in Standalone
HBase Standalone is a mode which allow you to get rid of HDFS and to test HBase before deploying in a cluster, **It is not production oriented.**

Installing HBase in standalone is extremely simple. First you have to download the HBase archive named `hbase-X.X.X-bin.tar.gz` available on one of the [apache mirrors][1].

Once you have done this, execute this shell command
    
    tar xzvf hbase-X.X.X-bin.tar.gz

It will export the archive in your directory, you can put it wherever you want.

Now, go to the HBase directory you have exported and edit the file conf/hbase-env.sh

    cd hbase-X.X.X
    vi -o conf/hbase-env.xml

In this file, uncomment the line and change the path of JAVA_HOME

    JAVA_HOME=/usr    #The directory must contain bin/java

Almost there ! now edit the file `conf/hbase-sitexml` and put the folowing lines 

    <configuration>
      <property>
        <name>hbase.rootdir</name>
        <value>file:///home/user/hbase</value>
      </property>
      <property>
        <name>hbase.zookeeper.property.dataDir</name>
        <value>/home/user/zookeeper</value>
      </property>
    </configuration>

You can put those directories wherever you want to, just be sure to remember it if you want to check logs etc.

Your HBase is now ready to run ! Just execute the command

    bin/start-hbase.sh

and if you want to stop HBase

    bin/stop-hbase.sh


Now your HBase is launched on your localhost and you can access it (using the Java API or the HBase shell). To run HBase shell, use

    bin/hbase shell


Have fun using HBase ! 

  [1]: http://www.apache.org/dyn/closer.cgi/hbase/

## Installing HBase in cluster
TODO

