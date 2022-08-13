---
title: "Getting started with oozie"
slug: "getting-started-with-oozie"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Pre-requisites**

This article demonstrated installing oozie-4.3.0 on Hadoop 2.7.3

1. Java 1.7+
2. Hadoop 2.x (here, 2.7.3)
3. Maven3+
4. Unix box

**Step1: Dist file**

Get oozie tar.gz file from http://www-eu.apache.org/dist/oozie/4.3.0/ and extract it

    cd $HOME
    tar -xvf oozie-4.3.0.tar.gz

**Step2: Build Oozie**

    cd $HOME/oozie-4.3.0/bin
    ./mkdistro.sh -DskipTests
    
**Step3: Server Installation**

Copy the built binaries to the home directory as ‘oozie’

    cd $HOME
    cp -R $HOME/oozie-4.3.0/distro/target/oozie-4.3.0-distro/oozie-4.3.0 .
 
 **Step 3.1: libext**
 Create libext directory inside oozie directory

     cd $HOME/oozie
     mkdir libext
**Note**: ExtJS (2.2+) library (optional, to enable Oozie webconsole)
But, The ExtJS library is not bundled with Oozie because it uses a different license :(
Now you need to put hadoop jars inside libext directory, else it will throw below error in oozie.log file

> WARN ActionStartXCommand:523 - SERVER[data01.teg.io] USER[hadoop]
> GROUP[-] TOKEN[] APP[map-reduce-wf]
> JOB[0000000-161215143751620-oozie-hado-W]
> ACTION[0000000-161215143751620-oozie-hado-W@mr-node] Error starting
> action [mr-node]. ErrorType [TRANSIENT], ErrorCode [JA009], Message
> [JA009: Cannot initialize Cluster. Please check your configuration for
> mapreduce.framework.name and the correspond server addresses.]

So, let's put below jars inside libext directory

    cp $HADOOP_HOME/share/hadoop/common/*.jar oozie/libext/
    cp $HADOOP_HOME/share/hadoop/common/lib/*.jar oozie/libext/
    cp $HADOOP_HOME/share/hadoop/hdfs/*.jar oozie/libext/
    cp $HADOOP_HOME/share/hadoop/hdfs/lib/*.jar oozie/libext/
    cp $HADOOP_HOME/share/hadoop/mapreduce/*.jar oozie/libext/
    cp $HADOOP_HOME/share/hadoop/mapreduce/lib/*.jar oozie/libext/
    cp $HADOOP_HOME/share/hadoop/yarn/*.jar oozie/libext/
    cp $HADOOP_HOME/share/hadoop/yarn/lib/*.jar oozie/libext/

**Step 3.2: Oozie Impersonate**
To avoid impersonate error on oozie, modify core-site.xml like below

    <!-- OOZIE -->
      <property>
        <name>hadoop.proxyuser.[OOZIE_SERVER_USER].hosts</name>
        <value>[OOZIE_SERVER_HOSTNAME]</value>
      </property>
      <property>
        <name>hadoop.proxyuser.[OOZIE_SERVER_USER].groups</name>
        <value>[USER_GROUPS_THAT_ALLOW_IMPERSONATION]</value>
      </property>

Assuming, my oozie user is huser and host is localhost and group is hadoop

    <!-- OOZIE -->
      <property>
        <name>hadoop.proxyuser.huser.hosts</name>
        <value>localhost</value>
      </property>
      <property>
        <name>hadoop.proxyuser.huser.groups</name>
        <value>hadoop</value>
      </property>

Note : You can use * in all values, in case of confusion

**Step 3.3: Prepare the war**

    cd $HOME/oozie/bin
    ./oozie-setup.sh prepare-war
This will create oozie.war file inside oozie directory.
If this war will be used further, you may face this error :

> ERROR ActionStartXCommand:517 - SERVER[data01.teg.io] USER[hadoop]
> GROUP[-] TOKEN[] APP[map-reduce-wf]
> JOB[0000000-161220104605103-oozie-hado-W]
> ACTION[0000000-161220104605103-oozie-hado-W@mr-node] Error,
> java.lang.NoSuchFieldError: HADOOP_CLASSPATH

Why? because, The oozie compilation produced Hadoop 2.6.0 jars even when specifying Hadoop 2.7.3 with the option "-Dhadoop.version=2.7.3". 

So, to avoid this error, 
copy the oozie.war file to a different directory

    mkdir $HOME/oozie_war_dir
    cp $HOME/oozie/oozie.war $HOME/oozie_war_dir
    cd $HOME/oozie_war_dir
    jar -xvf oozie.war
    rm -f oozie.war/WEB-INF/lib/hadoop-*.jar
    rm -f oozie.war/WEB-INF/lib/hive-*.jar
    rm oozie.war
    jar -cvf oozie.war ./*
    cp oozie.war $HOME/oozie/

Then, regenerate the oozie.war binaries for oozie with a prepare-war

    cd $HOME/oozie/bin
    ./oozie-setup.sh prepare-war

**Step 3.4: Create sharelib on HDFS**

    cd $HOME/oozie/bin
    ./oozie-setup.sh sharelib create -fs hdfs://localhost:9000

Now, this sharelib set up may give you below error:

> org.apache.oozie.service.ServiceException: E0104: Could not fully
> initialize service [org.apache.oozie.service.ShareLibService], Not
> able to cache sharelib. An Admin needs to install the sharelib with
> oozie-setup.sh and issue the 'oozie admin' CLI command to update the
> sharelib

To avoid this, modify oozie-site.xml like below

    cd $HOME/oozie
    vi conf/oozie-site.xml

Add below property

    <property>
        <name>oozie.service.HadoopAccessorService.hadoop.configurations</name> 
        <value>*=/usr/local/hadoop/etc/hadoop/</value>
    </property>

The value should be your $HADOOP_HOME/etc/hadoop, where all hadoop configuration files are present.

**Step 3.5 : Create Oozie DB**

    cd $HOME/oozie
    ./bin/ooziedb.sh create -sqlfile oozie.sql -run
**Step 3.6 : Start Daemon**

To start Oozie as a daemon use the following command:

    ./bin/oozied.sh start

To stop

    ./bin/oozied.sh stop
check logs for errors, if any

    cd $HOME/oozie/logs
    tail -100f oozie.log
Use the following command to check the status of Oozie from command line:

    $ ./bin/oozie admin -oozie http://localhost:11000/oozie -status
    System mode: NORMAL
**Step 4: Client Installation**

    $ cd
    $ cp oozie/oozie-client-4.3.0.tar.gz .
    $ tar -xvf oozie-client-4.3.0.tar.gz
    $ mv oozie-client-3.3.2 oozie-client
    $ cd bin
Add $HOME/oozie-client/bin to PATH variable in .bashrc file and restart your terminal or do

    source $HOME/.bashrc

For more details on set up, you can refer this URL
https://oozie.apache.org/docs/4.3.0/DG_QuickStart.html

Now you can submit hadoop jobs to oozie in your terminal.

To run an example, you can follow this URL and set up your first example to run
https://oozie.apache.org/docs/4.3.0/DG_Examples.html

You may face below error while running the map reduce example in above URL

> java.io.IOException: java.net.ConnectException: Call From
> localhost.localdomain/127.0.0.1 to 0.0.0.0:10020 failed on connection
> exception: java.net.ConnectException: Connection refused; For more
> details see:  http://wiki.apache.org/hadoop/ConnectionRefused

**Solution:**
Start mr-jobhistory-server.sh

    cd $HADOOP_HOME/sbin
    ./mr-jobhistory-server.sh start historyserver

Another point to note about modifying job.properties file is :

    nameNode=hdfs://localhost:9000
    jobTracker=localhost:8032

in your case, this can be different, as I am using apache hadoop, you may be using cloudera/hdp/anything

    To run spark job, I have tried running in local[*], yarn-client and
    yarn-cluster as master, but succeeded in local[*] only

