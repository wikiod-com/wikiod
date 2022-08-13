---
title: "Getting started with hadoop"
slug: "getting-started-with-hadoop"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of Hadoop on ubuntu
 

Creating Hadoop User:
=====================

    sudo addgroup hadoop

Adding a user:
==============

    sudo adduser --ingroup hadoop hduser001

[![enter image description here][1]][1]

Configuring SSH:
===============

    su -hduser001
    ssh-keygen -t rsa -P ""
    cat .ssh/id rsa.pub >> .ssh/authorized_keys

**Note**: If you get errors [*bash: .ssh/authorized_keys: No such file or directory*] whilst writing the authorized key. Check [here][2].

[![enter image description here][3]][3]
[![enter image description here][4]][4]
[![enter image description here][5]][5]

Add hadoop user to sudoer's list:
================================

    sudo adduser hduser001 sudo
[![enter image description here][6]][6]

Disabling IPv6:
==============
[![enter image description here][7]][7]
[![enter image description here][8]][8]

Installing Hadoop:
=================

    sudo add-apt-repository ppa:hadoop-ubuntu/stable
    sudo apt-get install hadoop

[![enter image description here][9]][9]
[![enter image description here][10]][10]


  [1]: http://i.stack.imgur.com/dRsz0.png
  [2]: http://askubuntu.com/questions/466549/bash-home-user-ssh-authorized-keys-no-such-file-or-directory
  [3]: http://i.stack.imgur.com/6pNSe.png
  [4]: http://i.stack.imgur.com/cpau3.png
  [5]: http://i.stack.imgur.com/3c2nb.png
  [6]: http://i.stack.imgur.com/ICB9j.png
  [7]: http://i.stack.imgur.com/UzX4h.png
  [8]: http://i.stack.imgur.com/znIbd.png
  [9]: http://i.stack.imgur.com/xj5jD.png
  [10]: http://i.stack.imgur.com/oamjE.png

## Installation or Setup on Linux
A Pseudo Distributed Cluster Setup Procedure

**Prerequisites**

 - Install JDK1.7 and set JAVA_HOME environment variable.

 - Create a new user as "hadoop".

    `useradd hadoop`

 - Setup password-less SSH login to its own account
    
        su - hadoop
        ssh-keygen
        << Press ENTER for all prompts >>
        cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
        chmod 0600 ~/.ssh/authorized_keys
     
 - Verify by performing `ssh localhost`

 - Disable IPV6  by editing `/etc/sysctl.conf` with the followings:

        net.ipv6.conf.all.disable_ipv6 = 1
        net.ipv6.conf.default.disable_ipv6 = 1
        net.ipv6.conf.lo.disable_ipv6 = 1

 - Check that using `cat /proc/sys/net/ipv6/conf/all/disable_ipv6`
    
      (should return 1)
    
**Installation and Configuration:**

 - Download required version of Hadoop from Apache archives using `wget` command.
        
        cd /opt/hadoop/
        wget http:/addresstoarchive/hadoop-2.x.x/xxxxx.gz
        tar -xvf hadoop-2.x.x.gz
        mv hadoop-2.x.x.gz hadoop 
       (or)

        ln -s hadoop-2.x.x.gz hadoop
        chown -R hadoop:hadoop hadoop
    
 - Update `.bashrc`/`.kshrc` based on your shell with below environment variables

         export HADOOP_PREFIX=/opt/hadoop/hadoop
         export HADOOP_CONF_DIR=$HADOOP_PREFIX/etc/hadoop
         export JAVA_HOME=/java/home/path
         export PATH=$PATH:$HADOOP_PREFIX/bin:$HADOOP_PREFIX/sbin:$JAVA_HOME/bin

 - In `$HADOOP_HOME/etc/hadoop` directory edit below files 
      - core-site.xml

            <configuration>
                <property>
                    <name>fs.defaultFS</name>
                    <value>hdfs://localhost:8020</value>
                </property>
            </configuration> 
        
    - mapred-site.xml
        
        Create `mapred-site.xml` from its template
        
        `cp mapred-site.xml.template mapred-site.xml`

            <configuration>
                <property>
                    <name>mapreduce.framework.name</name>
                    <value>yarn</value>
                </property>
            </configuration> 
       
     - yarn-site.xml
   
            <configuration>
                <property>
                    <name>yarn.resourcemanager.hostname</name>
                    <value>localhost</value> 
                </property>
                <property>
                    <name>yarn.nodemanager.aux-services</name>
                    <value>mapreduce_shuffle</value>
                </property>
            </configuration>
     - hdfs-site.xml
   
            <configuration>
                <property>
                    <name>dfs.replication</name>
                    <value>1</value>
                </property>
                <property>
                    <name>dfs.namenode.name.dir</name>
                    <value>file:///home/hadoop/hdfs/namenode</value>
                </property>
                <property>
                    <name>dfs.datanode.data.dir</name> 
                    <value>file:///home/hadoop/hdfs/datanode</value> 
                </property>
            </configuration>

    Create the parent folder to store the hadoop data

       mkdir -p /home/hadoop/hdfs

 - Format NameNode (cleans up the directory and creates necessary meta files)

       hdfs namenode -format
    
 - Start all services: 
    
       start-dfs.sh && start-yarn.sh
       mr-jobhistory-server.sh start historyserver
          
       
*Instead use start-all.sh (deprecated).*
               
    
- Check all running java processes 
    
      jps 

 - Namenode Web Interface: 
          http://localhost:50070/

 - Resource manager Web Interface:
          http://localhost:8088/
    
 - To stop daemons(services):

       stop-dfs.sh && stop-yarn.sh
       mr-jobhistory-daemon.sh stop historyserver
*Instead use stop-all.sh (deprecated).*
    
      













## Hadoop overview and HDFS
[![enter image description here][1]][1]

<ul>
    <li style="text-align: left;">Hadoop is an open-source software framework for storage and large-scale processing of data-sets in a distributed computing environment.</li>
    <li style="text-align: left;">It is sponsored by Apache Software Foundation.</li>
    <li style="text-align: left;">It is designed to scale up from single servers to thousands of machines, each offering local computation and storage.</li>
</ul>
<p style="text-align: center;"><strong>History</strong></p>

<ul>
    <li>Hadoop was created by Doug Cutting and Mike Cafarella in 2005.</li>
    <li>Cutting, who was working at Yahoo! at the time, named it after his son's toy elephant.</li>
    <li> It was originally developed to support distribution for the search engine project.</li>
</ul>
<p style="text-align: center;"><strong>Major modules of hadoop</strong></p>

<ul>
    <li style="text-align: left;">Hadoop Distributed File System (HDFS): A distributed file system that provides high-throughput access to application data.</li>
    <li style="text-align: left;">Hadoop MapReduce: A software framework for distributed processing of large data sets on compute clusters.</li>
</ul>
<p style="text-align: center;"><strong>Hadoop File System</strong>
<strong> Basic Features</strong></p>

<ul>
    <li style="text-align: left;">Highly fault-tolerant.</li>
    <li style="text-align: left;">High throughput.</li>
    <li style="text-align: left;">Suitable for applications with large data sets.</li>
    <li style="text-align: left;">Can be built out of commodity hardware.</li>
</ul>
<p style="text-align: center;"><strong>Namenode and Datanodes</strong></p>

<ul>
    <li style="text-align: left;">Master/slave architecture.</li>
    <li style="text-align: left;">HDFS cluster consists of a single Namenode, a master server that manages the file system namespace and regulates access to files by clients.</li>
    <li style="text-align: left;">The DataNodes manage storage attached to the nodes that they run on.</li>
    <li style="text-align: left;">HDFS exposes a file system namespace and allows user data to be stored in files.</li>
    <li style="text-align: left;">A file is split into one or more blocks and set of blocks are stored in DataNodes.</li>
    <li style="text-align: left;">DataNodes: serves read, write requests, performs block creation, deletion, and replication upon instruction from Namenode.</li>
</ul>

[![enter image description here][2]][2]

<ul>
    <li style="text-align: left;">HDFS is designed to store very large files across machines in a large cluster.</li>
    <li style="text-align: left;">Each file is a sequence of blocks.</li>
    <li style="text-align: left;">All blocks in the file except the last are of the same size.</li>
    <li style="text-align: left;">Blocks are replicated for fault tolerance.</li>
    <li style="text-align: left;">The Namenode receives a Heartbeat and a BlockReport from each DataNode in the cluster.</li>
    <li style="text-align: left;">BlockReport contains all the blocks on a Datanode.</li>
</ul>
<p style="text-align: center;"><strong>Hadoop Shell Commands</strong></p>

<ul>
    <li style="text-align: left;">Common commands used:-
<ol>
    <li style="text-align: left;"><strong>ls</strong> Usage: <strong>hadoop fs –ls Path</strong>(dir/file path to list).</li>
    <li style="text-align: left;"><strong>Cat</strong> Usage: <strong>hadoop fs -cat PathOfFileToView</strong></li>
</ol>
</li>
</ul>

[![enter image description here][3]][3]

Link for hadoop shell commands:- <a title="Link for hadoop shell commands:- https://hadoop.apache.org/docs/r0.18.3/hdfs_shell.html" href="https://hadoop.apache.org/docs/r0.18.3/hdfs_shell.html">https://hadoop.apache.org/docs/r2.4.1/hadoop-project-dist/hadoop-common/FileSystemShell.html</a>


  [1]: https://i.stack.imgur.com/gEskB.jpg
  [2]: https://i.stack.imgur.com/kt1kT.gif
  [3]: https://i.stack.imgur.com/qMrYh.jpg

