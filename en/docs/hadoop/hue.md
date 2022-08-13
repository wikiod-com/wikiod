---
title: "hue"
slug: "hue"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Hue is an User Interface to connect and work with most of the commonly used Bigdata technologies like HDFS, Hive, Spark, Hbase, Sqoop, Impala, Pig, Oozie etc. Hue also supports running queries against Relational databases.

Hue, a django web application, was primarily built as a workbench for running Hive queries. Later the functionality of Hue increased to support different components of Hadoop Ecosystem. It is available as open source software under Apache License.

## Hue Installation in Ubuntu
This installation assumes `hadoop` to be pre-installed under `hadoop` user.

**Prerequisites:**
      
Hue depends on these following packages

 1. gcc
 2. g++
 3. libxml2-dev
 4. libxlst-dev
 5. libsasl2-dev
 6. libsasl2-modules-gssapi-mit 
 7. libmysqlclient-dev 
 8. python-dev 
 9. python-setuptools 
 10. libsqlite3-dev 
 11. ant 
 12. libkrb5-dev 
 13. libtidy-0.99-0
 14. libldap2-dev
 15. libssl-dev
 16. libgmp3-dev

Installing all the packages 

    sudo apt-get update
    sudo apt-get install gcc g++ libxml2-dev libxslt-dev libsasl2-dev libsasl2-modules-gssapi-mit libmysqlclient-dev python-dev python-setuptools libsqlite3-dev ant libkrb5-dev libtidy-0.99-0 libldap2-dev libssl-dev libgmp3-dev

**Installation and Configuration**

Performing installation as `hadoop` user.

    su - hadoop

1. Download Hue from gethue.com (this link is an example obtained from Hue website)

    `wget https://dl.dropboxusercontent.com/u/730827/hue/releases/3.9.0/hue-3.9.0.tgz` 

2. Extract the downloaded tarball

    `tar -xvf hue-3.9.0.tgz`

3. Execute install command

        cd hue-3.9.0
        PREFIX=/home/hadoop/ make install

4. Once the above process is completed, 
    
    Update `~/.bashrc` file,
    
        export HUE_HOME=/home/hadoop/hue
        export PATH=$PATH:$HUE_HOME/build/env/bin

    source after adding the entries,
        source ~/.bashrc

5. Configure Hue ( 3 files to edit)

  `cd $HUE_HOME/desktop/conf`
   
- hue.ini

      [desktop]
       server_user=hadoop
       server_group=hadoop
       default_user=hadoop
       default_hdfs_superuser=hadoop
    
`cd $HADOOP_CONF_DIR`
  - core-site.xml

        <property>
            <name>hadoop.proxyuser.hadoop.hosts</name>
            <value>*</value> 
        </property> 
        <property>
            <name>hadoop.proxyuser.hadoop.groups</name>
            <value>*</value> 
        </property>

  - hdfs-site.xml

        <property>
            <name>dfs.webhdfs.enabled</name>
            <value>true</value> 
        </property>

6. Start Hue (Start Hadoop daemons if not already started)
    
    `nohup supervisor &`
7. Login to Hue Web Interface: http://localhost:8888

    *username:* hadoop

    *password*: user_choice








## Setup process
Instalation Dependencies
------------------------

[Hue installation process details][1] are not available for most operating systems, so depending on the OS, there might be variations on the dependencies you need to install prior to executing the install script provided in the [installation package][2]:

CentOS

    sudo yum install ant
    sudo yum install python-devel.x86_64
    sudo yum install krb5-devel.x86_64
    sudo yum install krb5-libs.x86_64
    sudo yum install libxml2.x86_64
    sudo yum install python-lxml.x86_64
    sudo yum install libxslt-devel.x86_64
    sudo yum install mysql-devel.x86_64
    sudo yum install openssl-devel.x86_64
    sudo yum install libgsasl-devel.x86_64
    sudo yum install sqlite-devel.x86_64
    sudo yum install openldap-devel.x86_64
    sudo yum install -y libffi libffi-devel
    sudo yum install mysql-devel gcc gcc-devel python-devel
    sudo yum install rsync
    sudo yum install maven
    wget https://bootstrap.pypa.io/ez_setup.py -O - | sudo python

 1. GMP
 - > CentOS > 7.x   
`sudo yum install  libgmp3-dev`
 - > CentOS < 6.x   
`sudo yum install gmp gmp-devel gmp-status`

  [1]: http://gethue.com/
  [2]: http://gethue.com/category/release/

