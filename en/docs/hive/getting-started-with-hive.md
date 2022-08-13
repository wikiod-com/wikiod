---
title: "Getting started with hive"
slug: "getting-started-with-hive"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of Hive(linux)
Start by downloading the latest stable release from https://hive.apache.org/downloads.html

-> Now untar the file with 

**$ tar -xvf hive-2.x.y-bin.tar.gz**

-> Create a directory in the /usr/local/ with

**$ sudo mkdir /usr/local/hive**

-> Move the file to root with

**$ mv ~/Downloads/hive-2.x.y /usr/local/hive**

-> Edit environment variablesfor hadoop and hive in .bashrc

**$ gedit ~/.bashrc**

like this

export HIVE_HOME=/usr/local/hive/apache-hive-2.0.1-bin/

export PATH=$PATH:$HIVE_HOME/bin

export CLASSPATH=$CLASSPATH:/usr/local/Hadoop/lib/*:.

export CLASSPATH=$CLASSPATH:/usr/local/hive/apache-hive-2.0.1-bin/lib/*:.


-> Now, start hadoop if it is not already running. And make sure that it is running and it is not in safe mode.

**$ hadoop fs -mkdir /user/hive/warehouse**

The directory "warehouse" is the location to store the table or data related to hive.

**$ hadoop fs -mkdir /tmp**

The temporary directory “tmp” is the temporary location to store the intermediate result of processing.

-> Set Permissions for read/write on those folders.

**$ hadoop fs -chmod g+w /user/hive/warehouse**

**$ hadoop fs -chmod g+w /user/tmp**

-> Now fire up HIVE with this command in console 

**$ hive**




## Word Count Example in Hive

**Docs file (Input File)**

Mary had a little lamb

its fleece was white as snow

and everywhere that Mary went

the lamb was sure to go.

**Hive Query**

    CREATE TABLE FILES (line STRING);

    LOAD DATA INPATH 'docs' OVERWRITE INTO TABLE FILES;

    CREATE TABLE word_counts AS
    SELECT word, count(1) AS count FROM
    (SELECT explode(split(line, ' ')) AS word FROM FILES) w
    GROUP BY word
    ORDER BY word;

**Output of word_counts table in Hive**

Mary,2

had,1

a,1

little,1

lamb,2

its,1

fleece,1

was,2

white,1

as,1

snow,1

and,1

everywhere,1

that,1

went,1

the,1

sure,1

to,1

go,1

## Hive Installation with External Metastore in Linux
**Pre-requisites:**

 1. Java 7
 2. Hadoop (Refer [here][1] for Hadoop Installation)
 3. Mysql Server and Client

**Installation:**

Step 1: Download the latest Hive tarball from the [downloads][2] page.

Step 2: Extract the downloaded tarball (***Assumption:** The tarball is downloaded in $HOME*)

    tar -xvf /home/username/apache-hive-x.y.z-bin.tar.gz
Step 3: Update the environment file (`~/.bashrc`)

    export HIVE_HOME=/home/username/apache-hive-x.y.z-bin
    export PATH=$HIVE_HOME/bin:$PATH
source the file to set the new environment variables.

    source ~/.bashrc

Step 4: Download the JDBC connector for mysql from [here][3] and extract it.

    tar -xvf mysql-connector-java-a.b.c.tar.gz
The extracted directory contains the connector jar file `mysql-connector-java-a.b.c.jar`. Copy it to the `lib` of `$HIVE_HOME`

    cp mysql-connector-java-a.b.c.jar $HIVE_HOME/lib/
**Configuration:**

Create the hive configuration file `hive-site.xml` under `$HIVE_HOME/conf/` directory and add the following metastore related properties.

    <configuration>
       <property>
          <name>javax.jdo.option.ConnectionURL</name>
          <value>jdbc:mysql://localhost/hive_meta</value>
          <description>JDBC connect string for a JDBC metastore</description>
       </property> 
       
       <property>
          <name>javax.jdo.option.ConnectionDriverName</name>    
          <value>com.mysql.jdbc.Driver</value>
          <description>Driver class name for a JDBC metastore</description>
       </property> 
       
       <property>
           <name>javax.jdo.option.ConnectionUserName</name> 
           <value>mysqluser</value>
           <description>username to use against metastore database</description>
       </property> 
       
       <property>
           <name>javax.jdo.option.ConnectionPassword</name> 
           <value>mysqlpass</value>
           <description>password to use against metastore database</description>
       </property> 

       <property>
           <name>datanucleus.autoCreateSchema</name>
           <value>false</value> 
       </property> 

       <property>
           <name>datanucleus.fixedDatastore</name>
           <value>true</value>
       </property>
    </configuration>

Update the values of MySQL "username" and "password" accordingly in the properties.

**Create the Metastore Schema:**

The metastore schema scripts are available under `$HIVE_HOME/scripts/metastore/upgrade/mysql/`

Login to MySQL and source the schema,

    mysql -u username -ppassword

    mysql> create database hive_meta;
    mysql> use hive_meta;
    mysql> source hive-schema-x.y.z.mysql.sql;
    mysql> exit;

    
**Starting Metastore:**

    hive --service metastore

To run it in background, 

    nohup hive --service metastore &

**Starting HiveServer2:** (Use if required)
 

    hiveserver2
To run it in background, 

    nohup hiveserver2 metastore &

**Note:** These executables are available under `$HIVE_HOME/bin/`

**Connect:**

Use either `hive`, `beeline` or [Hue][4] to connect with Hive.

Hive CLI is deprecated, using Beeline or Hue is recommended.


**Additional Configurations for Hue:**

Update this value in `$HUE_HOME/desktop/conf/hue.ini`

    [beeswax] 
        hive_conf_dir=/home/username/apache-hive-x.y.z-bin/conf

  [1]: https://www.wikiod.com/hadoop/getting-started-with-hadoop#Installation or Setup on Linux
  [2]: https://hive.apache.org/downloads.html
  [3]: https://dev.mysql.com/downloads/connector/j/
  [4]: https://www.wikiod.com/hadoop/hue#Hue Installation in Ubuntu

