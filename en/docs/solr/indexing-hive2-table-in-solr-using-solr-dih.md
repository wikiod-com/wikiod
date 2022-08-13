---
title: "INDEXING HIVE2 TABLE IN SOLR USING SOLR DIH"
slug: "indexing-hive2-table-in-solr-using-solr-dih"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This documentation provides a way to connect to hive using SOLR Data Import Handler and index the data in SOLR. This is an interesting documentation because I couldn't find it over internet.

The handler basically handles more than 80 million records which means a strong infrastructure with good CPUs and Memory is definitely needed.

Please feel free to reach out to us and we will try to help as much as possible.

## Steps
We got the hive2 jars first and made it working on through java to check the connectivity. Then we realized the jars to be used are :

 1. hadoop-common-2.7.0-mapr-1703.jar 
 2. hive-common-2.1.1-mapr-1703-r1.jar
 3. hive-jdbc-2.1.1-mapr-1703-r1-standalone.jar

If you are using SOLR Cloud then these jars are to be transferred to the VM where SOLR is installed and then referenced in solrconfig.xml like this:

**Import Part in solrconfig.xml**

< lib dir="/users/path_to_folder_with_jar" regex=".*.jar" />

Then this is the most important part: Your hive connection string:

**Connection Part**

< dataConfig > < dataSource name="ABC" driver="org.apache.hive.jdbc.HiveDriver" url="jdbc:hive2://....connectionString" user="username" password="password" />

< document name="collection_name">

< entity name="collection_lookup" query="select unique_key as id from table_name">

< /entity>

< /document>

< /dataConfig>

**Push config through zookeeper**

server/scripts/cloud-scripts/zkcli.sh -zkhost host1:2181,host2:2181 -cmd upconfig -confname configName -confdir server/solr/configsets/folder/

Go to http://host:8983/solr/#/collection_name/dataimport//dataimport then check debug and first check with 10 or 20 records.

You will see the data flowing. CHEERS !! I can help if you want to discuss further but I am assuming this should do. It is working for me.

