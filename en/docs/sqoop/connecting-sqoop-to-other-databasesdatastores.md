---
title: "Connecting Sqoop to other databasesdatastores"
slug: "connecting-sqoop-to-other-databasesdatastores"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Shows how a sqoop script could be used to import data from various datastores/databases.

## Load JDBC Driver
For accessing the MS SQL Server database Sqoop requires an additional JDBC driver which can be downloaded from Microsoft. The following steps will install MSSQL Server JDBC driver to Sqoop:

    wget 'http://download.microsoft.com/download/0/2/A/02AAE597-3865-456C-AE7F-613F99F850A8/sqljdbc_4.0.2206.100_enu.tar.gz'

    tar -xvzf sqljdbc_4

    cp sqljdbc_4.0/enu/sqljdbc4.jar /usr/hdp/current/sqoop-server/lib/

## Validate the connection
To check that the connection to the server is valid:

    sqoop list-tables --connect "jdbc:sqlserver://<server_ip>:1433;database=<database_name>" 
                      --username <user_name>
                      --password <password>

Before doing this it is recommended to check if SqlServer's configuration allows remote access to 1433 port.

 Open **Configuration Manager** => **SQL Server Network configuration** => **Protocols for MSSQLSERVER** and check that Protocol is enabled and the needed IP and port is enabled and is active.

## Import table to new catalog
To import data from SQL Server to Hadoop:
   
    sqoop import --table TestTable
                 --connect "jdbc:sqlserver://192.168.1.100:1433;database=Test_db"  
                 --username user 
                 --password password  
                 --split-by id  
                 --target-dir /user/test

* split-by – used mandatory if no primary key
* target-dir – new catalog, which does not exist yet


## Import the results of a query from a relational database into HDFS:
Query can be used instead of table in import operation:

    sqoop import --query 'select Id,Message from TestTable where $CONDITIONS' 
                 --where 'id>100' 
                 --connect "jdbc:sqlserver://192.168.1.100:1433;database=Test_db
                 --username user 
                 -–password password 
                 --split-by id  
                 --target-dir /user/test/ 
                 --fields-terminated-by '\t'

* where $CONDITIONS - mandatory even if where condition does not exist
* split-by - mandatory, specifies the column for the split operation. Used to split tasks in import MapReduce job

## Import data directly into Hive Warehouse
The data can be imported directly into Hive:

    sqoop import --hive-import 
                 --table EventLog 
                 --connect "jdbc:sqlserver://192.168.1.99:1433;database=Test_db" 
                 --username user 
                 --password password 
                 --split-by id 
                 --fields-terminated-by '\t'

## Import data from RDBMS to HBase table
The following sqoop command will be used to import the data from RDBMS table into HBase table, if the table does not exists in HBase it will create a new table and import the data into this table

    sqoop import \
            --query 'select emp_id, emp_name, emp_sal from employee where $CONDITIONS' \
            --connect "jdbc:sqlserver://192.168.1.99:1433;database=test_db" \
            --username username \
            -–password password \
            --hbase-create-table \
            --hbase-table employee_table \
            --hbase-row-key emp_id

