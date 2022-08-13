---
title: "Connecting to MySQL Database for JBoss AS 7"
slug: "connecting-to-mysql-database-for-jboss-as-7"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Setting Up MySQL for JBoss AS 7
The following are the steps that you will need to follow to setup mysql datasource in JBoss AS 7.

1. Download the MySQL jdbc zip file from [here][1]: 
2. Create a directory **mysql** inside `/jboss-as-7.1.1.Final/modules/com`
3. Inside that create a directory structure like the following:
 

    /jboss-as-7.1.1.Final/modules/com/mysql/main
4. Inside the main copy and paste the following jar from the mysql connector that was downloaded in the first step.
   

    mysql-connector-java-5.1.XX-bin.jar

5. Create a module.xml in the same folder and add the following inside that:

    
    <?xml version="1.0" encoding="UTF-8"?>
    <module xmlns="urn:jboss:module:1.1" name="com.mysql"> 
        <resources> 
           <resource-root path="mysql-connector-java-5.1.30-bin.jar"/> 
           <!-- Insert resources here --> 
        </resources> 
        <dependencies> 
           <module name="javax.api"/> 
        </dependencies>
    </module>

6. Inside the `standalone.xml` in the bin directory add the following:


    <driver name="mysql" module="com.mysql">
        <driver-class>com.mysql.jdbc.Driver</driver-class>
    </driver>

7. Now startup your jboss and open the admin console. 
8. Inside the admin console create/add a new datasource and enter the proper details. You can put any name you want. 
Remember to add the jndi name in the details as:
    

     java:/mysql

9. Then enable the connection.
10. Now you can check for the connection and you should be all set, if you have followed the above steps properly.

You can watch this [video][2], which demonstrates the above steps.


  [1]: http://dev.mysql.com/downloads/connector/j/
  [2]: https://www.youtube.com/watch?v=ZJQA4G5T5HA

