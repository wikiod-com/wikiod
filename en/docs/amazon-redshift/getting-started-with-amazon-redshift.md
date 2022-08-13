---
title: "Getting started with amazon-redshift"
slug: "getting-started-with-amazon-redshift"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup


As shown in the step by step guide ([getting started with Amazon Redshift][1]),it involves :
     

 - Step 1: Set Up Prerequisites

 -   Step 2: Launch a Sample Amazon Redshift Cluster

  -  Step 3: Authorize Access to the Cluster

   - Step 4: Connect to the Sample Cluster


## Setting Up Prerequisites ##
> Setting up Prerequisites involves Signing Up for AWS account and installing SQL Client Drivers and Tools like SQL Workbench/J 

> To install SQL Workbench J and configure it:  
> - Use this link to download generic package for all systems ([sql-workbench][2]) . Assuming you have all its prerequisites installed .
> - Get the appropriate [JDBC][3]/[ODBC][4] Driver.
> - Using the script in the installation directory launch SQL-Workbench . 
> - Open Connection Window > Manage Drivers > Browse and select the driver file.
> - In the Select Connection Profile Window. Select the created driver in the Driver box. Paste the URL from the redshift cluster created (JDBC URL from the Amazon Redshift console) and the master user Username , Password.

## Launch a Sample Amazon Redshift Cluster ##
> - Click on Launch Cluster from Amazon Redshift Dashboard
> - On cluster details page choose any Cluster Identifier, Database Name, Database Port (Choose an open port in your firewall since you cannot change the port number once the cluster is created.), Master User Name, Master User Password.
> On the Node Configuration page , select the Version (Redshift Version) , Node Type , Cluster Type and Number of Compute Nodes. 
> - Based on the EC2-VPC or EC2 Classic platform you select, the security steps vary for authorizing your cluster. For the rest of the pages you can use the default settings for now. 

## Authorize Cluster ##
> For EC2-VPC Platform, click on the created cluster name after opening Redshift cluster tab from navigation pane and go to the configuration tab. In cluster properties choose the security group. Edit the inbound and outbound rules(Protocol, Port Range ,Source) as per your requirements from the Inbound and Outbound Tab.

> For EC2-Classic Platform ,click on the created cluster name after opening Redshift cluster tab from navigation pane and go to the configuration tab. 
Choose default  under Cluster Properties , for Cluster Security Groups. Then Choose the cluster security group from the Security Groups tab, in the cluster security group list. Select CIDR/IP  from the connection type in the security group connections tab and authorize it with an IP/Port.

## Connect to the Sample Cluster ##
> Follow the last step in setting up prerequisites.

  [1]: http://docs.aws.amazon.com/redshift/latest/gsg/getting-started.html
  [2]: http://www.sql-workbench.net/downloads.html
  [3]: http://docs.aws.amazon.com/redshift/latest/mgmt/configure-jdbc-connection.html#download-jdbc-driver
  [4]: http://docs.aws.amazon.com/redshift/latest/mgmt/install-odbc-driver-linux.html
  [5]: http://i.stack.imgur.com/sAOgv.png
  [6]: http://docs.aws.amazon.com/redshift/latest/gsg/rs-gsg-create-an-iam-role.html

## Connect Amazon redshift database and fetch data into Array using Node.js
Best way to connect amazon redshift using JDBC , Use proper driver as per version 
http://docs.aws.amazon.com/redshift/latest/mgmt/configure-jdbc-connection.html

Step-1: npm install jdbc

Step-2: 

    var JDBC = require('jdbc');
    var jinst = require('jdbc/lib/jinst');
    // isJvmCreated will be true after the first java call.  When this happens, the
    // options and classpath cannot be adjusted.
    if (!jinst.isJvmCreated()) {
      // Add all java options required by your project here.  You get one chance to
      // setup the options before the first java call.
      jinst.addOption("-Xrs");
      // Add all jar files required by your project here.  You get one chance to
      // setup the classpath before the first java call.
      jinst.setupClasspath(['./drivers/hsqldb.jar',
                            './drivers/derby.jar',
                            './drivers/derbyclient.jar',
                            './drivers/derbytools.jar',
                            './lib/drivers/RedshiftJDBC41-1.1.10.1010.jar'
                            ]);
    }
    
    var config = {
      url: 'jdbc:redshift://test-redshift.czac2vcs84ci.us-east-.redshift.amazonaws.com:5439/testredshift?user=redshift&password=W9P3GC42GJYFpGxBitxPszAc8iZFW',
      drivername: 'com.amazon.redshift.jdbc41.Driver',
      user : 'username',
      password: 'password',
      minpoolsize: 10,
      maxpoolsize: 100
    };
    var hsqldbInit = false;
    GLOBAL.hsqldb = new JDBC(config);`
Step-3: npm install async (Use async module to query your code) (Optional)

Step-4: Manually create one database name **test** and table **sample_data** , find amazon redshift database command [here][2]

Step-5:

    var asyncjs = require('async');
    hsqldb.reserve(function(err, connObj) {
        if (connObj) {
            console.log("Connection: " + connObj.uuid);
            var conn = connObj.conn;
            asyncjs.series([
                function(callback) {
                    conn.createStatement(function(err, statement) {
                        if (err) {
                            callback(err);
                        } else {
                            statement.setFetchSize(100, function(err) {
                                if (err) {
                                    callback(err);
                                } else {
                                  statement.executeQuery("SELECT * FROM test.sample_data", function(err, resultset) {
                                    resultset.toObjArray(function(err,sresults){
                                      console.log(sresults);
                                    });   
                                  });   
                               }
                            })
                          }
                        })
                     }
                  ]) 
             }
          })


  [1]: http://docs.aws.amazon.com/redshift/latest/mgmt/configure-jdbc-connection.html
  [2]: http://docs.aws.amazon.com/redshift/latest/dg/c_SQL_commands.html

