---
title: "Connect Google Cloud SQL with Apps and Tools?"
slug: "connect-google-cloud-sql-with-apps-and-tools"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## How to connect Google Cloud SQL with Apps (like Google App Engine) and Tools like (mySQL workbench)?
In this document we'll see how to create a Google Cloud SQL Instance and connect them in your Google App Engine application and MySQL Workbench admin tool.

**Google Cloud SQL:**

Google Cloud SQL is a fully-managed database service that makes it easy to set-up, maintain, manage and administer your relational MySQL databases in the cloud. 

Google Cloud SQL provides a relational database that you can use with your App Engine application. Cloud SQL is a MySQL database that lives in Google's cloud.

refer:

> https://cloud.google.com/sql/ 

> https://cloud.google.com/sql/docs/


**Creating SQL Instances:**

 A Google Cloud SQL instance is a MySQL database hosted in Google's cloud.

    

 1. Go to the Cloud SQL Instances page in the Google Cloud Platform
    Console (https://console.cloud.google.com/sql/instances) and Click
    Create instance.
 2. Click Choose First Generation, Enter a name and Choose a tier for
        the instance and Click Create.
    
 3. After the instance finishes initializing, select the instance to
    open it.
    
 4. In Access Control > Users, Click Create user account and create a
    user with name root and specify a password (root_password). This
    creates the MySQL user 'root'@'%'.
    
 5. In Databases, Click New Database and create a database with a
    DataBase name (DataBase_Name)
    
**MySQL Workbench:**

MySQL Workbench is a unified visual tool for database architects, developers, and DBAs. MySQL Workbench provides data modeling, SQL development, and comprehensive administration tools for server configuration, user administration, backup, and much more. 

refer http://www.mysql.com/products/workbench/

Now we'll see how to connect to your Google Cloud SQL instance database with MySQL Workbench.


***Configuring access*** 
    

 1. Go to the Cloud SQL Instances page in the Google Cloud Platform
    Console and select the instance.
    
 2. In Access Control > IP address, Click Request IPv4 adddress and copy
    it(Instance_IPv4_address). It is needed to connect your Google Cloud
    SQL instance database with Admin tools like MySQL Workbench.

    *note:* You will be charged for IPv4 address @ $0.01 each hour the instance is inactive and $0.1 each hour the instance is active

    

 1. Google 'ip address' to find your public IP address
    
 2. In Access Control > Authorization > Authorized networks, click Add
    network and enter your IP address.
 3. In Access Control > Users, Create a user with username (userName),
    password (password) and the option 'Allow any host selected'. It is
    recommended to use a seperate user account to access from WorkBench

***Connecting***
    

 1. In the MySQL Workbench home view, click New Connection.

    

 1. In the Setup New Connection window, provide a Connection Name,
    Hostname and Username

    

 1. Click Test Connection. You will be prompted for a password.

    

 1. Once the MySQL connection is made successful, Click OK and click on
    the saved connection to open SQL Editor

**Google App Engine:**

Google App Engine is a platform for building scalable web applications and mobile backends.  App Engine will scale your application automatically.

refer https://cloud.google.com/appengine

Now we'll see how to set up a connection between an App Engine application and a Cloud SQL instance.

***Configuring access*** 
    

 1. Go to the Cloud SQL Instances page in the Google Cloud Platform
    Console and select the instance.

    

 1. In Access Control > Authorization > Authorized App Engine
    applications, click Add application ID and enter the application ID.
    Click Done and Save.

    

 1. In Overview > Properties Copy the 'Instance connection name'
    (Instance_Connection_Name)

    

 1. In your Google Web Application Project,
    war/WEB-INF/appengine-web.xml    add,
            <use-google-connector-j>true</use-google-connector-j>

***Code sample:***

An Exaple for Google App Engine - Java Standard Environment 

    public static Connection connect() throws ClassNotFoundException, SQLException {
        String url = null;

        {
            if (SystemProperty.environment.value() == SystemProperty.Environment.Value.Production) {

                // Connecting from App Engine.
                Class.forName(Messages.getString("com.mysql.jdbc.GoogleDriver")); 
                url = Messages.getString("jdbc:google:mysql://{{Instance_Connection_Name}}/{{DataBase_Name}}?user=root&password={{root_password}}");
            } else {
                // Connecting from an external network or localhost
                Class.forName(Messages.getString("com.mysql.jdbc.Driver"));
                url = Messages.getString("jdbc:mysql://{{Instance_IPv4_address}}:3306/{{DataBase_Name}}?user={{userName}}&password={{password}}");
            }

            Connection conn = DriverManager.getConnection(url);

            return conn;
        }
    }

