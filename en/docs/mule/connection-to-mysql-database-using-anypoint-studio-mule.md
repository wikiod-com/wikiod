---
title: "Connection to  MySQL Database Using Anypoint Studio (Mule)"
slug: "connection-to--mysql-database-using-anypoint-studio-mule"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Am new to Mule and wanted to share how to connect to Database and retrieve Values.

## Example to retrieve Rows from Table in MySQL
Required External Jar: mysql-connector-java-5.1.40-bin.jar to connect to Data Base.
Add this jar by right clicking the project -->Build Path--> Add external Archieve.
**Create the Flow as Flowing**

[![enter image description here][1]][1]


**2) Database Connector Configuration:**
Select MySQL as your database by double clicking the Database connector and fill all the details as mentioned.
After that Click Test Connection which will give Connection Successfull...

In Query: Select * from test.contact
where test is my schema and Contact is my table name.

[![enter image description here][2]][2]


**3)Object to JSON**:
Drag Object to JSON connector and leave it as it is without any modifications.

**4)Set Payload**: Drag the Set Payload connector and set the value attribute to **#[payload]**

[![enter image description here][3]][3]
**Final Step**: Deploye the code and run as 
http://localhost:8089/test

8089 is my port number.. whatever yours u need to give that.

When you run you can see the following output in Json Format


  [1]: https://i.stack.imgur.com/VVFWi.png
  [2]: https://i.stack.imgur.com/cPcYZ.png
  [3]: https://i.stack.imgur.com/lTgkn.png

## Select Rows from MySQL Data Base in Anypoint Studio(Mule)
**Step1 Message Flow:**
[![enter image description here][1]][1]

**Step 2: Databse Connector Configuration**

For this you need **mysql-connector-java-5.1.40-bin.jar** .
Right click on Project -->build Path--> Add external archieve and add the jar(without jar it cannot be connected)
Enter all the values correctly mentioned below screenshot.
Then Click Test connection and should show connection successful

Query: select * from test.contact

where test =Schema name or database name and contact is table name.
[![enter image description here][2]][2]

**3) XML view**

Use Object to JSON connector and leave the fields as it is

Use setpayload connector and enter the VALUE attribute as **#[payload]**

[![enter image description here][3]][3]

**Final Step:**
Deploye the project , run as mule application.
After running.. run the url either in POSTMAN or Chrome.
http://localhost:8089/test

8089 is my local port. Whatever u mention run with that port.


[![enter image description here][4]][4]

Finally the Values are displayed in JSON format.
Try to run Different Queries.

Am new to Mule. Found informative and posting it.
  [1]: https://i.stack.imgur.com/jt4tS.png
  [2]: https://i.stack.imgur.com/oNQJe.png
  [3]: https://i.stack.imgur.com/EI7U4.png
  [4]: https://i.stack.imgur.com/taq6z.png

