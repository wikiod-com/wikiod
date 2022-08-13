---
title: "Getting started with jdbc"
slug: "getting-started-with-jdbc"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a connection
To be able to use JDBC you need to have the JDBC driver of your database on the class path of your application. 

There are multiple ways to connect to a database, but the common ways are to either use the [`java.sql.DriverManager`][1], or to configure and use a database specific implementation of [`javax.sql.DataSource`][2].

A simple example to create a connection to a database with the url `jdbc:somedb://localhost/foobar` and execute an update statement to give all employees a 5% raise:

<!-- language: java -->

    try (Connection connection = DriverManager.getConnection(
            "jdbc:somedb://localhost/foobar", "anna", "supersecretpassword");
         Statement updateStatement = connection.createStatement()) {
        
        updateStatement.executeUpdate("update employees set salary = salary * 1.05");
    }

For further details see [creating a database connection][3]


  [1]: https://docs.oracle.com/javase/8/docs/api/java/sql/DriverManager.html
  [2]: https://docs.oracle.com/javase/8/docs/api/javax/sql/DataSource.html
  [3]: https://www.wikiod.com/jdbc/creating-a-database-connection

