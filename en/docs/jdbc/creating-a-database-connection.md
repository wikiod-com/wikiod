---
title: "Creating a database connection"
slug: "creating-a-database-connection"
draft: false
images: []
weight: 9887
type: docs
toc: true
---

## Syntax
 - DB_URL = "jdbc:DBMS://DB_HOST:DB_PORT/DB_NAME"

 - DBMS: Data Base Driver Manager, this can be any DBMS (mysql, oracle, postgresql, sqlite, ...), exemple of mysql: "com.mysql.jdbc.Driver"

 - DB_HOST: your database base host, the IP adress of your database exemple : 10.6.0.1, the default is localhost or 127.0.0.1

 - DB_PORT: Database port, every DBMS has a defeaut port exemple mysql=3306, postegesql=5432

 - DB_NAME: the name of your Database

 - To connect you should to obtains a reference to the class object,
 - Class.forName(DRIVER);

 - And to connect to database, you need to create a connection

 - java.sql.Connection con = DriverManager.getConnection(DB_URL,
   DB_USER_NAME, DB_PASSWORD);

 - DB_USER_NAME : the username of your databse
 - DB_PASSWORD : the password of your database


## Creating a connection to MySQL
<!-- language-all: java -->
To connect to MySQL you need to use the MySQL Connector/J driver. You can download it from http://dev.mysql.com/downloads/connector/j/ or you can use Maven:

<!-- language: xml -->

    <dependency>
        <groupId>mysql</groupId>
        <artifactId>mysql-connector-java</artifactId>
        <version>5.1.39</version>
    </dependency>

The [basic JDBC URL for MySQL][1] is:

<!-- language: none -->

    jdbc:mysql://<hostname>[:<port>]/<database>[?<propertyName>=<propertyValue>[&<propertyName>=<propertyValue>]...]

Where:

| Key | Description | Example |
| --- | ----------- | ------- |
| `<hostname>` | Host name of the MySQL server | `localhost` |
| `<port>` | Port of the MySQL server (optional, default: 3306) | `3306` |
| `<database>` | Name of the database | `foobar` |
| `<propertyName>` | Name of a [connection property][1] | `useCompression` |
| `<propertyValue>` | Value of a connection property | `true` |

The supported URL is more complex than shown above, but this suffices for most 'normal' needs.

To connect use:

    try (Connection connection = DriverManager.getConnection(
            "jdbc:mysql://localhost/foobardb", "peter", "nicepassword")) {
        // do something with connection
    }

<!-- if version [lte 4.0] -->
For older Java/JDBC versions:
<!-- if version [lt 4.0] -->

    // Load the MySQL Connector/J driver
    Class.forName("com.mysql.jdbc.Driver");

<!-- end version if -->

    Connection connection = DriverManager.getConnection(
            "jdbc:mysql://localhost/foobardb", "peter", "nicepassword");
    try {
        // do something with connection
    } finally {
      // explicitly close connection
      connection.close();
    }

<!-- end version if -->

  [1]: http://dev.mysql.com/doc/connector-j/5.1/en/connector-j-reference-configuration-properties.html

## Connection to a Microsoft Access database with UCanAccess
UCanAccess is a pure Java `JDBC` driver that allows us to read from and write to Access databases without using `ODBC`. It uses two other packages, `Jackcess` and `HSQLDB`, to perform these tasks.

Once it has been set up<sup>*</sup>, we can work with data in .accdb and .mdb files using code like this:

    import java.sql.*;
    
    Connection conn=DriverManager.getConnection("jdbc:ucanaccess://C:/__tmp/test/zzz.accdb");
    Statement s = conn.createStatement();
    ResultSet rs = s.executeQuery("SELECT [LastName] FROM [Clients]");
    while (rs.next()) {
        System.out.println(rs.getString(1));
    }

<sup>*</sup>For more details see the following question:

http://stackoverflow.com/q/21955256/2144390

## Introduction (SQL)


## Using the Connection (And Statements)


## Creating a connection using java.sql.DriverManager
To connect using [`java.sql.DriverManager`][1] you need a JDBC url to connect to your database. JDBC urls are database specific, but they are all of the form

    jdbc:<subprotocol>:<subname>

Where `<subprotocol>` identifies the driver or database (for example `postgresql`, `mysql`, `firebirdsql`, etc), and `<subname>` is subprotocol-specific.

You need to check the documentation of your database and JDBC driver for the specific url subprotocol and format for your driver.

A simple example to create a connection to a database with the url `jdbc:somedb://localhost/foobar`:

<!-- language: java -->

    try (Connection connection = DriverManager.getConnection(
            "jdbc:somedb://localhost/foobar", "anna", "supersecretpassword")) {
        // do something with connection
    }

We use a [try-with-resources][3] here so the connection is automatically closed when we are done with it, even if exceptions occur.

<!-- if version [lte 4.0] -->

On Java 6 (JDBC 4.0) and earlier, try-with-resources is not available. In those versions you need to use a `finally`-block to explicitly close a connection:


<!-- language: java -->

    Connection connection = DriverManager.getConnection(
            "jdbc:somedb://localhost/foobar", "anna", "supersecretpassword");
    try {
        // do something with connection
    } finally {
        // explicitly close connection
        connection.close();
    }

<!-- end version if -->

<!-- if version [lt 4.0] -->

JDBC 4.0 (Java 6) introduced the concept of automatic driver loading. If you use Java 5 or earlier, or an older JDBC driver that does not implement JDBC 4 support, you will need to explicitly load the driver(s):

<!-- language: java -->

    Class.forName("org.example.somedb.jdbc.Driver");

This line needs to occur (at least) once in your program, before any connection is made.

Even in Java 6 and higher with a JDBC 4.0 it may be necessary to explicitly load a driver: for example in web applications when the driver is not loaded in the container, but as part of the web application.

<!-- end version if -->

Alternatively you can also provide a `Properties` object to connect:

<!-- language: java -->

    Properties props = new Properties();
    props.setProperty("user", "anna");
    props.setProperty("password", "supersecretpassword");
    // other, database specific, properties
    try (Connection connection = DriverManager.getConnection(
            "jdbc:somedb://localhost/foobar", props)) {
        // do something with connection
    }

Or even without properties, for example if the database doesn't need username and password:

<!-- language: java -->

    try (Connection connection = DriverManager.getConnection(
            "jdbc:somedb://localhost/foobar")) {
        // do something with connection
    }


  [1]: https://docs.oracle.com/javase/8/docs/api/java/sql/DriverManager.html
  [3]: https://www.wikiod.com/java/exceptions-and-exception-handling#The try-with-resources statement

## Oracle JDBC connection
## Driver:
 * [12c R1](http://www.oracle.com/technetwork/database/features/jdbc/jdbc-drivers-12c-download-1958347.html)
 * [11g R2](http://www.oracle.com/technetwork/apps-tech/jdbc-112010-090769.html)

(**Note:** the driver is not included in Maven Central!)

## Driver class initialization:

        Class.forName("oracle.jdbc.driver.OracleDriver");

## Connection URL

**Older format, with SID**


    "jdbc:oracle:thin:@<hostname>:<port>:<SID>"

**Newer format, with Service Name**


    "jdbc:oracle:thin:@//<hostname>:<port>/<servicename>"

**Tnsnames like entry**

    "jdbc:oracle:thin:@(DESCRIPTION=(ADDRESS=(PROTOCOL=TCPS)(HOST=<hostname>)(PORT=<port>))"
        +"(CONNECT_DATA=(SERVICE_NAME=<servicename>)))"

**RAC cluster connection string for failover**


    "jdbc:oracle:thin:@(DESCRIPTION=(ADDRESS_LIST=(LOAD_BALANCE=OFF)(FAILOVER=ON)"
        +"(ADDRESS=(PROTOCOL=TCP)(HOST=<hostname1>)(PORT=<port1>))"
        +"(ADDRESS=(PROTOCOL=TCP)(HOST=<hostname2>)(PORT=<port2>)))"
        +"(CONNECT_DATA=SERVICE_NAME=<servicename>)(SERVER=DEDICATED)))"


## Example

    connection = DriverManager.getConnection("jdbc:oracle:thin:@localhost:1521:orcl", "HR", "HRPASS");



