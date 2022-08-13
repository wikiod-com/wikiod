---
title: "Connect to PostgreSQL from Java"
slug: "connect-to-postgresql-from-java"
draft: false
images: []
weight: 9921
type: docs
toc: true
---

The API to use a relational database from Java is JDBC.

This API is implemented by a JDBC driver.

To use it, put the JAR-file with the driver on the JAVA class path.

This documentation shows samples how to use the JDBC driver to connect to a database.

**JDBC URL**

The JDBC URL can take one of these forms:

- <code>jdbc:postgresql://*host*[:*port*]/[*database*][*parameters*]</code>

  <code>*host*</code> defaults to `localhost`, <code>*port*</code> to 5432.  
  If <code>*host*</code> is an IPv6 address, it must be enclosed in square brackets.  
  The default database name is the same as the name of the connecting user.

  To implement failover, it is possible to have several <code>*host*[:*port*]</code> entries separated by a comma.  
  They are tried in turn until a connection succeeds.

- <code>jdbc:postgresql:*database*[*parameters*]</code>

- <code>jdbc:postgresql:/[*parameters*]</code>

  These forms are for connections to `localhost`.

<code>*parameters*</code> is a list of <code>*key*[=*value*]</code> pairs, headed by `?` and separated by `&`. If the <code>*value*</code> is missing, it is assumed to be `true`.

An example:

    jdbc:postgresql://localhost/test?user=fred&password=secret&ssl&sslfactory=org.postgresql.ssl.NonValidatingFactory

**References**

- JDBC specification: http://download.oracle.com/otndocs/jcp/jdbc-4_2-mrel2-eval-spec/
- PostgreSQL JDBC driver: https://jdbc.postgresql.org/
- PostgreSQL JDBC driver documentation: https://jdbc.postgresql.org/documentation/head/index.html

## Connecting with java.sql.DriverManager
This is the simplest way to connect.

First, the driver has to be *registered* with `java.sql.DriverManager` so that it knows which class to use.  
This is done by loading the driver class, typically with <code>java.lang.Class.forname(*&lt;driver&nbsp;class&nbsp;name&gt;*)</code>.

<!-- language-all: lang-java -->

    /**
     * Connect to a PostgreSQL database.
     * @param url the JDBC URL to connect to; must start with "jdbc:postgresql:"
     * @param user the username for the connection
     * @param password the password for the connection
     * @return a connection object for the established connection
     * @throws ClassNotFoundException if the driver class cannot be found on the Java class path
     * @throws java.sql.SQLException if the connection to the database fails
     */
    private static java.sql.Connection connect(String url, String user, String password)
        throws ClassNotFoundException, java.sql.SQLException
    {
        /*
         * Register the PostgreSQL JDBC driver.
         * This may throw a ClassNotFoundException.
         */
        Class.forName("org.postgresql.Driver");
        /*
         * Tell the driver manager to connect to the database specified with the URL.
         * This may throw an SQLException.
         */
        return java.sql.DriverManager.getConnection(url, user, password);
    }

Not that user and password can also be included in the JDBC URL, in which case you don't have to specify them in the `getConnection` method call.

## Connecting with java.sql.DriverManager and Properties
Instead of specifying connection parameters like user and password (see a complete list [here][1]) in the URL or a separate parameters, you can pack them into a `java.util.Properties` object:

<!-- language-all: lang-java -->

    /**
     * Connect to a PostgreSQL database.
     * @param url the JDBC URL to connect to. Must start with "jdbc:postgresql:"
     * @param user the username for the connection
     * @param password the password for the connection
     * @return a connection object for the established connection
     * @throws ClassNotFoundException if the driver class cannot be found on the Java class path
     * @throws java.sql.SQLException if the connection to the database fails
     */
    private static java.sql.Connection connect(String url, String user, String password)
        throws ClassNotFoundException, java.sql.SQLException
    {
        /*
         * Register the PostgreSQL JDBC driver.
         * This may throw a ClassNotFoundException.
         */
        Class.forName("org.postgresql.Driver");
        java.util.Properties props = new java.util.Properties();
        props.setProperty("user", user);
        props.setProperty("password", password);
        /* don't use server prepared statements */
        props.setProperty("prepareThreshold", "0");
        /*
         * Tell the driver manager to connect to the database specified with the URL.
         * This may throw an SQLException.
         */
        return java.sql.DriverManager.getConnection(url, props);
    }

[1]: https://jdbc.postgresql.org/documentation/head/connect.html#connection-parameters

## Connecting with javax.sql.DataSource using a connection pool
It is common to use `javax.sql.DataSource` with JNDI in application server containers, where you register a data source under a name and look it up whenever you need a connection.

This is code that demonstrates how data sources work:

<!-- language: lang-java -->

    /**
     * Create a data source with connection pool for PostgreSQL connections
     * @param url the JDBC URL to connect to. Must start with "jdbc:postgresql:"
     * @param user the username for the connection
     * @param password the password for the connection
     * @return a data source with the correct properties set
     */
    private static javax.sql.DataSource createDataSource(String url, String user, String password)
    {
        /* use a data source with connection pooling */
        org.postgresql.ds.PGPoolingDataSource ds = new org.postgresql.ds.PGPoolingDataSource();
        ds.setUrl(url);
        ds.setUser(user);
        ds.setPassword(password);
        /* the connection pool will have 10 to 20 connections */
        ds.setInitialConnections(10);
        ds.setMaxConnections(20);
        /* use SSL connections without checking server certificate */
        ds.setSslMode("require");
        ds.setSslfactory("org.postgresql.ssl.NonValidatingFactory");

        return ds;
    }

Once you have created a data source by calling this function, you would use it like this:

    /* get a connection from the connection pool */
    java.sql.Connection conn = ds.getConnection();
    
    /* do some work */

    /* hand the connection back to the pool - it will not be closed */
    conn.close();

