---
title: "Configuring a JDBC Datasource"
slug: "configuring-a-jdbc-datasource"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

In order to utilize a JDBC datasource, we must first set up a [JNDI][1] reference in Tomcat. After the JNDI reference is made, JDBC datasources can be used within our Tomcat server and applications using shared or independent references (Great for `dev`/`staging`/`prod` setup, or removing connection strings/credentials from committed code).


  [1]: http://stackoverflow.com/questions/4365621/what-is-jndi-what-is-its-basic-use-when-is-it-used

Utilizing JNDI and JDBC also affords you to use ORMs like Hibernate or platforms like JPA to define "persistence units" for object and table mapp

## Configuring a server-wide JNDI reference
**Inside of your `{CATALINA_HOME}/conf/` folder exists a `server.xml` and `context.xml` file. Each one of these contains similar code, but references different parts of Tomcat to complete the same task.**

`server.xml` is server-wide configuration. This is where you can set up HTTPS, HTTP2, JNDI Resources, etc.

`context.xml` is specific to each context in Tomcat, taken from Tomcat's documentation it explains this well:

> The Context element represents a web application, which is run within a particular virtual host. Each web application is based on a Web Application Archive (WAR) file, or a corresponding directory containing the corresponding unpacked contents, as described in the Servlet Specification (version 2.2 or later). For more information about web application archives, you can download the [Servlet Specification][1], and review the Tomcat [Application Developer's Guide][2].

Essentially, it's application-specific configuration.

In order to operate correctly, we'll need to set up a `Resource` in `server.xml` and a reference to that resource inside of `context.xml`.

Inside of `server.xml`'s `<GlobalNamingResources>` element, we'll append a new `<Resource>` which will be our JNDI reference:

     <GlobalNamingResources> 
        <!-- 
          JNDI Connection Pool for AS400
          Since it uses an older version of JDBC, we have to specify a validationQuery 
          to bypass errornous calls to isValid() (which doesn't exist in older JDBC)
        -->
        <Resource name="jdbc/SomeDataSource"
                  auth="Container"
                  type="javax.sql.DataSource"
                  maxTotal="100"
                  maxIdle="30"
                  maxWaitMillis="10000"
                  username="[databaseusername]"
                  password="[databasepassword]"
                  driverClassName="com.ibm.as400.access.AS400JDBCDriver"
                  validationQuery="Select 1 from LIBRARY.TABLE"
                  url="jdbc:as400://[yourserver]:[port]"/>
  </GlobalNamingResources>

In this example, we're using a rather particular datasource (an IBMi - running DB2), which requires a `validationQuery` element set since it's using an older version of JDBC. This example is given as there is very little examples out there, as well as a display of the interoperability that a JDBC system affords you, even for an antiquated DB2 system (as above). Similar configuration would be the same for other popular database systems:

    <Resource name="jdbc/SomeDataSource"
        auth="Container"
        type="javax.sql.DataSource"
        username="[DatabaseUsername]"
        password="[DatabasePassword]"
        driverClassName="com.mysql.jdbc.Driver"
        url="jdbc:mysql:/[yourserver]:[port]/[yourapplication]"
        maxActive="15"
        maxIdle="3"/>

Inside of `context.xml` we'll need to configure a "pointer" towards our jdbc datasource (which we made with a JNDI reference):

    <Context>
        ...
        <ResourceLink name="jdbc/SomeDataSource"
          global="jdbc/SomeDataSource"
          type="javax.sql.DataSource" />
    </Context>

Utilizing `ResourceLink` inside of `context.xml` allows us to reference the same datasource across applications and have it configured at the server level for multiple-database systems. (Although it also works just as well with one database)


  [1]: http://wiki.apache.org/tomcat/Specifications
  [2]: https://tomcat.apache.org/tomcat-8.5-doc/appdev/index.html

## Using a JNDI reference as a JDBC Resource in Context
    public void test() {
        Connection conn = null;
        Statement stmt = null;
        try {
            Context ctx = (Context) new InitialContext().lookup("java:comp/env");
            conn = ((DataSource) ctx.lookup("jdbc/SomeDataSource")).getConnection();

            stmt = conn.createStatement();
            //SQL data fetch using the connection
            ResultSet rs = stmt.executeQuery("SELECT * FROM TABLE");
            while (rs.next()) {
                System.out.println(rs.getString("Id"));
            }
            conn.close();
            conn = null;
        }
        catch(Exception e){
            e.printStackTrace();
        }
        finally {

            if (stmt != null || conn != null) try {
                assert stmt != null;
                stmt.close();
            } catch (SQLException ex) {
                // ignore -- as we can't do anything about it here
                ex.printStackTrace();
            }
        }
    }

