---
title: "Getting started with hibernate"
slug: "getting-started-with-hibernate"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Using XML Configuration to set up Hibernate
I create a file called `database-servlet.xml` somewhere on the classpath. 

Initially your config file will look like this:

<!-- language: xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    <beans xmlns="http://www.springframework.org/schema/beans"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:jdbc="http://www.springframework.org/schema/jdbc"
    xmlns:tx="http://www.springframework.org/schema/tx"
    xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.2.xsd
        http://www.springframework.org/schema/jdbc http://www.springframework.org/schema/jdbc/spring-jdbc-3.2.xsd
        http://www.springframework.org/schema/tx http://www.springframework.org/schema/tx/spring-tx-3.2.xsd">

    </beans>

You'll notice I imported the `tx` and `jdbc` Spring namespaces. This is because we are going to use them quite heavily in this config file.

First thing you want to do is enable annotation based transaction management (`@Transactional`). The main reason that people use Hibernate in Spring is because Spring will manage all your transactions for you. Add the following line to your configuration file:

<!-- language: xml -->

    <tx:annotation-driven />

We need to create a data source. The data source is basically the database that Hibernate is going to use to persist your objects. Generally one transaction manager will have one data source. If you want Hibernate to talk to multiple data sources then you have multiple transaction managers.

<!-- language: xml -->

    <bean id="dataSource" 
        class="org.springframework.jdbc.datasource.DriverManagerDataSource">
        <property name="driverClassName" value="" />
        <property name="url" value="" />
        <property name="username" value="" />
        <property name="password" value="" />
    </bean>

The class of this bean can be anything that implements `javax.sql.DataSource` so you could write your own. This example class is provided by Spring, but doesn't have its own thread pool. A popular alternative is the Apache Commons `org.apache.commons.dbcp.BasicDataSource`, but there are many others. I'll explain each of the properties below:

 - *driverClassName*: The path to your JDBC driver. This is a **database specific** JAR that should be available on your classpath. Ensure that you have the most up to date version. If you are using an Oracle database, you'll need a OracleDriver. If you have a MySQL database, you'll need a MySQLDriver. See if you can find the driver you need [here][1] but a quick google should give you the correct driver.

 - *url*: The URL to your database. Usually this will be something like `jdbc\:oracle\:thin\:\path\to\your\database` or `jdbc:mysql://path/to/your/database`. If you google around for the default location of the database you are using, you should be able to find out what this should be. If you are getting a `HibernateException` with the message `org.hibernate.HibernateException: Connection cannot be null when 'hibernate.dialect' not set` and you are following this guide, there is a 90% chance that your URL is wrong, a 5% chance that your database isn't started and a 5% chance that your username/password is wrong.

 - *username*: The username to use when authenticating with the database.
 
 - *password*: The password to use when authenticating with the database.

The next thing, is to set up the `SessionFactory`. This is the thing that Hibernate uses to create and manage your transactions, and actually talks to the database. It has quite a few configuration options that I will try to explain below.

<!-- language: xml -->

    <bean id="sessionFactory"
        class="org.springframework.orm.hibernate4.LocalSessionFactoryBean">
        <property name="dataSource" ref="dataSource" />
        <property name="packagesToScan" value="au.com.project />
        <property name="hibernateProperties">
            <props>
                <prop key="hibernate.use_sql_comments">true</prop>
                <prop key="hibernate.hbm2ddl.auto">validate</prop>
            </props>
        </property>
    </bean>

 - *dataSource*: Your data source bean. If you changed the Id of the dataSource, set it here.
 
 - *packagesToScan*: The packages to scan to find your JPA annotated objects. These are the objects that the session factory needs to manage, will generally be POJO's and annotated with `@Entity`. For more information on how to set up object relationships in Hibernate [see here][2].

 - *annotatedClasses* (not shown): You can also provide a list of classes for Hibernate to scan if they are not all in the same package. You should use either `packagesToScan` or `annotatedClasses` but not both. The declaration looks like this:
    
<!-- language: xml -->

    <property name="annotatedClasses">
        <list>
            <value>foo.bar.package.model.Person</value>
            <value>foo.bar.package.model.Thing</value>
        </list>
    </property>

 - *hibernateProperties*: There are a myriad of these all lovingly [documented here][3]. The main ones you will be using are as follows:
  - *hibernate.hbm2ddl.auto*: One of the hottest Hibernate questions details this property. [See it for more info][4]. I generally use validate, and set up my database using either SQL scripts (for an in-memory), or create the database beforehand (existing database).
  - *hibernate.show_sql*: Boolean flag, if true Hibernate will print all the SQL it generates to `stdout`. You can also configure your logger to show you the values that are being bound to the queries by setting `log4j.logger.org.hibernate.type=TRACE` `log4j.logger.org.hibernate.SQL=DEBUG` in your log manager (I use log4j).
  - *hibernate.format_sql*: Boolean flag, will cause Hibernate to pretty print your SQL to stdout.
  - *hibernate.dialect* (Not shown, for good reason): A lot of old tutorials out there show you how to set the Hibernate dialect that it will use to communicate to your database. Hibernate **can** auto-detect which dialect to use based on the JDBC driver that you are using. Since there are about 3 different Oracle dialects and 5 different MySQL dialects, I'd leave this decision up to Hibernate. For a full list of dialects Hibernate supports [see here][5].

The last 2 beans you need to declare are:

<!-- language: xml -->

    <bean class="org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor"
        id="PersistenceExceptionTranslator" />

    <bean id="transactionManager" 
        class="org.springframework.orm.hibernate4.HibernateTransactionManager">
        <property name="sessionFactory" ref="sessionFactory" />
    </bean>

The `PersistenceExceptionTranslator` translates database specific `HibernateException` or `SQLExceptions` into Spring exceptions that can be understood by the application context. 

The `TransactionManager` bean is what controls the transactions as well as roll-backs.

Note: You should be autowiring your `SessionFactory` bean into your DAO's.

 [1]: http://mvnrepository.com/tags/jdbc
  [2]: http://stackoverflow.com/questions/24257449/how-do-i-use-annotations-to-define-x-relationship-in-hibernate-4-and-spring
  [3]: http://docs.jboss.org/hibernate/orm/4.3/manual/en-US/html_single/#configuration-optional
  [4]: http://stackoverflow.com/questions/438146/hibernate-hbm2ddl-auto-possible-values-and-what-they-do
  [5]: http://docs.jboss.org/hibernate/orm/4.3/manual/en-US/html_single/#configuration-optional-dialects

## XML-less Hibernate configuration
This example has been taken from [here][1] 

    package com.reborne.SmartHibernateConnector.utils;
    
    import org.hibernate.HibernateException;
    import org.hibernate.Session;
    import org.hibernate.SessionFactory;
    import org.hibernate.cfg.Configuration;
    
    public class LiveHibernateConnector implements IHibernateConnector {
    
        private String DB_DRIVER_NAME = "";
        private String DB_URL = "jdbc:h2:~/liveDB;MV_STORE=FALSE;MVCC=FALSE";
        private String DB_USERNAME = "sa";
        private String DB_PASSWORD = "";
        private String DIALECT = "org.hibernate.dialect.H2Dialect";
        private String HBM2DLL = "create";
        private String SHOW_SQL = "true";
        
        private static Configuration config;
        private static SessionFactory sessionFactory;
        private Session session;
        
        private boolean CLOSE_AFTER_TRANSACTION = false;
    
        public LiveHibernateConnector() {
            
            config = new Configuration();
    
            config.setProperty("hibernate.connector.driver_class",         DB_DRIVER_NAME);
            config.setProperty("hibernate.connection.url",                 DB_URL);
            config.setProperty("hibernate.connection.username",         DB_USERNAME);
            config.setProperty("hibernate.connection.password",         DB_PASSWORD);
            config.setProperty("hibernate.dialect",                     DIALECT);
            config.setProperty("hibernate.hbm2dll.auto",                 HBM2DLL);
            config.setProperty("hibernate.show_sql",                    SHOW_SQL);
        
            /*
             * Config connection pools
             */
    
            config.setProperty("connection.provider_class", "org.hibernate.connection.C3P0ConnectionProvider");
            config.setProperty("hibernate.c3p0.min_size", "5");
            config.setProperty("hibernate.c3p0.max_size", "20");
            config.setProperty("hibernate.c3p0.timeout", "300");
            config.setProperty("hibernate.c3p0.max_statements", "50");
            config.setProperty("hibernate.c3p0.idle_test_period", "3000");
            
            
            /**
             * Resource mapping
             */
            
    //        config.addAnnotatedClass(User.class);
    //        config.addAnnotatedClass(User.class);
    //        config.addAnnotatedClass(User.class);
        
            sessionFactory = config.buildSessionFactory();
        }
    
    
        public HibWrapper openSession() throws HibernateException {
            return new HibWrapper(getOrCreateSession(), CLOSE_AFTER_TRANSACTION);
        }
    
    
        public Session getOrCreateSession() throws HibernateException {
            if (session == null) {
                session = sessionFactory.openSession();
            }
            return session;
        }
    
        public void reconnect() throws HibernateException {
            this.sessionFactory = config.buildSessionFactory();
        }
    
        
    }

Please note, that with latest Hibernate this approach doesn't work well (Hibernate 5.2 release still allow this configuration)

  [1]: https://github.com/reborne/SmartHibernateConnector

## Simple hibernate example using XML
To set up a simple hibernate project using XML for the configurations you need 3 files, hibernate.cfg.xml, a POJO for each entity, and a EntityName.hbm.xml for each entity. Here is an example of each using MySQL:

hibernate.cfg.xml

    <?xml version="1.0" encoding="utf-8"?>
    <!DOCTYPE hibernate-configuration PUBLIC 
    "-//Hibernate/Hibernate Configuration DTD 3.0//EN"
    "http://hibernate.sourceforge.net/hibernate-configuration-3.0.dtd">
    
    <hibernate-configuration>
       <session-factory>
       <property name="hibernate.dialect">
          org.hibernate.dialect.MySQLDialect
       </property>
       <property name="hibernate.connection.driver_class">
          com.mysql.jdbc.Driver
       </property>
    
       <property name="hibernate.connection.url">
          jdbc:mysql://localhost/DBSchemaName
       </property>
       <property name="hibernate.connection.username">
          testUserName
       </property>
       <property name="hibernate.connection.password">
          testPassword
       </property>
    
       <!-- List of XML mapping files -->
       <mapping resource="HibernatePractice/Employee.hbm.xml"/>
    
    </session-factory>
    </hibernate-configuration>

DBSchemaName, testUserName, and testPassword would all be replaced.
Make sure to use the full resource name if it is in a package.

Employee.java

    package HibernatePractice;
    
    public class Employee {
        private int id;
        private String firstName;
        private String middleName;
        private String lastName;
        
        public Employee(){
            
        }
        public int getId(){
            return id;
        }
        public void setId(int id){
            this.id = id;
        }
        public String getFirstName(){
            return firstName;
        }
        public void setFirstName(String firstName){
            this.firstName = firstName;
        }
        public String getMiddleName(){
            return middleName;
        }
        public void setMiddleName(String middleName){
            this.middleName = middleName;
        }
        public String getLastName(){
            return lastName;
        }
        public void setLastName(String lastName){
            this.lastName = lastName;
        }
    }

Employee.hbm.xml

<?xml version="1.0"?>
<!DOCTYPE hibernate-mapping PUBLIC
    "-//Hibernate/Hibernate Mapping DTD//EN"
    "http://hibernate.sourceforge.net/hibernate-mapping-3.0.dtd" >

    <hibernate-mapping>
       <class name="HibernatePractice.Employee" table="employee">
          <meta attribute="class-description">
             This class contains employee information. 
          </meta>
          <id name="id" type="int" column="empolyee_id">
             <generator class="native"/>
          </id>
          <property name="firstName" column="first_name" type="string"/>
          <property name="middleName" column="middle_name" type="string"/>
          <property name="lastName" column="last_name" type="string"/>
       </class>
    </hibernate-mapping>

Again, if the class is in a package use the full class name packageName.className.

After you have these three files you are ready to use hibernate in your project.

