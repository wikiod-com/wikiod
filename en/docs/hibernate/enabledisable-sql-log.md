---
title: "EnableDisable SQL log"
slug: "enabledisable-sql-log"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

Logging these queries is **slow**, even slower than Hibernate usually is. It also uses up a massive amount of log space. Do not use logging in scenarios where performance is required. Use this only when testing the queries that Hibernate actually generates.

## Using a logging config file
In the logging configuration file of your choice set the logging of the following packages to the levels shown.:

    # log the sql statement
    org.hibernate.SQL=DEBUG
    # log the parameters
    org.hibernate.type=TRACE

There will probably be some logger specific prefixes that are required.

Log4j config:

    log4j.logger.org.hibernate.SQL=DEBUG
    log4j.logger.org.hibernate.type=TRACE

Spring Boot `application.properties`:

    logging.level.org.hibernate.SQL=DEBUG
    logging.level.org.hibernate.type=TRACE

Logback `logback.xml`:

    <logger name="org.hibernate.SQL" level="DEBUG"/>
    <logger name="org.hibernate.type" level="TRACE"/>

## Using Hibernate properties
This will show you the generated SQL, but will not show you the values contained within the queries.

    <bean id="sessionFactory"
        class="org.springframework.orm.hibernate4.LocalSessionFactoryBean">
        <property name="hibernateProperties">
            <props>
                <!-- show the sql without the parameters -->
                <prop key="hibernate.show_sql">true</prop>
                <!-- format the sql nice -->
                <prop key="hibernate.format_sql">true</prop>
                <!-- show the hql as comment -->
                <prop key="use_sql_comments">true</prop>
            </props>
        </property>
    </bean>

## Enable/Disable SQL log in debug
Some applications that use Hibernate generate a huge amount of SQL when the application is started. Sometimes it's better to enable/disable the SQL log in specific points when debugging.


To enable, just run this code in your IDE when you are debugging the aplication:

    org.apache.log4j.Logger.getLogger("org.hibernate.SQL")
        .setLevel(org.apache.log4j.Level.DEBUG)

To disable: 

    org.apache.log4j.Logger.getLogger("org.hibernate.SQL")
        .setLevel(org.apache.log4j.Level.OFF)

