---
title: "log4j logs customization"
slug: "log4j-logs-customization"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Configuration file
## Configuration ##
Log4j configuration file can be in any of these formats:

 - JSON
 - YAML
 - properties (text file)
 - XML

### Configuration discovery ###

 1. Log4j will inspect the `log4j.configurationFile` system property and, if set, will attempt to load the configuration.
 2. If no system property is set log4j will look for `log4j2-test.properties` in the classpath.
 3. If no such file is found the log4j will look for `log4j2-test.yaml` or `log4j2-test.yml` in the classpath.
 4. If no such file is found the log4j will look for `log4j2-test.json` or `log4j2-test.jsn` in the classpath.
 5. If no such file is found the logj4 will look for `log4j2-test.xml` in the classpath.
 6. If a test file cannot be located the log4j will look for `log4j2.properties` on the classpath.
 7. If a properties file cannot be located the log4j will look for `log4j2.yaml` or `log4j2.yml` on the classpath.
 8. If a YAML file cannot be located the log4j will look for `log4j2.json` or `log4j2.jsn` on the classpath.
 9. If a JSON file cannot be located the log4j will try to locate `log4j2.xml` on the classpath.
 10. If no configuration file could be located the DefaultConfiguration will be used. This will cause logging output to go to the console.

## XML Configuration
## XML Example ##
The below configuration configures two appenders (log output). The first logs to standard system output (console) and the other logs to file. In this example, the location of the file can be set statically in configuration (appender `file`) or dynamically via maven filtering feature (`<Property name="APPENDER">`). The logs in file will be packed by day. The log line format `Conversion Pattern` is set as a variable. 

    <?xml version="1.0" encoding="UTF-8"?>
    <!-- 'status' sets log level for parsing configuration file itself -->
    <Configuration status="INFO">
    <Properties>
        <!-- Sets variable PID, if it's not present in log4j context will take value as below: -->
        <Property name="PID">????</Property>
        <!-- Sets variable 'LOG_PATTERN', defining how log line will look like -->
        <Property name="LOG_PATTERN">%clr{%d{yyyy-MM-dd HH:mm:ss.SSS}}{faint} %clr{%5p} %clr{${sys:PID}}{magenta} %clr{%X{usr}}{green} %clr{---}{faint}%clr{[%15.15t]}{faint} %clr{%-40.40c{1.}:%l}{cyan} %clr{:}{faint} %m%n%wEx</Property>
        <!-- LOG_DIR may be set by maven filtering feature: -->
        <Property name="LOG_DIR">@logging.path@</Property>
        <!-- APPENDER may be set by maven filtering feature, to set 'console' or 'file' appender: -->
        <Property name="APPENDER">@logging.default.appender@</Property>
    </Properties>
    <Appenders>
        <!-- Sets console output: -->
        <Console name="console" target="SYSTEM_OUT">
            <PatternLayout pattern="${LOG_PATTERN}"/>
        </Console>
        <!-- Sets output to file, and names it 'file' -->
        <RollingRandomAccessFile append="true" fileName="${LOG_DIR}/log4j2.log"
            filePattern = "${LOG_DIR}/log4j2.%d{yyyy-MM-dd}.nr%i.log.gz" name="file">
            <PatternLayout>
                <Pattern>${LOG_PATTERN}</Pattern>
            </PatternLayout>
            <Policies>
                <TimeBasedTriggeringPolicy />
            </Policies>
        </RollingRandomAccessFile>
    </Appenders>
    <Loggers>
        <!-- Sets debug for Class 'com.example.package.Clazz', and attaches to file 'file' -->
        <Logger name="com.example.package.Clazz" level="debug">
            <AppenderRef ref="file"/>
        </Logger>
        <!-- Sets 'info' for package 'org.springframework' -->
        <Logger name="org.springframework" level="info" />
        <Root level="WARN">
            <AppenderRef ref="${APPENDER}"/>
        </Root>
    </Loggers>
    </Configuration>

