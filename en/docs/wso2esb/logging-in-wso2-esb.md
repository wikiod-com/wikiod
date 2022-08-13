---
title: "Logging in WSO2 ESB"
slug: "logging-in-wso2-esb"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Separate log files for each proxy service
All proxy services events are logged in the wso2carbon log file (located in %CARBON_HOME%/repository/logs).

If you want, you can have separate log files for each proxy service. Keep in mind, though, that they will still be logged in wso2carbon log file as well.

To do so, you should change the log4j.properties file, located in the %CARBON_HOME%/repository/conf folder.

For logs rolling on **time basis**, add the following lines at the bottom of the log4j.properties file:

    log4j.category.SERVICE_LOGGER.Name_Of_Your_Proxy=INFO, YOUR_PROXY_SERVICE_APPENDER
    log4j.additivity.SERVICE_LOGGER.Name_Of_Your_Proxy=false
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER=org.apache.log4j.DailyRollingFileAppender
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER.File=logs/Name_Of_Your_Proxy.log
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER.datePattern='.'yyyy-MM-dd
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER.layout=org.apache.log4j.PatternLayout
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER.layout.ConversionPattern=%d{ISO8601} \[%X{ip}-%X{host}\] \[%t\] %5p %c{180} %m%n

Note that the `datePattern` determines how often a new file will be created. `yyyy-MM-dd` means that a new log file will be created daily and the old one will be renamed with the respective date. If you want to roll the file hourly, the pattern should also include the hours: `yyyy-MM-dd-HH`. Note that the colon character `:` should not be used anywhere in the `datePattern`.

For logs rolling on **size basis**, use the following:

    log4j.category.SERVICE_LOGGER.Name_Of_Your_Proxy=INFO, YOUR_PROXY_SERVICE_APPENDER
    log4j.additivity.SERVICE_LOGGER.Name_Of_Your_Proxy=false
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER=org.apache.log4j.RollingFileAppender
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER.File=logs/Name_Of_Your_Proxy.log
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER.MaxFileSize=10MB
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER.MaxBackupIndex=100
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER.layout=org.apache.log4j.PatternLayout
    log4j.appender.YOUR_PROXY_SERVICE_APPENDER.layout.ConversionPattern=%d{ISO8601} \[%X{ip}-%X{host}\] \[%t\] %5p %c{180} %m%n


You can set the max file size and the back up index to the desired values. 

Note that `YOUR_PROXY_SERVICE_APPENDER` is an alias and should be different for each proxy service.

The location of the log files: `logs/Name_Of_Your_Proxy.log` can be changed. `logs/Name_Of_Your_Proxy.log` leads to `%CARBON_HOME%/logs/Name_Of_Your_Proxy.log`.

