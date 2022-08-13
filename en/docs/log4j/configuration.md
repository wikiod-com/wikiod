---
title: "configuration"
slug: "configuration"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Log4j property file
Below is a configuration file for log4j. Log4j2 can use the same syntax, but there are different appender classes:

    log4j.rootLogger=INFO, FOO

    ## ConsoleAppender
    log4j.appender.CA=org.apache.log4j.ConsoleAppender
    log4j.appender.CA.layout=org.apache.log4j.PatternLayout
    log4j.appender.CA.layout.ConversionPattern= %d{hh:mm:ss,SSS} [%t] %-5p %c %x - %m%n

    ## FileAppender
    log4j.appender.FOO=org.apache.log4j.RollingFileAppender
    log4j.appender.FOO.File=${catalina.home}/logs/app.log
    log4j.appender.FOO.Append=true
    log4j.appender.FOO.layout=org.apache.log4j.PatternLayout
    log4j.appender.FOO.layout.ConversionPattern= %d{hh:mm:ss,SSS} [%t] %-5p %c %x - %m%n

    ## attaching appender to specific package:
    log4j.logger.com.example.package=INFO, CA

Directive `log4j.rootLogger` defines log level and appender for any class that doesn't meet `logger` criteria. Notice that appender `name` is defined after word 'appender'.

## Resolve run time issue with log4j configuration
Users may face the following issue:

     log4j:WARN No appenders could be found for logger (dao.hsqlmanager).
     log4j:WARN Please initialize the log4j system properly.
     log4j:WARN See http://logging.apache.org/log4j/1.2/faq.html#noconfig for more info.

One reason this can occur is if the log4j.properties or .xml file is not located within the project itself. (This can happen when you ship a tool, where the tool/JAR is in one directory and all the configuration is another directory).

You will then need to specify the path to the log4j.properties or .xml file. In the command line utility,

    java -Dlog4j.configuration=file:///path/To/log4j.properties YourProject.jar

or if you have a script to run the tool you can add

`-Dlog4j.configuration=file:///path/To/log4j.properties` 

to the place where you do the equivalent action of the command line version. Not that `log4j.configuration` is specified in a URL format, prefixed by `file:///`.

