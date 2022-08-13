---
title: "Getting started with log4j2"
slug: "getting-started-with-log4j2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
This would be a basic setup in wich we will send all the log messages to the console and to a log file.

Let's start with the libraries. Will use maven for that:

        <dependency>
            <groupId>org.apache.logging.log4j</groupId>
            <artifactId>log4j-api</artifactId>
            <version>2.8.2</version>
        </dependency>
        <dependency>
            <groupId>org.apache.logging.log4j</groupId>
            <artifactId>log4j-core</artifactId>
            <version>2.8.2</version>
        </dependency>

Next we'll write the log4j2.properties (I'll use a properties file in this case). We'll put it in the context path, in maven it should be in the resources folder (src/main/resources in most cases)

    name=PropertiesConfig
    property.filename = logs
    appenders = console, file
    
    appender.console.type = Console
    appender.console.name = STDOUT
    appender.console.layout.type = PatternLayout
    appender.console.layout.pattern = [%-5level] %d{yyyy-MM-dd HH:mm:ss.SSS} [%t] %c{1} - %msg%n
    
    appender.file.type = File
    appender.file.name = LOGFILE
    appender.file.fileName=${filename}/propertieslogs.log
    appender.file.layout.type=PatternLayout
    appender.file.layout.pattern=[%-5level] %d{yyyy-MM-dd HH:mm:ss.SSS} [%t] %c{1} - %msg%n
    
    loggers=file
    logger.file.name=com.es.form.studio
    logger.file.level = debug
    logger.file.appenderRefs = file
    logger.file.appenderRef.file.ref = LOGFILE
    
    rootLogger.level = debug
    rootLogger.appenderRefs = stdout
    rootLogger.appenderRef.stdout.ref = STDOUT

To use this example you'll need to change logger.file.name=com.es.form.studio so it targets the package of your choice. If you don't do that the file won't show any logs.

