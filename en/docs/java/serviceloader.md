---
title: "ServiceLoader"
slug: "serviceloader"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

`ServiceLoader` can be used to get instances of classes extending a given type(=service) that are specified in a file packed in a `.jar` file. The service that is extended/implemented is often a interface, but this is not required.

The extending/implementing classes need to provide a zero argument constructor for the `ServiceLoader` to instantiate them.

To be discovered by the `ServiceLoader` a text file with the name of the fully qualified type name of the implemented service needs to be stored inside the `META-INF/services` directory in the jar file. This file contains one fully qualified name of a class implementing the service per line.

## Logger Service
The following example shows how to instantiate a class for logging via the `ServiceLoader`.

# Service

    package servicetest;
    
    import java.io.IOException;
    
    public interface Logger extends AutoCloseable {
        
        void log(String message) throws IOException;
    }

## Implementations of the service

The following implementation simply writes the message to `System.err`

    package servicetest.logger;
    
    import servicetest.Logger;
    
    public class ConsoleLogger implements Logger {
    
        @Override
        public void log(String message) {
            System.err.println(message);
        }
    
        @Override
        public void close() {
        }
    
    }

The following implementation writes the messages to a text file:

    package servicetest.logger;
    
    import java.io.BufferedWriter;
    import java.io.FileWriter;
    import java.io.IOException;
    import servicetest.Logger;
    
    public class FileLogger implements Logger {
    
        private final BufferedWriter writer;
    
        public FileLogger() throws IOException {
            writer = new BufferedWriter(new FileWriter("log.txt"));
        }
    
        @Override
        public void log(String message) throws IOException {
            writer.append(message);
            writer.newLine();
        }
    
        @Override
        public void close() throws IOException {
            writer.close();
        }
    
    }

# META-INF/services/servicetest.Logger

The `META-INF/services/servicetest.Logger` file lists the names of the `Logger` implementations.

<!-- language: lang-none -->

    servicetest.logger.ConsoleLogger
    servicetest.logger.FileLogger

# Usage

The following `main` method writes a message to all available loggers. The loggers are instantiated using `ServiceLoader`.

    public static void main(String[] args) throws Exception {
        final String message = "Hello World!";

        // get ServiceLoader for Logger
        ServiceLoader<Logger> loader = ServiceLoader.load(servicetest.Logger.class);

        // iterate through instances of available loggers, writing the message to each one
        Iterator<Logger> iterator = loader.iterator();
        while (iterator.hasNext()) {
            try (Logger logger = iterator.next()) {
                logger.log(message);
            }
        }
    }

## Simple ServiceLoader Example


