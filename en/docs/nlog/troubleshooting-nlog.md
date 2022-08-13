---
title: "Troubleshooting NLog"
slug: "troubleshooting-nlog"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Enable the internal logger in nlog.config
In case of problems, enable the internal logger

    <nlog internalLogFile="c:\log.txt" internalLogLevel="Trace">
       <targets>
          <!-- target configuration here -->
       </targets>
       <rules>
          <!-- log routing rules -->
       </rules>
    </nlog>

## Enable the internal logger programmatically
In case of problems, enable the internal logger.  

C# example:

<!--  language: c# -->

    // set internal log level
    InternalLogger.LogLevel = LogLevel.Trace;

    // enable one of the targets: file, console, logwriter:

    //  enable internal logging to a file (absolute or relative path. Don't use layout renderers)
    InternalLogger.LogFile = "c:\\log.txt";

    // enable internal logging to the console
    InternalLogger.LogToConsole = true;

    // enable internal logging to the console (error stream)
    InternalLogger.LogToConsoleError = true

    // enable internal logging to the Trace
    InternalLogger.LogToTrace = true

    // enable internal logging to a custom TextWriter
    InternalLogger.LogWriter = new StringWriter(); //e.g. TextWriter writer = File.CreateText("C:\\perl.txt")




