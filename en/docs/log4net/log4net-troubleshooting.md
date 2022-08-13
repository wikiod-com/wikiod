---
title: "Log4Net Troubleshooting"
slug: "log4net-troubleshooting"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Log4net is a fail-stop logging system. Fail stop means that it stops logging on an internal exception and by design does not interact with the program flow. Knowing this explains you troubleshooting log4net isn't so easy. If logging fails, your program does not notice. You see a lot of questions about: why is my log4net logging not working? This article explains the basic troubleshooting and to most common solutions.

In most cases the first step is to enable internal debugging for log4net.

## Enable internal debugging
There are 2 way of enabling internal debugging in log4net:
- Specify the log4net.Internal.Debug option in the application's config file
- Enable log4net's internal debug programmatically 

**Specify the log4net.Internal.Debug option in the application's config file**

This is the preferred method to enable internal debugging, add the log4net.Internal.Debug key to the app.config file of you application.

    <?xml version="1.0" encoding="utf-8" ?>
    <configuration>
        <appSettings>
            <add key="log4net.Internal.Debug" value="true"/>
        </appSettings>
    </configuration>

Debug logging will start immediately when the application starts. 

**Enable log4net's internal debug programmatically**

A second way is to do this programmatically. Set the log4net.Util.LogLog.InternalDebugging property to true:

    log4net.Util.LogLog.InternalDebugging = true;

**Internal debug log output**

Internal debugging messages are written to the console and to the System.Diagnostics.Trace. When you have to console output you can redirect the System.Console.Out. Or you can redirect the trace message to a file:

    <configuration>
    ...
    
    <system.diagnostics>
        <trace autoflush="true">
            <listeners>
                <add 
                    name="textWriterTraceListener" 
                    type="System.Diagnostics.TextWriterTraceListener" 
                    initializeData="C:\tmp\log4net.txt" />
            </listeners>
        </trace>
    </system.diagnostics>

    ...
    </configuration>

## Buffered appenders
Some of the log4net appenders are buffered appenders. These appenders will only log when a certain amount of messages are logged. Some samples are the SmtpAppender, RemotingAppender or the AdoNetAppender. These appenders have a setting BufferSize:

    <bufferSize value="100" />

This means that the logger will log when there are 100 messages in the buffer. When you want to test the appender you can set the bufferSize to 1.

If you what the buffer to be flushed on an error, you can use an evaluator:

    <evaluator type="log4net.Core.LevelEvaluator">
         <threshold value="ERROR"/>
    </evaluator>

If the condition of the evaluator is met, the buffer will be flushed.

## Configure() not called, or called multiple times
If you do not see any logging, you can check if `Configure()` is called in your application. The easiest way is to add it as an attribute to your assembly:

    [assembly: log4net.Config.XmlConfigurator(Watch = true)]

Then you do not have to add  `log4net.Config.XmlConfigurator.Configure()` anywhere in your configuration. Or you can add `log4net.Config.XmlConfigurator.Configure()` in one of your startup methods. Make sure you only call the configuration once.

## File appender does not write
If you have a file appender, make sure you are writing to a location where the user is allowed to create and update the files. If not the logging will fail. You can check this when you have internal debugging enabled.

