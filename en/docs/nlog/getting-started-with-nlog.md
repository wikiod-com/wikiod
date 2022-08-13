---
title: "Getting started with nlog"
slug: "getting-started-with-nlog"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Writing first logs
Install the [NLog.Config](https://www.nuget.org/packages/NLog.Config/) package. (NuGet 3 users should install [NLog](https://www.nuget.org/packages/NLog/) and add the config manually.) Write to logger:

<!-- language: c# -->

    using NLog;

    public class MyClass
    {
      private static Logger logger = LogManager.GetCurrentClassLogger();
    
      public void MyMethod1()
      {
        logger.Trace("Sample trace message");
        logger.Debug("Sample debug message");
        logger.Info("Sample informational message");
        logger.Warn("Sample warning message");
        logger.Error("Sample error message");
        logger.Fatal("Sample fatal error message");
    
        // alternatively you can call the Log() method 
        // and pass log level as the parameter.
        logger.Log(LogLevel.Info, "Sample informational message");

        try
        {
            SendMail();
        }
        catch(Exception ex)
        {
           //example writing exception
          logger.Error(ex, "Error when sending mail");
        }
      }
    }

This example config should be added with the NLog.Config package. Otherwise add the following to the root of the application - or next to the .dll as "nlog.config"

<!-- language: xml -->

    <?xml version="1.0" encoding="utf-8" ?>
    <nlog xmlns="http://www.nlog-project.org/schemas/NLog.xsd"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation="http://www.nlog-project.org/schemas/NLog.xsd NLog.xsd"
          autoReload="true"
          throwExceptions="false"
          internalLogLevel="Off" internalLogFile="c:\temp\nlog-internal.log">
    
      <!-- optional, add some variables
      https://github.com/nlog/NLog/wiki/Configuration-file#variables
      -->
      <variable name="myvar" value="myvalue"/>
    
      <!--
      See https://github.com/nlog/nlog/wiki/Configuration-file
      for information on customizing logging rules and outputs.
       -->
      <targets>
    
        <!--
        add your targets here
        See https://github.com/nlog/NLog/wiki/Targets for possible targets.
        See https://github.com/nlog/NLog/wiki/Layout-Renderers for the possible layout renderers.
        -->
    
        <!--  Write events to a file with the date in the filename.  -->
        <target xsi:type="File" name="f" fileName="${basedir}/logs/${shortdate}.log"
                layout="${longdate} ${uppercase:${level}} ${message} ${exception}" />
       
      </targets>
    
      <rules>
        <!-- add your logging rules here -->
    
        <!--
        Write all events with minimal level of Debug (So Debug, Info, Warn, Error and Fatal, but not Trace)  to "f"  -->
        <logger name="*" minlevel="Debug" writeTo="f" />
       
      </rules>
    </nlog>

The config writes the log messages to a file.

