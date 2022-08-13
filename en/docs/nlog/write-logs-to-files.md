---
title: "Write logs to files"
slug: "write-logs-to-files"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Write to single file
Write all events with level: debug, info, warn, error and fatal to one file:

<!-- language: xml -->

    <?xml version="1.0" ?>
    <nlog xmlns="http://www.nlog-project.org/schemas/NLog.xsd"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    
        <targets>
            <target name="file" xsi:type="File"
                layout="${longdate} ${logger} ${message} ${exception}" 
                fileName="${basedir}/logs/logfile.txt" />
        </targets>
    
        <rules>
            <logger name="*" minlevel="Debug" writeTo="file" />
        </rules>
    </nlog>

## Write one log file per day
Create one log file for each day. The log files will have the following names (dependent of culture)

    2016-06-05.log
    2016-06-06.log
    2016-06-07.log
    ...


<!-- language: xml -->

    <?xml version="1.0" ?>
    <nlog xmlns="http://www.nlog-project.org/schemas/NLog.xsd"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    
        <targets>
            <target name="file" xsi:type="File"
                layout="${longdate} ${logger} ${message} ${exception}" 
                fileName="${basedir}/${shortdate}.log" />
        </targets>
    
        <rules>
            <logger name="*" minlevel="Debug" writeTo="file" />
        </rules>
    </nlog>



