---
title: "Tracing"
slug: "tracing"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Tracing Settings
WCF tracing is built on top of System.Diagnostics. To use tracing, you should define trace sources in the configuration file or in code.

Tracing is not enabled by default. To activate tracing, you must create a trace listener and set a trace level other than "Off" for the selected trace source in configuration; otherwise, WCF does not generate any traces.

The following example shows how to enable message logging and specify additional options:

    <configuration>
     <system.diagnostics>
      <sources>
        <source name="System.ServiceModel"
                switchValue="All"
                propagateActivity="true" >
          <listeners>
            <add name="wcf_trace"/>
          </listeners>
        </source>
        <source name="System.ServiceModel.MessageLogging">
          <listeners>
            <add name="wcf_trace"/>
          </listeners>
        </source>
      </sources>
      <sharedListeners>
        <add name="wcf_trace"
             type="System.Diagnostics.XmlWriterTraceListener" 
             initializeData="c:\temp\wcf_trace\DistanceService.svclog" />
      </sharedListeners>
     </system.diagnostics>
    
     <system.serviceModel>
      <diagnostics wmiProviderEnabled="true">
          <messageLogging 
               logEntireMessage="true" 
               logMalformedMessages="true"
               logMessagesAtServiceLevel="true" 
               logMessagesAtTransportLevel="true"
               maxMessagesToLog="3000" />
      </diagnostics>
     </system.serviceModel>
    </configuration>

initializeData specifies the name of  output file for that listener. If you do not specify a file name, a random file name is generated based on the listener type used.

**Logging Levels and Options**

 - Service Level

Messages logged at this layer are about to enter (on receiving) or leave (on sending) user code. If filters have been defined, only messages that match the filters are logged. Otherwise, all messages at the service level are logged. Infrastructure messages (transactions, peer channel, and security) are also logged at this level, except for Reliable Messaging messages. On streamed messages, only the headers are logged. In addition, secure messages are logged decrypted at this level.

 - Transport Level

Messages logged at this layer are ready to be encoded or decoded for or after transportation on the wire. If filters have been defined, only messages that match the filters are logged. Otherwise, all messages at the transport layer are logged. All infrastructure messages are logged at this layer, including reliable messaging messages. On streamed messages, only the headers are logged. In addition, secure messages are logged as encrypted at this level, except if a secure transport such as HTTPS is used.

 - Malformed Level

Malformed messages are messages that are rejected by the WCF stack at any stage of processing. Malformed messages are logged as-is: encrypted if they are so, with non-proper XML, and so on. maxSizeOfMessageToLog defined the size of the message to be logged as CDATA. By default, maxSizeOfMessageToLog is equal to 256K. For more information about this attribute, see the Other Options section.



If you want to disable the trace source, you should use the logMessagesAtServiceLevel, logMalformedMessages, and logMessagesAtTransportLevel attributes of the messageLogging element instead. You should set all these attributes to false.



