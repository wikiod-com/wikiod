---
title: "How to DisableEnable WCF tracing in C# application code"
slug: "how-to-disableenable-wcf-tracing-in-c-application-code"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## One way: use a custom listener defined in your C# code
It took me a while to get this right, so I decided to share a solution because it might save someone else several days of trial and error.

The problem: I want to be able to enable/disable WCF tracing in my C# .NET application and choose the trace output filename. I don't want users editing the .config file, there's too much room for error there.

Here is one solution.

The application's .config file:

    <?xml version="1.0"?>
    <configuration>
      <system.diagnostics>
        <trace autoflush="true"/>
        <sources>
          <source name="System.ServiceModel" switchValue="All">
            <listeners>
              <add name="MyListener"/>
            </listeners>
          </source>
          <source name="System.ServiceModel.MessageLogging" switchValue="All">
            <listeners>
              <add name="MyListener"/>
            </listeners>
          </source>
          <source name="System.ServiceModel.Activation" switchValue="All">
            <listeners>
              <add name="MyListener"/>
            </listeners>
          </source>
          <source name="System.IdentityModel" switchValue="All">
            <listeners>
              <add name="MyListener"/>
            </listeners>
          </source>
        </sources>
        <sharedListeners>
          <add name="MyListener" type="MyNamespace.MyXmlListener, MyAssembly"/>
        </sharedListeners>
      </system.diagnostics>
      <system.serviceModel>
        <diagnostics wmiProviderEnabled="true">
          <messageLogging
            logEntireMessage="true"
            logMalformedMessages="true"
            logMessagesAtServiceLevel="true"
            logMessagesAtTransportLevel="true"
            maxMessagesToLog="1000"
            maxSizeOfMessageToLog="8192"/>
        </diagnostics>
      </system.serviceModel>
      <startup>
        <supportedRuntime version="v4.0" sku=".NETFramework,Version=v4.0"/>
      </startup>
    </configuration>

And the C# code:

    using System;
    using System.IO;
    using System.Diagnostics;
    namespace MyNamespace
    {
        public class MyXmlListener : XmlWriterTraceListener
        {
            public static String TraceOutputFilename = String.Empty;

            public static Stream MakeOutputStream()
            {
                if (String.IsNullOrWhiteSpace(TraceOutputFilename))
                    return Stream.Null;

                return new FileStream(TraceOutputFilename, FileMode.Create);
            }

            public MyXmlListener ()
                : base(MakeOutputStream())
            { }
        }
    }

To enable WCF tracing to a file, set TraceOutputFilename before the WCF object is created:

    MyXmlListener.TraceOutputFilename = "trace.svclog";

I've gotten great benefits from this forum, I hope this post pays some of it forward.

Getting the "type" right in the .config file was much more challenging than it should have been, see [Specifying Fully Qualified Type Names][1] for setting the "type" correctly in a .config file.

  [1]: https://msdn.microsoft.com/en-us/library/yfsftwz6(v=vs.110).aspx

