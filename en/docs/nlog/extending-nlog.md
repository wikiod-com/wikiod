---
title: "Extending NLog"
slug: "extending-nlog"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Create custom Target
<!-- language: csharp -->

    using NLog;
    using NLog.Config;
    using NLog.Targets;
    
    namespace MyNamespace 
    { 
        
        [Target("MyFirst")] 
        public sealed class MyFirstTarget: TargetWithLayout  //or inherit from Target
        { 
            public MyFirstTarget()
            {
                //set defaults
                this.Host = "localhost";
            }
    
            [RequiredParameter] 
            public string Host { get; set; }
    
            protected override void Write(LogEventInfo logEvent) 
            { 
                string logMessage = this.Layout.Render(logEvent); 
    
                //TODO write to target
            } 
        } 
    }

Register it under the name `MyFirst` - as soon as possible - e.g. in `main()`, `application_start()`.

<!-- language: csharp -->

    ConfigurationItemFactory
              .Default
              .Targets
              .RegisterDefinition("MyFirst", typeof(MyNamespace.MyFirstTarget));


Usage:

<!-- language: xml -->

    <?xml version="1.0" ?>
    <nlog xmlns="http://www.nlog-project.org/schemas/NLog.xsd"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    
        <targets>
           <target name="target1" type="MyFirst" Host="somehost.com" />
        </targets>
    
        <rules>
            <logger name="*" minlevel="Debug" writeTo="target1" />
        </rules>
    </nlog>



## Create custom Layout Renderer
<!-- language: csharp -->

    [LayoutRenderer("hello-world")]
    public class HelloWorldLayoutRenderer : LayoutRenderer
    {

        /// <summary>
        /// I'm option and not required or default
        /// </summary>
        public string Config1 { get; set; }

        /// <summary>
        /// I'm required option. And error will be thrown if not set.
        /// </summary>
        [RequiredParameter]
        public string Config2 { get; set; }

        /// <summary>
        /// I'm the default parameter.
        /// The first parameter value (without name) will be set to this one
        /// You can set me as required also. 
        /// </summary>
        [DefaultParameter]
        public bool Caps {get;set;}

        protected override void Append(StringBuilder builder, LogEventInfo logEvent)
        {
            //TODO use options 
            builder.Append("hello world!");
        }
    }


Register - as soon as possible - e.g. in `main()`, `application_start()`.


<!-- language: csharp -->

    //register under "hello-world"
    ConfigurationItemFactory.Default.LayoutRenderers
                            .RegisterDefinition("hello-world", typeof(MyNamespace.HelloWorldLayoutRenderer ));


Usage

    ${hello-world} - raises exception: required parameter Config2 isn't set
    ${hello-world:Config2=abc} - OK, Config2 property set
    ${hello-world:true:config2=abc} - default parameter (Caps) set to true
    ${hello-world:true:config2=abc:config1=yes} - all the three properties set.



