---
title: "Getting started with servicestack"
slug: "getting-started-with-servicestack"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
You can install ServiceStack in 3 ways: 

* Complete Visual studio templates (Self hosted)
* Start from scratch run ServiceStack Self hosted (Console App)
* Run ServiceStack inside Asp.net MVC.


# Complete visual studio templates

You can find info about these here:
https://github.com/ServiceStack/ServiceStackVS.

1. Download the servicestack Visual studio plugin: https://visualstudiogallery.msdn.microsoft.com/5bd40817-0986-444d-a77d-482e43a48da7
2. Start a new project in visual studio and choose one of the templates.

# Start from scratch (Selfhosted)

For complete guide go here: https://github.com/ServiceStack/ServiceStack/wiki/Self-hosting

To setup:

1. Make a Console application
2. install ServiceStack through nuget: `Install-Package ServiceStack`
3. Setup your base code. ServiceStack uses an `AppHost` to thigh everything together:

     
     //This your main entry point of the application
     public class AppHost : AppSelfHostBase {
        public AppHost() 
          : base("HttpListener Self-Host", typeof(HelloService).Assembly) {}

        public override void Configure(Funq.Container container) { }
    }

    //Run it!
    static void Main(string[] args)
    {
        var listeningOn = args.Length == 0 ? "http://*:1337/" : args[0];
        var appHost = new AppHost()
            .Init()
            .Start(listeningOn);

        Console.WriteLine("AppHost Created at {0}, listening on {1}", 
            DateTime.Now, listeningOn);

        Console.ReadKey();
    }

Now you can start developing your api Services.

# Mvc integration

For complete Mvc integration guide: https://github.com/ServiceStack/ServiceStack/wiki/Mvc-integration

First download your nuget packages: `Install-Package ServiceStack.Mvc`

Setup your ServiceStack Apphost


    public class AppHost : AppHostBase
    {
        public AppHost() : base("MVC 4", typeof(MyServices).Assembly) {}
    
        public override void Configure(Container container)
        {            
            SetConfig(new HostConfig { 
                HandlerFactoryPath = "api" 
            });
    
            ControllerBuilder.Current.SetControllerFactory(
                new FunqControllerFactory(container));
        }
    }

    //Bootstrap your appHost through MVC Global.asax:
    public class Global : System.Web.HttpApplication
    {
        protected void Application_Start(object sender, EventArgs e)
        {
            new AppHost().Init();
        }
    }

Now to run ServiceStack services, we can add web.Config to make ServicStack run under a certain path, eg: `/api/*`

    <location path="api">
      <system.web>
        <httpHandlers>
          <add path="*" type="ServiceStack.HttpHandlerFactory, ServiceStack" 
               verb="*"/>
        </httpHandlers>
      </system.web>
    
      <system.webServer>
        <modules runAllManagedModulesForAllRequests="true"/>
        <validation validateIntegratedModeConfiguration="false" />
        <handlers>
          <add path="*" name="ServiceStack.Factory" 
               type="ServiceStack.HttpHandlerFactory, ServiceStack" verb="*" 
               preCondition="integratedMode" 
               resourceType="Unspecified" allowPathInfo="true" />
        </handlers>
      </system.webServer>
    </location>

