---
title: "Getting started with ASP.NET"
slug: "getting-started-with-aspnet"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
By default, all the required libraries for build ASP.NET applications are included during the installation of Visual Studio. If a newer version of ASP.NET is released that was not included with Visual Studio, you can download the appropriate SDK library from Microsoft, which will include all the necessary libraries for that version.

Similarly, the Windows operating system comes pre-installed with a more recent version of ASP.NET and is automatically registered with IIS for configuration and execution. Similarly, if a newer version of ASP.NET becomes available, you can install the SDK for the version you need and then use the `aspnet_regiis` tool to register the framework with IIS for use.

It should be also noted that for server deployments, there also exists a ASP.NET SDK Redistributable package. This version is a streamlined version of the SDK, with just the essential libraries and does not have the tools and integrations with Visual Studio in it.

## ASP.NET Overview
ASP.NET is a unified Web development model that includes the services necessary for you to build enterprise-class Web applications with a minimum of coding. ASP.NET is part of the .NET Framework, and when coding ASP.NET applications you have access to classes in the .NET Framework. 

You can code your applications in any language compatible with the common language runtime (CLR), including Microsoft Visual Basic, C#, JScript .NET, and J#. These languages enable you to develop ASP.NET applications that benefit from the common language runtime, type safety, inheritance, and so on.

ASP.NET includes:

 - A page and controls framework 
 - The ASP.NET compiler 
 - Security infrastructure 
 - State-management facilities 
 - Application configuration
 - Health monitoring and performance features 
 - Debugging support 
 - An XML Web services framework  
 - Extensible hosting environment and
   application life cycle management
 - An extensible designer environment

## Simple Intro of ASP.NET
Asp.net is  web application framework developed by Microsoft to build dynamic data-driven Web Application and WebServices.

Asp.net  is basically a subset of wider .NET framework. A framework is nothing but a collection of classes. 

In .NET Framework you can build Console application. Web Application, Window Application, Mobile Application. So for web application ASP.net is being used.

ASP.NET is the successor to classic ASP (Active Server Page.) 

**What is Web Application?**

  A web application is an application that is accessed by users using a web browser such as:
  - Microsoft Internet Explorer.
  - Google Chrome
  - Mozilla FireFox
  - Apple safari

## Hello World with OWIN
Use the packet manager to install Microsoft.Owin.SelfHost

    install-packet Microsoft.Owin.SelfHost


Code for a bare minimum HelloWorld web application running from a console window:

    namespace HelloOwin
    {
        using System;
        using Owin;
    
        class Program
        {
            static readonly string baseUrl = "http://localhost:8080";
    
            static void Main(string[] args)
            {
                using (Microsoft.Owin.Hosting.WebApp.Start<Startup>(baseUrl))
                {
                    Console.WriteLine("Prease any key to quit.");
                    Console.ReadKey();
                }
            }
    
            
        }
    
        public class Startup
        {
            public void Configuration(IAppBuilder app)
            {
                app.Run(ctx =>
                {
                    return ctx.Response.WriteAsync("Hello World");
                });
            }
        }
    }



