---
title: "Getting started with asp.net-web-api"
slug: "getting-started-with-aspnet-web-api"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting asp.net-web-api set up or installed.

## What and Why ASP.NET Web API ?
**What? :** 
A fully supported and extensible framework for building HTTP based endpoints. In the world of HTML5, mobile devices, and modern development techniques HTTP have become the default option for building rich, scalable services. The ASP.NET Web API provides an easy to use set of default options but also provides a deep extensibility infrastructure to meet the demands of any scenario using HTTP.

**Why? :** 
- An HTML5 application that needs a services layer.
- A mobile application that needs a services layer.
- A client-server desktop application that needs a services layer.

## To add Web API to an existing MVC application.
Use Nuget to find the Web Api Package.

You can do that either by using the Manage Nuget Packages and searching for the Web Api package or use Nuget Package Manager and type

    PM> Install-Package Microsoft.AspNet.WebApi

Add WebApiConfig.cs to the App_Start/ folder The config file should contain this.

    using System.Web.Http;
    namespace WebApplication1
    {
    public class WebApiApplication : System.Web.HttpApplication
    {
        protected void Application_Start()
        {
            GlobalConfiguration.Configure(config =>
            {
                config.MapHttpAttributeRoutes();

                config.Routes.MapHttpRoute(
                    name: "DefaultApi",
                    routeTemplate: "api/{controller}/{id}",
                    defaults: new { id = RouteParameter.Optional }
                );
            });
        }
     }
    }

Source : [Configuring ASP.NET Web API][1]

Add `GlobalConfiguration.Configure(WebApiConfig.Register);` in Application_Start of the Global.asax file.


  [1]: https://docs.microsoft.com/en-us/aspnet/web-api/overview/advanced/configuring-aspnet-web-api

