---
title: "Getting started with asp.net-mvc-5"
slug: "getting-started-with-aspnet-mvc-5"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What's New in ASP.NET MVC 5
1) Authentication filters 
             Are a new kind of filter added in ASP.NET MVC 5.0 .That run prior to authorization filters in the ASP.NET MVC pipeline and allow you to specify authentication logic per-action, per-controller, or globally for all controllers. Authentication filters process credentials in the request and provide a corresponding principal. Authentication filters can also add authentication challenges in response to unauthorized requests. 

  2.Filter overrides
              You can now override which filters apply to a given action method or      controller by specifying an override filter.

  3) Attribute routing

## Install MVC5 or Update to Specific Version
To install/update MVC version, follow these steps: 

 1. In visual studio, open the Package Manager console (use CTRL + Q, and type package manager console)
 2. In the console appearing, enter the following after the console cursor showing `PM>`:

        Install-Package Microsoft.AspNet.Mvc -Version 5.2.3
    *Note: specify the version you want. In the above example we used 5.2.3 (the latest version when these instructions were written)*

 3. Verify the installation by using the following command in the package manager console:
 
        Get-Package -ListAvailable -Filter mvc

