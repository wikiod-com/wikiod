---
title: "Getting started with asp.net-mvc-4"
slug: "getting-started-with-aspnet-mvc-4"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What are new features in MVC 4? Why to use MVC 4?
The fourth version of the framework focuses mainly on making mobile web application development easier.

**New features in AP.NET MVC 4**


1) **ASP.NET Web API**

ASP.NET Web API is a framework that makes it easy to build HTTP services that reach a broad range of clients, including browsers and mobile devices. 

HTTP is not just for serving up web pages. It is also a powerful platform for building APIs that expose services and data. Almost any platform that you can think of has an HTTP library, so HTTP services can reach a broad range of clients, including browsers, mobile devices, and traditional desktop applications.

ASP.NET Web API is an ideal platform for building RESTful applications on the .NET Framework.


2) **Enhancement To Default project templates**

The template that is used to create new ASP.NET MVC 4 projects has been updated to look like more modern-looking website:

[![enter image description here][1]][1]

In addition to UI improvements, the also template employs a technique called adaptive rendering to look good in both desktop browsers and mobile browsers without any customization.

3) **Mobile Project template using jquery Mobile**

ASP.NET MVC 4 also introducesd new Moble Application project template to create a site specifically for mobile and tablet browsers. This is based on jQuery Mobile, an open-source library for building touch-optimized UI.

[![enter image description here][2]][2]

4. **Display Modes**

The new Display Modes feature lets an application select views depending on the browser that's making the request. For example, if a desktop browser requests the Home page, the application might use the `Views\Home\Index.cshtml` template. If a mobile browser requests the Home page, the application might return the `Views\Home\Index.mobile.cshtml` template.


If you want to create more specific views, layouts, or partial views for other devices, you can register a new DefaultDisplayMode instance to specify which name to search for when a request satisfies particular conditions. For example, you could add the following code to the `Application_Start` method in the Global.asax file to register the string "iPhone" as a display mode that applies when the Apple iPhone browser makes a request:

    DisplayModeProvider.Instance.Modes.Insert(0, new
    DefaultDisplayMode("iPhone")
    {
        ContextCondition = (context => context.GetOverriddenUserAgent().IndexOf
            ("iPhone", StringComparison.OrdinalIgnoreCase) >= 0)
     });

For more details: [ASP.NET  MVC 4 Mobile Features][3]

5) **Task Support for Asynchronous Controller**

The ASP.NET MVC 4 Controller class in combination .NET 4.5  enables you to write asynchronous action methods that return an object of type  `Task<ActionResult>`. The .NET Framework 4 introduced an asynchronous programming concept referred to as a Task and ASP.NET MVC 4 supports Task. Tasks are represented by the Task  type and related types in the System.Threading.Tasks namespace. The .NET Framework 4.5 builds on this asynchronous support with  the `await` and `async` keywords that make working with Task objects much less complex than previous asynchronous approaches.  The await keyword is syntactical shorthand for indicating that a piece of code should asynchronously wait on some other piece of code. The `async` keyword represents a hint that you can use to mark methods as task-based asynchronous methods.  

For more details: [Using Asynchronous Methods in ASP.NET MVC 4][4]  

6) **Bundling & Minification.**

The bundling and minification framework enables you to reduce the number of HTTP requests that a Web page needs to make by combining individual files into a single, bundled file for scripts and CSS. It can then reduce the overall size of those requests by minifying the contents of the bundle. Minifying can include activities like eliminating whitespace to shortening variable names to even collapsing CSS selectors based on their semantics. Bundles are declared and configured in code and are easily referenced in views via helper methods which can generate either a single link to the bundle or, when debugging, multiple links to the individual contents of the bundle. 

For more details: [Bundling and Minification][5]


  [1]: http://i.stack.imgur.com/5YyYX.png
  [2]: http://i.stack.imgur.com/CnCP6.jpg
  [3]: http://www.asp.net/mvc/overview/older-versions/aspnet-mvc-4-mobile-features
  [4]: http://www.asp.net/mvc/overview/performance/using-asynchronous-methods-in-aspnet-mvc-4
  [5]: http://www.asp.net/mvc/overview/performance/bundling-and-minification


7. **Database Migrations**

ASP.NET MVC 4 projects now include Entity Framework 5. One of the great features in Entity Framework 5 is support for database migrations. This feature enables you to easily evolve your database schema using a code-focused migration while preserving the data in the database. For more information on database migrations, see Adding a New Field to the Movie Model and Table in the Introduction to ASP.NET MVC 4 tutorial.


## Installation or Setup
Detailed instructions on getting asp.net-mvc-4 set up or installed.

