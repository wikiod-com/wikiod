---
title: "Getting started with asp.net-identity"
slug: "getting-started-with-aspnet-identity"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting asp.net-identity set up or installed.

## ASP.NET Identity Basic information
ASP.NET identity is a membership management system which allows a user to register and login into a web application. ASP.NET identity system can be used in entire ASP.NET framework, like ASP.NET MVC, Web Forms, Web Pages, Web API and SignalR. ASP.NET identity can be used when people are building a web application, mobile application, store application and even in a hybrid application.

ASP.NET identity system also support external login providers like Microsoft Account, Facebook, Google, Twitter and others. Developer have to just provide your API key and API secret for your external login provider and "It just works".

To install ASP.NET identity service in your application add the NuGet package from Nuget Package Manager or from Package Manager Console.

Package Name : [Microsoft.AspNet.Identity.Core][1]

Now, Create a new Web Application in visual studio with individual User Accounts.

In visual studio, select **File -> New -> Project** then from left pane select Web and then from right pane select ASP.NET Web Application.

[![New Project][2]][2]

Click ok and from next window select MVC and then click ok.

[![Select Project][3]][3]

After project created successfully check out the references in solution explorer. You will find three Nuget package reference as shown in below image.

[![References][4]][4]

Now run the application and register an account to application and login.

To explore the login functionality open **Startup.Auth.cs** from solution explorer to get basic information about how login and register mechanism works.

As ASP.NET Identity is a very large topic so we can describe it only in basic information. For more information you can go to ASP.NET [Identity][5] website.


  [1]: https://www.nuget.org/packages/Microsoft.AspNet.Identity.Core/
  [2]: http://i.stack.imgur.com/cDuTJ.png
  [3]: http://i.stack.imgur.com/o520X.png
  [4]: http://i.stack.imgur.com/zDyza.png
  [5]: https://www.asp.net/identity

