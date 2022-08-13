---
title: "Getting started with umbraco"
slug: "getting-started-with-umbraco"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Install Umbraco with NuGet
**NuGet version**

Before you start: make sure your NuGet version is up to date.

In Visual Studio, go to Tools > Extensions and Updates, then Updates > Visual Studio Gallery. Check if there is a NuGet Update available and install it. Or, you can uninstall the existing nuget and reinstall it. It will install the latest version of nuget.

[![enter image description here][1]][1]


**.Net Version**

If you're installing Umbraco 7+ then you need to choose .NET Framework 4.5 or 4.5.1 here.
For Umbraco 6 you can still choose .NET Framework 4, but 4.5 and 4.5.1 also work.

You can select the version from the create new project window of visualstudio.

**Project**:

Create an empty asp.net web application. Then, right-click on the new project you just made and choose Manage NuGet Packages.

[![enter image description here][2]][2]

Find the "UmbracoCms" package, and click install. NuGet will then download dependencies and will install all of Umbraco's files in your new solution.

It will ask you to overwrite your web.config file. Confirm with yes. Build and Run solution (CTRL-SHIFT-B).

After the build a web browser will open, and you can continue installation in the web interface. You have to configure the database and select a template (if you want any template to start with).


> Above information are from umbraco official site.for more details go to [this link][3]


  [1]: http://i.stack.imgur.com/vo7Ov.png
  [2]: http://i.stack.imgur.com/BbOFb.png
  [3]: https://our.umbraco.org/documentation/getting-started/setup/install/install-umbraco-with-nuget

