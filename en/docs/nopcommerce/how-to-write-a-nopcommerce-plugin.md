---
title: "How to write a nopCommerce plugin"
slug: "how-to-write-a-nopcommerce-plugin"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Every new functionality in NopCommerce comes in as a plugin.  

This documentation helps you create plugin that is basic in nature but teaches how a plugin is made for NopCommerce.

Since NopCommerce is an Open Source project, there are many plugins available for reference which are very helpful.

This is just an example to create a sample plugin.

You could enhance the functionalities of this plugin or add a totally new plugin.

Hope this helped.

Thank you!
---

## Create a Plugin project
1. Get the source code from the NopCommerce website and open it in Visual Studio

2. In the plugins folder of NopCommerce solution, add a project of type class library with the plugin name prefixed by Nop.Plugin like `Nop.Pugin.CategoryName.PluginName` as the name. Then build the solution.
3. Delete the class.cs file which is added automatically to the project.
4. Add the references to `Nop.Core.dll`, `Nop.Data.dll`, `Nop.Services.dll`, `Nop.Web.Framework.dll`, `System.Web.Mvc.dll`, `Autofac.dll`, `Autofac.Integration.Mvc.dll`, `EntityFramework.dll`, `FluentValidation.dll` and others which are necessary for the plugin.

5. Create a text file `Description.txt` inside the new folder and copy the content from any other plugin and edit according to your Plugin data. This file is necessary for a plugin to work.

    An example `Description.txt` is here.

    Group: Plugin category name here  
    FriendlyName: Plugin Name here  
    SystemName: CategoryName.PluginName  
    Version: 1.0  
    SupportedVersions: 3.80  
    Author: Your Name  
    DisplayOrder: 1  
    FileName: Nop.Plugin.CategoryName.PluginName.dll  
    Description: This shows up in the Admin area as the plugin's description

6. Copy the web.config file from any other plugin to this plugin and don't alter anything

7. Change the build location from bin in plugin folder(default) to `.\Presentation\Nop.Web\Plugins\CategoryName.PluginName\`. This is where NoCommerce looks for plugins when loaded and the Configuration to `All configurations`.

8. Clean and build the solution

---



