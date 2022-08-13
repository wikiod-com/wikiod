---
title: "Getting started with sitecore"
slug: "getting-started-with-sitecore"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Using SIM manager for installation
Sitecore Instance Manager is open-source tool which is used for managing the local park of Sitecore instances. You can install, locate, maintain, reinstal or delete Sitecore products. It also helps you install your sitecore instance with any sitecore packages, modules and only thing you need to do is set the folder for local repository where all the installations files are kept. See the below screenshot for settings.
[![enter image description here][1]][1]

It also has API and plugin engine so you can extend it for any of your need.

**For Installation click [here][2]**

**For More details about SIM please click [here][3]**


  [1]: http://i.stack.imgur.com/yITta.png
  [2]: http://dl.sitecore.net/updater/sim/
  [3]: https://github.com/Sitecore/Sitecore-Instance-Manager

## Installation or Setup
**Executable File**

Download the .exe from http://dev.sitecore.net/  and double-click to start. This .exe will do everything for you - attach databases, modify host file, and set folder permissions. The only downside is that it leaves an entry in the registry under [HKEY_LOCAL_MACHINE\SOFTWARE\Sitecore CMS] and makes your instance available under ‘Add/Remove Programs’, which is misleading. Sitecore is just an ASP.NET application, not a Desktop application.

**Manual Install**

You can choose to install and set up Sitecore manually by downloading the site root zip. You are responsible for attaching databases and ensuring that permissions are set up properly. You may wish to use this method in environments that require custom installation steps (databases are on another server, custom permissions needed). Follow the installation guide (available on http://dev.sitecore.net/; make sure you are reading the guide for your specific version) and - if you are installing on production - the Security Hardening Guide.

Once you become a certified Sitecore developer, you will be able to download the latest version of Sitecore and all associated modules from http://dev.sitecore.net/. On dev.sitecore.net, there are always two different formats available for download - the complete site root or an executable file. The following is a list of ways that you can install Sitecore.

**Step by Step**

 1. Download the Sitecore version you will from [the Sitecore website][1] - it's okay if you prefer the automatic installation method, but the following steps covers the manual process
 2. Unzip the archives to a folder on your computer. You'll see the following subfolders:
 - **Data** - This is where Sitecore keeps a number of archives related to the currently installed instance, such as logs, indexes, packages, and cache files
 - **Databases** - Database files from SQL Server and Oracle
 - **Website** - Everything that will be accessible from the web browser.
 3. Open IIS and create a new website linking to the folder "Website"
 7. Set the Application Pool of your website to use the .NET Framework to version 4.5
 8. Adjust your DataFolder path (at the web.config) - for Dev you can copy the Data folder to the Website folder 
 9. Copy the license.xml file to the Data folder 
 10. At the Website folder properties, uncheck the “read only” checkbox. In Dev you can and give full permissions to the Application Pool user. In production you must follow the instructions contained ([here][2]), under item “4.2 Configuring Folder and Registry Permissions”.
 11. Install all databases to the “Database” folder and edit the configurations in the ConnectionStrings.config file, located in the Website/App_Config folder
 12. Still at the ConnectionStrings.config, setup connection informations for all MongoDB entries (Those with connectionString="mongodb:...")
 13. If your site is on localhost, point your browser to http://localhost/sitecore. If you see a login screen, you have successfully installed Sitecore.

  [1]: https://dev.sitecore.net/Downloads.aspx
  [2]: https://dev.sitecore.net/~/media/Downloads/Sitecore_Experience_Platform/8_0/Sitecore_Experience_Platform_8_0/Non-secure/Installation-Guide-SC80-A4.ashx

> Source:
> http://sitecore-community.github.io/docs/sitecore-basics/installing-sitecore/

