---
title: "Installation of Umbraco CMS"
slug: "installation-of-umbraco-cms"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Hosting enviroment
For Umbraco 7 the requirements are
* IIS 7 or higher 
* Database, one of the following: SQL CE, SQL Server 2008 or higher or MySQL with support for  case insensitive queries)
* ASP.NET 4.5 or 4.5.1. Full-Trust
* Ability to set file/folder permissions for the user that "owns" the Application Pool

## Manual installation of Umbraco
1. Download the files from [our.umbraco.org/download][1] and unzip them in a folder. 
2. Set up web server hosting the folder you chose. Although IIS is a requirement for production sites, it runs fine for development in IIS Express. 
3. Set up file permissions for the folder. For development server it is okey to have full read/write/modify permissions for the process serving the web page (AppPool or Network service)
4. If you want to set up Umbraco on Sql Server or Mysql, download and install it. Make a new database for Umbraco, and remember Connectionstring details, inccluding database name, user and password
5. Open your site at localhost, and follow the wizard to set up your site with the database. 






  [1]: http://our.umbraco.org/download

## Install Umbraco with NuGet
 1. Check for updates to the Nuget package manager. In Visual Studio: Tools > Extensions and Updates > Updates > Visual Studio Gallery. Install if availalbe
 2. Create a new web application with template "ASP.NET Web Application with an Empty template" on .NET Framework 4.5.1
 3. Open the package manager console and run `Install-Package UmbracoCms`
 4. Press F5 to build and run your new website. 
 5. Complete the wizard to choose database provider and set up your site.


## Use Umbraco as a cloud service
Either you just want to test out Umbraco CMS, or host your site in a cloud service, you could sign up for a free trial at [umbraco.com/cloud][1]. The site you develop in the cloud service could be downloaded for local development or your own hosting later. 

  [1]: https://umbraco.com/cloud

