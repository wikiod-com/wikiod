---
title: "Getting started with sharepoint"
slug: "getting-started-with-sharepoint"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of SharePoint 2016 for Single Server Farm
Introduction
============
SharePoint 2016 is the version 16 release of the SharePoint product family. It was released on May 4, 2016. This example covers the installation of SharePoint 2016 using the Single Server Farm configuration. This configuration covers the basics of setting up a SharePoint farm without the need to have multiple servers. Note that the covered scenarios by a Single Server Farm are usually limited to development and very small production scenarios.

Requirements
============
Prior to installing SharePoint, the basic environment must be set up. SharePoint stores documents as well as metadata, logs, custom applications, customizations, and much more. Ensure that you have sufficient disk space and RAM available above the base line requirements.

* 4 Cores on a 64-bit compatible processors
* 12 - 24 GB of RAM (depending on test or prod deployment)
* 80GB hard drive for system
* 100GB hard drive as second drive
* Server with 64-bit Windows Server 2012 R2 or Technical Preview “Threshold”
* SQL Server 2014 or SQL Server 2016
* .NET Framework 4.5.2 or .NET Framework 4.6
* Domain joined computer and delegated farm service accounts

All other prerequisites can be installed manually or done using the SharePoint Prerequisite installer included with the SharePoint installation.

Installation
============
* Run the prerequisites installer; it may request to reboot the server before continuing
* Run Setup.exe from the SharePoint installation
* Enter the license key
* Accept the license agreement
* Select "Complete" on the Server Type Tab
* Setup should complete successfully
* On the complete page, leave the check box checked next to the Run Product Configuration Wizard and click Close

Configuration
=============
If you are continuing from the previous step the SharePoint 2016 Product Configuration Wizard should open automatically. if the box does not appear or you are running the configuration later, open the configuration wizard by going to Start -> SharePoint 2016 Products -> SharePoint 2016 Product Configuration Wizard.

* Click next on the welcome page
* A modal dialog will pop up saying some services my be restarted during the configuration; nothing has been installed yet, so click yes
* Add the database server for the farm
    * Enter the name of the machine running SQL Server; in this case, it is the local machine
     * Enter the name of the Configuration database or keep the default name SharePoint_Config
    * Enter the username of the domain service user who will be accessing the database (in the form of DOMAIN\user)
    *Enter the password for the domain user
    * Click next when done
* Enter the farm password; this will be used when joining additional servers to the new farm
* Select the Single Server Farm role
* Configure the Central Admin Web App (where SharePoint will be managed from by the farm administrators) select the port number and select the type of authentication federation (NTLM or Negotate (Kerberose))
* Review the settings on the final pages and make changes as necessary
* When ready, run the configuration which may take a few minutes
* On completion, you will open the wizard will allow you to open the Central Admin site
* On failure, you can investigate the logs in the %COMMONPROGRAMFILES%\Microsoft Shared\Web Server Extensions\16\LOG folder

Farm Configuration
==================
Once the central web app, config database, and central admin are set up, you will be ready to configure the farm for use for users or development. You can bookmark the location of the Central Admin site or access it through a shortcut in the same location as the Product Configuration Wizard. 

* If you are starting the configuration later, click on Quick Launch -> Configuration Wizards -> Farm Configuration Wizard
* If you are starting the Wizard from the installation step, click Start the Wizard
* Choose if you want to be part of the customer improvement program by clicking Yes or No
* On the farm configuration page, select the domain account that will run background services on the farm
    * While this account may be the same as the database account, they may also be different for separation of roles and privileges
    * Enter the account as DOMAIN\user
* Validate the services you want available on the farm on the Services page
* Create the first site collection on the farm (this step can be skipped and done at a later time)
    * Enter the site collection's title, description, web address (usually the first site is at the server root), and the template
    * Most things can be changed (title, description) can be changed easily, but others like the web URL may take much more work to change; the template can also not be easily rolled back, but SharePoint allows a large amount of customizations that allows you to take any base template and convert the style and layout of the site
* When you are complete with the configuration, click finish

The farm and the first site collection are now configured for use.

## Build a web part with the SharePoint Framework
[dev.office.com/sharepoint][1] is a great place to get started with the SharePoint Framework.

The SharePoint Framework is a modern, client side approach to SharePoint Development initially targeted at SharePoint Online in Office 365. Web parts created with the SharePoint Framework are a new type of web part and they can be made available to add on both existing SharePoint pages and new SharePoint pages.

There's a great hello world example for this process hosted at [Build your first SharePoint client-side web part (Hello World part 1)][2]. All of the examples at dev.office.com are available for community contributions through github.

The basic steps of Hello World in the SharePoint Framework are:

 1. Generate the skeleton of the project with [the Yeoman SharePoint Generator][3].

    yo @microsoft/SharePoint

 2. Edit the generated code in the editor of your choice. Support for [Visual Studio Code][4] is strong across platforms.

 3. Preview the web part using gulp and the local SharePoint Workbench

    gulp serve

 4. Preview in your SharePoint Online environment

> Go to the following URL: 'https://your-sharepoint-site/_layouts/workbench.aspx'

  [1]: https://dev.office.com/sharepoint
  [2]: https://dev.office.com/sharepoint/docs/spfx/web-parts/get-started/build-a-hello-world-web-part
  [3]: https://www.npmjs.com/package/@microsoft/generator-sharepoint
  [4]: https://code.visualstudio.com/

## SharePoint ULS Logs and Logging
The SharePoint Unified Logging Service (ULS) provides support and debugging capabilities for both ops and developers. Understanding how to read the logs is an important first step to resolving issues.

# Tooling
Microsoft provides the [ULS Viewer][1] to help read old logs and logs that are currently being written to as the farm is running. It can also filter and apply formatting to logs to help narrow down a problem.

# Correlation Identifier
To isolate an issue, it is helpful to only look at a particular correlation id. Each correlation id is associated with a request or end to end action of the system (such as a time jobber). If there is a problem with a web page being rendered, locating the request in the ULS logs and isolating it to the specific correlation id removes all the noise from the other logs, helping to pinpoint the problem.

# Adding SPMonitoredScope to My Code
One way to figure add logging and some performance monitoring is to add SPMonitoredScope to your code. 

    using (new SPMonitoredScope("Feature Monitor"))
    {
        // My code here 
    }

This code will log the beginning and end of your requests as well as some performance data. Building your own custom monitor that implements ISPScopedPerformanceMonitor allows you to set the trace level or maximum execution time for a set of code.

  [1]: https://www.microsoft.com/en-us/download/details.aspx?id=44020

