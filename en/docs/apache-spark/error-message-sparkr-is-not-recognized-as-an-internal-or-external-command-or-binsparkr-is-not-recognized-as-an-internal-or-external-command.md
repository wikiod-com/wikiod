---
title: "Error message 'sparkR' is not recognized as an internal or external command or '.binsparkR' is not recognized as an internal or external command"
slug: "error-message-sparkr-is-not-recognized-as-an-internal-or-external-command-or-binsparkr-is-not-recognized-as-an-internal-or-external-command"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This post is for those who were having trouble installing Spark in their windows machine. Mostly using sparkR function for R session.

Used reference from r-bloggers 

## details for set up Spark for R
Use below URL to get steps for download and install- 
https://www.r-bloggers.com/installing-and-starting-sparkr-locally-on-windows-os-and-rstudio-2/
Add the environment variable path for your 'Spark/bin', 'spark/bin' , R and Rstudio path.
I have added below path (initials will vary based on where you have downloaded files)
C:\spark-2.0.1
C:\spark-2.0.1\bin
C:\spark-2.0.1\sbin
C:\Program Files\R\R-3.3.1\bin\x64
C:\Program Files\RStudio\bin\x64

To set the environment variable please follow below steps:
Windows 10 and Windows 8
In Search, search for and then select: System (Control Panel)
Click the Advanced system settings link.
Click on Advanced tab under Sytem Properties
Click Environment Variables. In the section System Variables, find the PATH environment variable and select it. 
Click Edit. If the PATH environment variable does not exist, click New.
In the Edit System Variable (or New System Variable) window, specify the value of the PATH environment variable. 
Click OK. Close all remaining windows by clicking OK.
Reopen Command prompt window, and run sparkR (no need to change directory).

Windows 7
From the desktop, right click the Computer icon.
Choose Properties from the context menu.
Click the Advanced system settings link.
Click Environment Variables. In the section System Variables, find the PATH environment variable and select it. 
Click Edit. If the PATH environment variable does not exist, click New.
In the Edit System Variable (or New System Variable) window, specify the value of the PATH environment variable.
Click OK. Close all remaining windows by clicking OK.
Reopen Command prompt window, and run sparkR (no need to change directory). 


