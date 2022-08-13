---
title: "Getting started with mysql-workbench"
slug: "getting-started-with-mysql-workbench"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
MySQL Workbench is available for all major operating systems -Windows, Linux, Mac-
You can find the version for you operating system from [here][1].

**For windows:** It uses the msi (Windows installer) to install packages. You only need to right click install and it starts.


**For Linux:** There are multiple .deb and .rpm packages for each distro available. For [ubuntu][2] installation through apt see here.

**For Mac:** Simply download the .dmg file. Then double-click the downloaded file.


  [1]: https://dev.mysql.com/downloads/workbench/
  [2]: https://askubuntu.com/questions/45115/how-to-install-mysql-workbench

## Common command-line arguments for MySQL Workbench
One can use the command-line launching facility is when one wants to customize some aspects of the way MySQL Workbench operates. 

MySQL Workbench has the following common command line options:

    --admin instance - Launch MySQL Workbench and load the server instance specified.

    --query connection - Launch MySQL Workbench and load the connection specified.

    --model modelfile - Launch MySQL Workbench and load the model specified.

    --script script - Launch MySQL Workbench and run the script specified.

    --run code - Launch MySQL Workbench and run the code snippet specified.

    --quit-when-done - quits MySQL Workbench after --script or --run finishes. 

