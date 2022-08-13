---
title: "Getting started with Nexus Repository Manager"
slug: "getting-started-with-nexus-repository-manager"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Prerequisites
To get Nexus Repository Manager 2 running so you can use it with Maven, you need to run through a few steps before you install the repository manager itself.

* Download and install the Oracle JRE, version 7 or newer (suggest using version 8, because, duh). It can be obtained at the following link: http://www.oracle.com/technetwork/java/javase/downloads/index.html
* Download Apache Maven from the following link: http://maven.apache.org/download.cgi
* Download Nexus Repository Manager 2 from the following link: https://www.sonatype.com/download-oss-sonatype

Once you've done this, you should be ready to move on and install Repository Manager itself.

## Installation on Linux
Once you've met the prerequisites for installing Nexus Repository Manager 3, you'll need to follow a few steps to get it running on Linux.

* Open a terminal 
* Copy the package to a sane place `sudo cp nexus-3.1.0-04-unix.tar.gz /usr/local`
* `cd /usr/local`
* Untar the Nexus Repository Manager package via a similar command: 
`sudo tar xvzf nexus-3.1.0-04-unix.tar.gz`
* Create a symlink for ease of use `sudo ln -s nexus-3.1.0-04 nexus`

Once you've done these steps you can run Nexus Repository Manager via the following command

`./bin/nexus run`

You should see a lot of logs come up, and you should be able to access Nexus Repository Manager via the following URL: http://localhost:8081

Voila!

## Installation on Windows
Once you've met the prerequisites for installing Nexus Repository Manager 3, you'll need to follow a few steps to get it running on Windows.
 
* Unzip the package, e.g. `7za.exe e nexus-3.1.0-04-win64.zip`
* Copy the package to a sane place, likely `C:\nexus`
 
Assuming you are using the command prompt and in the C:\nexus (or whatever directory you chose), once you've done these steps you can run Nexus Repository Manager via the following commands
 
* `cd bin`
* `nexus.bat`
 
You should see a lot of logs come up, and you should be able to access Nexus Repository Manager via the following URL: http://localhost:8081
 
Voila!

