---
title: "Getting started with sas"
slug: "getting-started-with-sas"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
SAS can be run in client-server model, using either the Enterprise Guide thick client or the SAS Studio thin (web-enabled) client, or in "local server" mode where a fully functional SAS system is present on a local machine (Windows or Unix/Linux desktop or server running in interactive mode) and run either in Display Manager mode (the local client) or through one of the client-server clients above (connected to the locally installed server).

SAS installation typically is performed by a SAS administrator, who will install the software from a software depot that is customized for the site (and often provided by SAS Institute directly).

For the purpose of learning SAS, there is also the free SAS University Edition, which can be installed for free for educational purposes by anyone on a Windows, Mac, or Unix/Linux computer.  It is available from SAS directly, currently at the [SAS University Edition page](http://www.sas.com/en_us/software/university-edition.html), either by running an AWS instance (on the free tier) or by downloading a virtual machine locally.  See [the installation guide on SAS.com](http://www.sas.com/en_us/software/university-edition/download-software.html) for up to date instructions, or below for the current (July 2016) instructions.

To install it locally, you first download and install Oracle Virtualbox 5.0 [(Windows/Mac/Linux)](https://www.virtualbox.org/wiki/Download_Old_Builds_5_0).   Then download the newest [SAS University Edition disk image](http://www.sas.com/store/expresscheckout.ep?item=DPUNVE001_VirtualBox), which is around 2GB and requires setting up a SAS.com profile.

Once you've done so, you need to set up the virtual machine in VirtualBox.  Import the SAS VM as an appliance ("Import Appliance" in VirtualBox).  Create a folder for SAS to use as its local storage (so you can put files in a location SAS can see them), and set that up as a Shared Folder in the machine settings dialog box.  Set it up to auto-mount.

Then, start the SAS virtual machine, and once it's started you can connect via your web browser, connecting to http://localhost:10080/ if you used the default settings.

If you have issues, the [SAS Community Forums - Analytics U](https://communities.sas.com/t5/SAS-Analytics-U/bd-p/sas_analytics_u) are the vendor forums to obtain support, or ask a question on [Stack Overflow](https://stackoverflow.com).

## Overview of Base SAS
SAS is an integrated system of software solutions that enables you to perform the following tasks:
 - data entry, retrieval, and management
 - report writing and graphics design
 - statistical and mathematical analysis
 - business forecasting and decision support
 - operations research and project management
 - applications development

How you use SAS depends on what you want to accomplish. Some people use many of the capabilities of the SAS System, and others use only a few.


## Hello World!
Due to the stucture of SAS, there are three main ways to create "Hello World!" examples: 

  1. Within a data step to put a message into the SAS log (`_null_` denotes that no output dataset should be created):

    data _null_;
        put "Hell" "o World!";
    run;

  2. Within a data step to store "Hello World!" within a variable (`foo` denotes that an output dataset called `foo` should be created that a) contains only one record and b) contains only one variable: `bar`, which has a value of `Hello World!`):

    data foo ;
        bar="Hello" ;
        put bar= "World!";
    run ;

  3. Via the SAS Macro language (in 'open code' outside of any data steps). `&` identifies a call to a macro variable and `.` identifies the end of the variable (if a white space character is not wanted):

    %let foo=Hello;
    %put &foo.o World!;

  4. Hybrid: Using a macro variable within a data step:

    %let foo=Hello;

    data _null_ ;
      put "&foo World!";
    run ;


## SAS Server Architecture
**Overview**: There are typically two types of SAS Deployments: 

1. SAS Foundation only installation (BASE SAS). This is typically is installed on a PC. It does not run any server software. 

2. SAS Planned Deployment for their server architecture which will install the SAS server environment along with possibly any SAS client software. 

Which one of these you have will be indicated in your SAS Software Order email by indicating planning or non-planning. If you are doing a planned installation you will need a plan file for your order that first your topology. 

[Installation Note 44320: Using deployment plans during a SAS® installation][1]


**SAS Server Architecture**

The SAS server environment is broken into 3 different tiers: 

1. SAS Metadata Server(s) - The SAS Metadata server is responsible for managing the SAS server environment including libraries, users, and server configuration.

**2. SAS Application Server(s)** - The SAS Application Server is mostly a compute server where your clients would typically launch jobs from.


**3. SAS Middle Tier (s)** = The SAS Middle Tier is primarily your Web tier which runs your web applications.

**4. Client Tier** - The client tier is your users client applications they use to connect to the environment such as SAS Enterprise Guide.

[Paper 363-2011| Understanding the Anatomy of a SAS® Deployment: What's in My Server Soup? Mark Schneider, Donna Bennett, and Connie Robison, SAS Institute Inc., Cary, NC][2]

**Topology:** 

The SAS Metadata Tier, SAS Application Server tier, and SAS Middle Tier can be installed on a single machine server, or spread out across multiple servers. This is determined by the plan file you have, it should meet the desired topology for your deployment. 

Typically most, if not all of the client tier are Windows based applications, so the client tier would be on your SAS users workstations. Optionally they could probably be installed on the server(s) as well if they are Windows based.

[SAS Supported Operating Systems][3]


  [1]: http://support.sas.com/kb/44/320.html
  [2]: http://support.sas.com/resources/papers/proceedings11/363-2011.pdf
  [3]: http://support.sas.com/supportos/list

## Versioning
The main current versions of SAS are 9.4 and 9.3 these are the versions of the base SAS engine most commonly used today. The link to release notes for versions 9.1 + and other related documentation are included below. 

***Please note also, there are various packages and functions which extend the functionality of SAS, and these have their own self standing documentation and functionality*.**

 - [SAS 9.4 - Key Documentation and Release Notes][1] 
 - [SAS 9.3 - Key Documentation and Release Notes][2] 
 - [SAS 9.2 - Key Documentation and Release Notes][3] 
 - [SAS 9.1.x - Key Documentation and Release Notes][4]

  [1]: https://support.sas.com/documentation/onlinedoc/base/index.html#base94
  [2]: https://support.sas.com/documentation/onlinedoc/base/index.html#base93
  [3]: https://support.sas.com/documentation/onlinedoc/base/index.html#base92
  [4]: https://support.sas.com/documentation/onlinedoc/base/index.html#base913

