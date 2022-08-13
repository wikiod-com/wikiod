---
title: "Getting started with tfs2015"
slug: "getting-started-with-tfs2015"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Requirements of Team Foundation Server 2015
Team Foundation Server (TFS) can be installed in two basic infrastructure ways, Single Server Deployment and Multi-Server Deployment.  The difference between these two deployments is where the Application Tier (TFS) and the Data Tier (MS SQL) reside.

*It is considered best practice to install TFS in a Multi-Server Deployment.*  

Before you can install TFS 2015, you must make sure you have met the requirements for both hardware and software.

*According to Microsoft Documentation, TFS 2015 does support 32-bit hardware but it is highly recommended you install on 64-bit as TFS 2017 **ONLY** support 64-bit.*

Single Server Deployment:
 - 1 Dual-Core Processor
 - 4 to 8 GB of RAM

Multi-Server Deployment:
 - Application Tier
   - 1 Dual-Core Processor
   - 8 to 16 GB of RAM (or more)
   - High-performance storage such as an SSD
 - Data Tier
   - 1 - 2 Quad-Core Processor
   - 8 to 16 GB of RAM (or more)
   - High-performance storage such as an SSD

Operating System Version Support:

 - Windows Server 2016
 - Windows Server 2012 R2 (Essentials, Standard,
   Datacenter)
 - Windows Server 2012 (Essentials, Standard, Datacenter)
 - Windows Server 2008 R2 (minimum SP1) (Standard, Enterprise,
   Datacenter)

Microsoft SQL Server Version Support:

*TFS Supports Express, Standard, and Enterprise editions but it is recommended that you use Standard as the bare minimum.*
 - TFS 2015 Update 3
   - SQL Server 2016
   - SQL Server 2014
   - SQL Server 2012 (*minimum SP1*)
 - TFS 2015 (*Recommend you get Update 3 ASAP*)
   - SQL Server 2014
   - SQL Server 2012 (minimum SP1)

The following are the required components of Microsoft SQL needed:
 - Required for TFS
   - Database Engine Services 
   - Full-Text and Semantic Extractions for Search
 - Required for reporting
   - Reporting Services – Native 
   - Analysis Services

Microsoft SharePoint Versions Support:
 - SharePoint 2013 (Foundation, Standard, Enterprise)
 - SharePoint 2010 (Foundation, Standard, Enterprise)

