---
title: "Getting started with biztalk"
slug: "getting-started-with-biztalk"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Software and hardware prerequisites for installation of BizTalk Server (All versions)

**Software requirements**
---------------------

 **1. Development environment**

   Below software tools are required for BizTalk environment, not all of them are needed, but having all might add some advantage for developing BizTalk applications and artifacts.

All installations

- Internet Information Services
- Microsoft SQL Server
- Microsoft .NET Framework
- SQLXML 4.0 with Service Pack 1
- WinSCP version 5.7.5 (New SFTP adapter in BizTalk 2016)
- Distributed Transaction Coordinator (included in standard OS installation)

Minimum developer tools

- Notepad++ (with XML tools plugin)
- Visual Studio
- Microsoft Excel (For BAM)
- Team Foundation Server or similar source repository

***Note:***
For a development installation the SQL Server Developer Edition is sufficient if  available for the version of BizTalk you want to install

For a production environment a dual server installation is recommended as minimum, an DB server and the BizTalk server

BizTalk supports high availability either through Clustering or Network Load Balancing depending on usage

**Minimum Hardware Requirements** (for development environment)
---------------------
**1. Processors**

 - 1 GHz or higher for single processors

 - 900 MHz or higher for double processors

 - 700 MHz or higher for quad processors

 
***Note:***
 Hyper-threading and dual-core processors are supported.
The 64-bit versions of BizTalk Server require a 64-bit operating system running on an x64-based system. Computers based on CPUs that are compatible with the AMD64 (x86-64) and Extended Memory 64-bit Technology (EM64T) processor architecture are considered x64-based systems.
It is recommended to install the 64-bit version of BiTalk to support large XML documents

**2. Memory** 

 - 2 GB of RAM

**3. Hard disk** 

 - 10 GB of available hard disk space in NTFS format.
- ***Note:***
Sufficient space for BizTalk database backup of transaction logs and databases is needed as only disk backup is currently supported

