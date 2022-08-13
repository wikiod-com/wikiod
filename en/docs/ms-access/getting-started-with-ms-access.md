---
title: "Getting started with ms-access"
slug: "getting-started-with-ms-access"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What is MS-Access and what do we use it for ?
Microsoft Access is an **Application Generator** for developing databases and data-driven applications, primarily for local use. Microsoft Access consists of two main elements:

 1. A **Relational Database Management System** (**RDBMS**) that combines the [Microsoft Jet Database Engine][1] (Access 2003 and earler) or the Access Database Engine (Access 2007 and later; see below) with graphical management tools. A unique **Linked Tables** system allows remote tables to be treated as local.
 2. **Graphical User Interface** (**GUI**), and **software development** tools, supported by [Visual Basic for Applications][2] (**VBA**) that can reference a variety of objects.  

It is a member of the **Microsoft Office** suite of applications, included in the Professional and higher editions or sold separately. Database applications that have been created with a full version of Microsoft Access can be [compiled for distribution and run via a free Microsoft Access Runtime][3].

The two elements allow Microsoft Access to be used in various ways:

 - **As a Database**: A Microsoft Access database does not call for a ***Data-Server***, and is often used as a database for local applications, such as a **web-site database**, located on the web-server.
 - **As a Data Application Generator**: Tools for creating GUIs containing Forms and Controls bound to (*local* or *linked*) tables allow developers to create local applications for accessing and managing local or remote data. VBA modules allow developers to create capabilities not supported by GUI tools.
 - **As a full Application Generator**: The above abilities allow developers to create full local data applications in one or more Access files.

## Microsoft Access Database Engines

Through Access 2003 (11.0), the built-in database engine was [Microsoft Jet][1]. With Access 2007 (12.0), Microsoft introduced a new descendant of the Jet engine, the Access Database Engine (originally called the Access Connectivity Engine and still commonly known as the **ACE Engine**), and made it the default for new databases. Its feature set and behavior overlaps incompletely with the last version of Jet (4.0). Versions of Access released since have been able to create and work with databases in either Jet (`.mdb`) or ACE (`.accdb`) format, even though [Jet has been officially deprecated][4] as a technology.

  [1]: https://en.wikipedia.org/wiki/Microsoft_Jet_Database_Engine
  [2]: https://www.wikiod.com/vba/getting-started-with-vba
  [3]: http://www.fmsinc.com/MicrosoftAccess/runtime/index.htm
  [4]: https://msdn.microsoft.com/en-us/library/ms810810.aspx

## Installation or Setup
Microsoft Access is one of the Microsoft Office suite of programs. However, it is only available in some packages of MS Office. 

If you wish to obtain Access, please make sure to carefully examine the box or download specifications for each version of Microsoft Office. MS Access is **only available for Windows PCs**, it is not available on Macintosh systems in the native environment, even through other MS Office programs may be available. Similarly, it is not available for linux operating systems.

In Office 365, Access can be found in the Home, Personal, ProPlus, Enterprise E3 or E5 versions, but **not Enterprise E1 nor Business** (or B. Essentials, B. Premium). 

In Office 2016 it is **not included in the Home & Student or Home & Buisness packages**, but it is in Professional. It does not appear to be in any versions for the Macintosh.

## Versions
Microsoft Access has existed since 1992, and older versions continue to see regular use when business-critical database applications have been built on them. A very comprehensive resource summarizing the release history (with links to release notes, where available) is:

- [Microsoft Access Version Releases, Service Packs, Hotfixes, and Updates History][1]

  [1]: http://www.fmsinc.com/MicrosoftAccess/history/versions.htm

