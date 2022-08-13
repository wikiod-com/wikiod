---
title: "Getting started with firebird"
slug: "getting-started-with-firebird"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Download
--------

Use Firebird site to [download][1] the correct "[server package][2]" for your system.
First, select the version of Firebird that you would like to install.
Next, select the appropriated installer for your system. Example, for almost any version of Windows 32 bits, you would select under 32-bit kits the option with "Windows executable installer recommended for first-time users".

Installing
----------
Execute the installer and follow instructions. For first-time users, you probably will not need to change any configuration on installer.    


  [1]: http://www.firebirdsql.org/en/downloads/
  [2]: http://www.firebirdsql.org/en/server-packages/

## What is ODS version and how to retrieve it?
ODS (on-disk structure) version is a number representing version of the database low-level data layout structure (ODS). When a new feature is added to Firebird it might or might not require the structure of database pages or system tables (database metadata) to change. If it does, the ODS version must increase.

This number is checked upon connection, so that server makes sure it can 'understand' the database structure. For example, when you try to connect with a 1.0 server to a database created with Firebird 2.0, you'll get an error as the 1.0 server is not able to handle that ODS - simply because there are fields whose meaning it does not understand.

Firebird 2.5 can open databases with ODS of Interbase 5, Interbase 6, and Firebird 0.9 to 2.5. However Firebird 3.0 was a clean start in backward compatibility regard and it can no more open databases with ODS versions of prior Firebird releases.

The ODS version, as reported by user tools, shows with which server version the database was created, e.g.: 

    -------------------------------------------------------------
    Database created with version:                    ODS version:
    InterBase® 5                                                9
    InterBase® 5.5, 5.6                                        9.1
    InterBase® 6    /   Firebird 1.0                          10.0
    InterBase® 6.5  /   Firebird 1.5                          10.1
    InterBase® 7    /   Firebird 2.0                            11
    InterBase® 7.1  /   Firebird 2.1                          11.1
    InterBase® 7.5  /   Firebird 2.5                          11.2
    InterBase® 2007 /   Firebird 3.0                            12
    InterBase® 2009                                             13
    InterBase® XE                                             15.0

NOTE 1: When the same ODS version reported for some Interbase and Firebird versions that does NOT mean the very ODS is the same thus it does NOT mean compatibility across IB/FB boundary! Except for Firebird 0.9 and 1.0 and Interbase 6.0 which were almost compatible. Back then it was expected Interbase be kept opensource and re-use Firebird project code. However, with Interbase 6.5 it changed. What that practically means here, is while some Interbase/Yaffil/Firebird databases may report having the same ODS version (number), the very structure (ODS itself) of them was getting more and more different. IB 7 would not open FB 2 database and vice versa - they have different internal formats (ODS), while both now separate projects gave them the same version number. The ODS version may be the same between some IB and FB versions, but the ODS itself (except IB6.0) would be not!

NOTE 2: with Firebird version 1.5 there was 64-bit version of the server introduced. Databases created with 64-bit and with 32-bit builds of Firebird 1.5 are both reporting ODS version 10.1, but their actual ODSes are a bit different and they can not open databases of one another. Starting with FB 2.0 that was fixed and both x86 and x64 builds of Firebird Server can open databases created by one another.


To retrieve ODS version you can use the Firebird API, or simply use the tool that reads it for you.

If you only have command-line access you can use Firebird's gstat command line tool (located in bin directory). Its option -h outputs the header page information, which contains the ODS:

    gstat –h database_file_name

User and password here unnecessary, because gstat with –h option just read physical part of the database (header page, number 0).

If gstat will not understand read information, it will show corresponding message – what it expected, and what it found.

If you only have remote connection to the server and you can log into the database, but you have no access to the database file itself, then starting with Firebird 2.1 you can also query ODS by regular SQL commands using Monitoring Tables.

         select MON$ODS_MAJOR, MON$ODS_MINOR from MON$DATABASE



Example of use command prompt: 

[![enter image description here][1]][1]

Example of use "Database Properties" in:

 [FlameRobin](http://www.flamerobin.org/):

[![enter image description here][2]][2]

IbExpert: 

[![enter image description here][3]][3] 




  [1]: http://i.stack.imgur.com/DPU5C.png
  [2]: http://i.stack.imgur.com/1hvBl.png
  [3]: http://i.stack.imgur.com/MPo5a.png

