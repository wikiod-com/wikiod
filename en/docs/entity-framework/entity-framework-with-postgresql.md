---
title: "Entity-Framework  with Postgresql"
slug: "entity-framework--with-postgresql"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Pre-Steps needed in order to use Entity Framework 6.1.3 with PostgresSql using Npgsqlddexprovider
1)Took backup of Machine.config from locations C:\Windows\Microsoft.NET\Framework\v4.0.30319\Config and C:\Windows\Microsoft.NET\Framework64\v4.0.30319\Config

2)Copy them to different location and edit them as 
         

> a)locate and add under `<system.data> <DbProviderFactories>`

                <add name="Npgsql Data Provider" invariant="Npgsql" support="FF"
                description=".Net Framework Data Provider for Postgresql Server"
                type="Npgsql.NpgsqlFactory, Npgsql, Version=2.2.5.0, Culture=neutral, PublicKeyToken=5d8b90d52f46fda7" />


        

> b)if already exist above entry, check verison and update it.

3) Replace original files with changed ones.
4) run Developer Command Prompt for VS2013 as Administrator.
5) if Npgsql already installed use command " gacutil -u Npgsql " to uninstall then install new version of Npgsql 2.5.0 by command " gacutil -i [path of dll] "
6) Do above for Mono.Security 4.0.0.0
7) Download NpgsqlDdexProvider-2.2.0-VS2013.zip and run NpgsqlDdexProvider.vsix from    it(Do close all instances of visual studio)
8) Found EFTools6.1.3-beta1ForVS2013.msi and run it.
9) After crating new project, Install version of EntityFramework(6.1.3), NpgSql(2.5.0) and NpgSql.EntityFramework(2.5.0) from Manage Nuget Packages.10)Its Done go ahead...Add new Entity Data Model in your MVc project


