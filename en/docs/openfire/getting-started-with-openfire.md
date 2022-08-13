---
title: "Getting started with openfire"
slug: "getting-started-with-openfire"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Openfire it's available to download at [Ignite Realtime website][1]
It's also possible to download relative source code[enter link description here][2]

**Windows installer:**

 - Just execute the exe and follow basic instructions as any program
   (installation directory, shortcut etc.).

**Unix-Linux-Mac install:**

 - Extract in an arbitrary folder or follow the basic instructions as
   any program

You can find detailed instructions on [official documentation][3]


  [1]: https://www.igniterealtime.org/downloads/index.jsp
  [2]: https://www.igniterealtime.org/downloads/source.jsp
  [3]: http://download.igniterealtime.org/openfire/docs/latest/documentation/install-guide.html

## Setup
Openfire setup it's based on a combination of xml configuration and database entries. After first installation, run Openfire will leads to deploy a web administration panel listening on port 9090. Endpoint to access it's typically on http://machinenamewhereinstalled:9090 and flagged on Openfire's log during startup.

Access to admin panel will land to web based wizard configuration.
It's possible to follow the wizard or avoid it to setup an already configurated Openfire server, as example with given domain, users and other configurations (to clone or move another installation).

**What you need before start wizard**

Openfire requires access to a database for data persistence, such like registered users and many other configurations. In Openfire folder, following the path `<Openfire_home>/database` is possible to find database's declined script to execute on an existent database.

Openfire will require to select a database driver and insert credential to access your database.

As alternative, it's possible to let Openfire create an embedded (but not ever comfortable to develop) [HSQLDB database][1]; more, if Openfire it's configurated with database credentials of admin grants, it will be able to lunch the database script itself during wizard transparently.

Keep in mind what is the scope the installer wants to achieve to obtain the right behaviour.

**Oracle notes:**

 - Oracle ODBC drivers have not a standard Opensource license, so
   cannot' be delivered with Openfire; to use download from [Oracle
   website][2] and copy before starting the wizard in 
   `<Openfire_Home>/lib` directory.
 - Oracle's script may needs updates depending by Oracle database target
   version, check file before execute it.

**What you need to avoid wizard** 

If is needed to avoid the wizard, it's necessary to:

 1. Configure properly **<Openfire_home>/conf/openfire.xml**
 2. Configure properly **<Openfire_home>/conf/security.xml**
 3. Export `ofProperty` table

**openfire.xml** and **security.xml**
To clone (and so "move") an installation, be sure to have the flag setup to **true** under the `jive` tag


    <setup>true</setup> 

To replicate an installation with same params (for example on multiple environment such like develop and maintenance) check out on database tag configuration and be sure about encryption key.
This is how looks like encrypted keys

      <database> 
        <defaultProvider> 
          <username encrypted="true">QXxDT1fOVuvrY=</username>  
          <password encrypted="true">QXxDT1fO1jnAiTGZq6u=</password>
        </defaultProvider> 
      </database> 

 And this how looks like uncrypted keys

      <database> 
        <defaultProvider> 
          <username>admin</username>  
          <password>password</password>
        </defaultProvider> 
      </database> 

`encrypted` attribute it's just a placeholder (encrypted="false" has no effect): username and password are considered encrypted if in `<Openfire_home>/conf/security.xml` there is the encrypt tag declared with entries as in the default behaviour like this one:

    <security> 
      <encrypt> 
        <!-- This can be set to "AES" or "Blowfish" (default) at setup time -->  
        <algorithm>AES</algorithm>  
        <key> 
        </key>  
        <property> 
          <!-- 
            This list includes the names of properties that have been marked for
            encryption. Any XML properties (from openfire.xml) that are listed here 
            will be encrypted automatically upon first use. Other properties 
            (already in the database) can be added to this list at runtime via the 
            "System Properties" page in the Openfire console.
          -->  
          <name>database.defaultProvider.username</name>  
          <name>database.defaultProvider.password</name> 
        </property> 
      </encrypt>  


Remove those 2 name entry to use plain password

      <encrypt> 
        <!-- This can be set to "AES" or "Blowfish" (default) at setup time -->  
        <algorithm>AES</algorithm>  
        <key> 
        </key>  
        <property> 
        </property> 
      </encrypt>

**ofProperty table**

`ofProperty` is a table that must be exported to clone the configuration between 2 Openfire servers. There are however 3 entries to focus on and maybe edited:

 - `xmpp.domain`  if the domain of new Openfire installation is not
   equal from source one 
 - `admin.authorizedJIDs` must be changed accordly    to update the jid
   of admins
 - `database.defaultProvider.serverURL` must    be changed if database
   changes.

Example of insert:


    Insert into OFPROPERTY  (NAME,PROPVALUE) values ('admin.authorizedJIDs','admin@environmentDomain');
    Insert into OFPROPERTY (NAME,PROPVALUE) values ('database.defaultProvider.serverURL','jdbc:oracle:thin:@...');
    Insert into OFPROPERTY (NAME,PROPVALUE) values ('xmpp.domain','environmentDomain');



  [1]: http://hsqldb.org/
  [2]: http://www.oracle.com/technetwork/database/windows/downloads/utilsoft-098155.html


