---
title: "Getting started with solr"
slug: "getting-started-with-solr"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing SOLR
## Introduction ##
The following procedure was tested on a test instance in AWS, with Redhat and `Solr 6.1.0`. You may need to adjust the process to your operating system and environment accordingly.

## Prerequisites

 1. Make sure you use RedHat or a similar (Fedora-based) OS.  
`cat /etc/redhat-release`  
displays your OS version.
[![enter image description here][1]][1]
 2. Check if java 1.6 or higher is installed  
`which java`
[![enter image description here][2]][2]
 3. Install Java if necessary  
`sudo yum list available java*`  
[![enter image description here][3]][3]  
`sudo yum install java-1.8.0-openjdk.x86_64`

 4. Check if it is installed correctly  
`which java` displays the Java home  
`java -version` displays the Java version 
[![enter image description here][4]][4]
 5. Create a SOLR user.  
`sudo adduser solr`

 6. Add a password for the user.  
`sudo passwd solr`
[![enter image description here][5]][5]
 7. Enable sudo on the SOLR user, run visudo  
`sudo visudo`

 8. Find the following lines:  
`## Allows people in group wheel to run all commands`  
`# %wheel        ALL=(ALL)       ALL`

 9. If `%wheel` is commented out, uncomment the second line by removing the # character.  
`%wheel        ALL=(ALL)       ALL`
[![enter image description here][6]][6]
 10. If you made a change use `:wq` otherwise use `:q` to quit. 

 11. Add the solr user to the wheel group.  
`sudo usermod -aG wheel solr`

 12. Switch over to the `solr` user an check if you have root privileges:  
`su solr -`  
`sudo whoami`
[![enter image description here][7]][7] 
## Downloading SOLR ## 

 13. Find your local mirror at: `http://www.apache.org/dyn/closer.lua/lucene/solr/`

 14. Change your directory location to opt:  
`cd /opt/`

 14. Download a copy of the package from the mirror:  
`sudo curl -O http://www.trieuvan.com/apache/lucene/solr/6.1.0/solr-6.1.0.tgz`
[![enter image description here][8]][8]
 15. Untar the package:  
`sudo tar zxvf solr-6.1.0.tgz`

 16. Copy the installer script to your folder:  
`sudo cp /opt/solr-6.1.0/bin/install_solr_service.sh .`

 17. Remove the unnecessary files:  
`sudo rm -rf solr-6.1.0`

## Install ##

 1. Run the install script:  
`sudo ./install_solr_service.sh solr-6.1.0.tgz`
[![enter image description here][9]][9]
 2. Make SOLR service autostart when the server is rebooted.  
`sudo chkconfig --add solr`  
`chkconfig | grep solr`
[![enter image description here][10]][10]
 3. Change service owner  
`sudo chown -R solr:solr /var/solr/`

## Testing your installation ##

 1. Create a core from command line:  
`sudo su - solr -c "/opt/solr/bin/solr create -c NewCore1 -n data_driven_schema_configs"`
[![enter image description here][11]][11]
 2. Open the Admin in a browser:  
`http://<solr_server>:8983/solr/#/`
[![enter image description here][12]][12]
 3. Open the list of cores in the menu to see the NewCore1 core.
[![enter image description here][13]][13]
 4. Test if cores are sticky:  
`sudo service solr restart`
 5. Refresh the Admin in a browser:  
`http://<solr_server>:8983/solr/#/`
Make sure the Admin page reloads and the core reappears after the reboot.
 6. View server status in the command line:  
`sudo service solr status`
 7. Prepare a new core config for core creation in the Web Admin, by changing your directory location to data:  
`cd var/solr/data/`
[![enter image description here][14]][14]
 8. This is where the new cores are stored:  
`ll`
 9. The newly created core's conf folder can be used as a template:  
`ll NewCore1/`
 10. Create a folder for another core you will create in the Web Admin:  
`mkdir CoreFromWebAdmin`
 11. Copy the conf directory over to the new location:  
`sudo cp -R NewCore1/conf/ CoreFromWebAdmin`
 12. Switch to the Web Admin interface in your browser
 13. Click Add Core
 14. Add CoreFromWebAdmin as the name and the folder for the new core.
[![enter image description here][15]][15]
 15. Open the new core.
 16. Click documents to add docs.
 17. Select XML format and paste the code below:

    <add><doc>
      <field name="id">F9V7464-APL-KIT</field>
      <field name="name">Belkin Mobile Power Cord for iPod w/ Dock</field>
      <field name="manu">Belkin</field>
      <!-- Join -->
      <field name="manu_id_s">belkin</field>
      <field name="cat">electronics</field>
      <field name="cat">connector</field>
      <field name="features">car power adapter, white</field>
      <field name="weight">4.0</field>
      <field name="price">19.95</field>
      <field name="popularity">1</field>
      <field name="inStock">false</field>
      <!-- Buffalo store -->
      <field name="store">45.18014,-93.87741</field>
      <field name="manufacturedate_dt">2005-08-01T16:30:25Z</field>
    </doc>
    
    <doc>
      <field name="id">IW-032</field>
      <field name="name">iPod &amp; iPod Mini USB 2.0 Cable</field>
      <field name="manu">Belkin</field>
      <!-- Join -->
      <field name="manu_id_s">belkin</field>
      <field name="cat">electronics</field>
      <field name="cat">connector</field>
      <field name="features">car power adapter for iPod, white</field>
      <field name="weight">2.0</field>
      <field name="price">11.50</field>
      <field name="popularity">1</field>
      <field name="inStock">false</field>
      <!-- San Francisco store -->
      <field name="store">37.7752,-122.4232</field>
      <field name="manufacturedate_dt">2006-02-14T23:55:59Z</field>
    </doc>
    <doc>
      <field name="id">F887464-APL-KIT</field>
      <field name="name">Belkin Mobile Power Cord for iPod w/ Dock</field>
      <field name="manu">Belkin</field>
      <!-- Join -->
      <field name="manu_id_s">belkin</field>
      <field name="cat">electronics</field>
      <field name="cat">connector</field>
      <field name="features">car power adapter, black</field>
      <field name="weight">4.0</field>
      <field name="price">19.95</field>
      <field name="popularity">1</field>
      <field name="inStock">true</field>
      <!-- Buffalo store -->
      <field name="store">45.18014,-93.87741</field>
      
    </doc>
    <doc>
      <field name="id">FAV7464-APL-KIT</field>
      <field name="name">Belkin Mobile Power Cord for iPod w/ Dock</field>
      <field name="manu">Belkin</field>
      <!-- Join -->
      <field name="manu_id_s">belkin</field>
      <field name="cat">electronics</field>
      <field name="cat">connector</field>
      <field name="features">car power adapter, blue</field>
      <field name="weight">4.0</field>
      <field name="price">15.95</field>
      <field name="popularity">2</field>
      <field name="inStock">true</field>
      <!-- Buffalo store -->
      <field name="store">45.18014,-93.87741</field>
      <field name="manufacturedate_dt">2015-09-21T16:30:25Z</field>
    </doc></add>
If your response returns a success, you have successfully installed SOLR and verified your installation.
[![enter image description here][16]][16]


  [1]: https://i.stack.imgur.com/XlvBS.png
  [2]: https://i.stack.imgur.com/fQntd.png
  [3]: https://i.stack.imgur.com/boW3s.png
  [4]: https://i.stack.imgur.com/icELN.png
  [5]: https://i.stack.imgur.com/ofp5z.png
  [6]: https://i.stack.imgur.com/zVt2n.png
  [7]: https://i.stack.imgur.com/nIPDa.png
  [8]: https://i.stack.imgur.com/tDiqv.png
  [9]: https://i.stack.imgur.com/SF8W2.png
  [10]: https://i.stack.imgur.com/0tfwS.png
  [11]: https://i.stack.imgur.com/XSBIN.png
  [12]: https://i.stack.imgur.com/l0VK8.png
  [13]: https://i.stack.imgur.com/8xJ9w.png
  [14]: https://i.stack.imgur.com/IAKAO.png
  [15]: https://i.stack.imgur.com/GMNxR.png
  [16]: https://i.stack.imgur.com/LTAFU.png

##  Introduction
Solr is a standalone enterprise search server with a REST-like API. You put documents in it (called "indexing") via JSON, XML, CSV or binary over HTTP. You query it via HTTP GET and receive JSON, XML, CSV or binary results.
Solr uses the Lucene search library and extends it.

Here are some of the main features that solr provides:

 **- Advanced Full-Text Search Capabilities**

Powered by Lucene™, Solr enables powerful matching capabilities including phrases, wildcards, joins, grouping and much more across any data type

 - **Optimized for High Volume Traffic**

Solr is proven at extremely large scales the world over

 - **Standards Based Open Interfaces - XML, JSON and HTTP**

Solr uses the tools you use to make application building a snap

 - **Comprehensive Administration Interfaces**

Solr ships with a built-in, responsive administrative user interface to make it easy to control your Solr instances

 - **Easy Monitoring**

Need more insight into your instances? Solr publishes loads of metric data via JMX

 - **Highly Scalable and Fault Tolerant**

Built on the battle-tested Apache Zookeeper, Solr makes it easy to scale up and down. Solr bakes in replication, distribution, rebalancing and fault tolerance out of the box.

 - **Flexible and Adaptable with easy configuration**

Solr's is designed to adapt to your needs all while simplifying configuration

 - **Near Real-Time Indexing**

Want to see your updates now? Solr takes advantage of Lucene's Near Real-Time Indexing capabilities to make sure you see your content when you want to see it

 - **Extensible Plugin Architecture**

Solr publishes many well-defined extension points that make it easy to plugin both index and query time plugins. Of course, since it is Apache-licensed open source, you can change any code you want!

**Some solr cool features:**

 - **Schema when you want, schemaless when you don't**

Use Solr's data-driven schemaless mode when getting started and then lock it down when it's time for production.

 - **Powerful Extensions**

Solr ships with optional plugins for indexing rich content (e.g. PDFs, Word), language detection, search results clustering and more

 - **Faceted Search and Filtering**

Slice and dice your data as you see fit using a large array of faceting algorithms

 - **Geospatial Search**

Enabling location-based search is simple with Solr's built-in support for spatial search

 - **Query Suggestions, Spelling and More**

Solr ships with advanced capabilites for auto-complete (typeahead search), spell checking and more

 - **Rich Document Parsing**

Solr ships with Apache Tika built-in, making it easy to index rich content such as Adobe PDF, Microsoft Word and more.

## Example of Solr Search

[![Here is a basic example of the solr search][1]][1]


  [1]: https://i.stack.imgur.com/q1aZi.jpg

