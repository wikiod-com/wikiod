---
title: "Getting started with jboss"
slug: "getting-started-with-jboss"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting jboss set up or installed.

## Installing on Mac OS X
To install and run jboss AS standalone you can follow the processes described below(assuming that you already have java 7 and jdk installed on your mac):

 1. Download the jboss application server from [here][1].
 2. Unzip the package and extract the folders
 3. Set up the your bash_profile like the following:
    
        open -a TextEdit ~/.bash_profile
        
 4. In your `bash_profile` add the following variables:
     
        export JBOSS_HOME=/Users/$USER/Desktop/Servers/jboss-as-7.1.1.Final
        JAVA_HOME=`/usr/libexec/java_home -v 1.7`

  5. Then move to jboss installation directory and `cd` into the bin directory.
  6. In the bin directory 

         ./standalone.sh

This should start your jboss as a standalone application server. You should see your admin page at http://localhost:8080/

**NB: This installation process does not work with Java 8**

  [1]: http://jbossas.jboss.org/downloads/

