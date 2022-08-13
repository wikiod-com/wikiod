---
title: "Maven install in window"
slug: "maven-install-in-window"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

how to install maven in window 7 


how to install maven in window 7 
Steps:
1. download the maven form https://maven.apache.org/download.cgi (poffice website)
2.unzip the maven binary folder and  save into any floder (good : save in the program files in c drive )
3. Check environment variable value
open command prompt and type echo %java_home% 
its should display path of the jdk 
like : C:\Program Files\Java\jdk1.8.0_102
if not displayed set the environment variable java_home

Set the environment variables using system
m2_home  : set the path of the folder where its stored 
maven_home : same as above 
set the path 
Adding to PATH: Add the unpacked distribution’s bin directory to your user PATH environment variable
path:%m2_home%\bin 

To verify 
open the command prompt  and type mvn -version 
should display this message 
Apache Maven 3.3.3 (7994120775791599e205a5524ec3e0dfe41d4a06; 2015-04-22T04:57:37-07:00)
Maven home: /opt/apache-maven-3.3.3
Java version: 1.8.0_45, vendor: Oracle Corporation
Java home: /Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home/jre
Default locale: en_US, platform encoding: UTF-8
OS name: "mac os x", version: "10.8.5", arch: "x86_64", family: "mac


some time it will not display because mvn folder not running with the admin access 
make running as administrator





## installing
Check environment variable value e.g.
echo %JAVA_HOME% 
C:\Program Files\Java\jdk1.7.0_51

