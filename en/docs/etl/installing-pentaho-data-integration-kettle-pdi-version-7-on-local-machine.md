---
title: "Installing Pentaho Data Integration (Kettle) PDI version 7 on local machine"
slug: "installing-pentaho-data-integration-kettle-pdi-version-7-on-local-machine"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

It will focus on Installing Pentaho Kettle - GUI Spoon on Local machine both Windows and Ubuntu machine.


Spoon is a very effective ETL Tool from the basket of Pentaho. it is easy to install and it can change the way the Data Loading and data Cleaning is done in any organisation. it is faster and works well with huge data.

## STEP BY STEP INSTALLATION
Working with Kettle
There are two versions of Kettle aka Pentaho Data Integration :
1. Kettle CE (Community Edition)
2. Kettle EE (Enterprise Edition)

Documents aims mainly on Kettle CE edition.

**Prerequisites**

PDI requires the Oracle Java Runtime Environment (JRE) version 7. You can obtain a JRE for free from Oracle.

Java 8  for PDI 6 and above

Java 7 for older versions

Make sure the java path is set in the environmental variable under Control panel before you run the spoon.bat file.

**Download PDI**

You can download Pentaho Data Integration **Community Edition** from Sourceforge.net. 

Link : https://sourceforge.net/projects/pentaho/files/Data%20Integration/

For the **Enterprise Edition**, please use the Customer Support Portal, or go to

  Link : http://www.pentaho.com/download/ for an Evaluation copy.[![enter image description here][1]][1]

Above image is of sourceforge site

Once you open the Link :
Install pdi-ce-7.0.0.0-25.zip (Latest as of now)

**Windows OS :**
The installation of 801 MB will began.once the installation is done 
1. Extract the  zip file from the download
2. you will find the folder called “data integration”
3. Navigate to the spoon.bat file and  run spoon.bat file to start the Spoon GUI
*Make sure the java path is set properly

**Ubuntu/Linux OS**

On Unix-like operating systems, you may need to make the shell scripts executable by using the chmod command:

navigate to the folder in the terminal 

> **cd data-integration**
> 
> ****chmod +x *.sh*****

then just run the spoon.sh file

>  **./spoon.sh**

*Make sure the java path is set properly


Checking Java Path is set or not

go to terminal 

> **echo $JAVA_HOME**

the path returned by echo $JAVA_HOME is the java path
If the above command  is not returning the path check for java path again. Set java path and try again.

[![enter image description here][2]][2]

That's all about installing Spoon on Local Machine.


  [1]: https://i.stack.imgur.com/yHt2b.png
  [2]: https://i.stack.imgur.com/p89kG.png

