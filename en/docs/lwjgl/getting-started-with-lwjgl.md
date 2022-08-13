---
title: "Getting started with lwjgl"
slug: "getting-started-with-lwjgl"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setting up LWJGL3 in Eclipse
Note: This setup guide assumes that you have Java and Eclipse already installed on your machine.  If you do not, download the latest version of the Java Development Kit (henceforth referred to as the JDK) and the latest version of Eclipse (Neon 3 at time of writing).

# Step One: Downloading LWJGL #

To get started, you will need a copy of LWJGL, which can be accessed from the [LWJGL website][1].  To be assured the most stable version, choose **Release**.  This setup guide will use the **ZIP Bundle** for adding LWJGL to your project.  On the far right of the webpage, choose which dependencies you would like to download.  Once you have selected the contents of the ZIP archive, select which natives you would like, as well as if you want the sources and/or JavaDocs for each binding selected.  Once complete, selected "Download Zip" at the bottom of the selection box.

[![LWJGL Webpage][2]][2]

# Step Two: Making a new project in Eclipse #

Once you have the necessary JAR files downloaded from the LWJGL website, you will need to create a new project in Eclipse.  To do this, open Eclipse to the desired workspace folder.  Once the Eclipse window is opened, navigate to the **New Java Project** dialogue by going to *File -> New -> Java Project*.  Once you have completed the dialogue, hit **Finish** and you will have a new project in your Package Explorer and Navigator.  

[![New Project Dialogue][3]][3]

# Set Three: Adding LWJGL to the Build Path ##

You will also see a new directory in your file explorer.  Inside that new directory, create a folder called **lib**.  Inside this folder, make a folder for your jar files called **jar**.  You will also have native libraries, so create a folder for these as well called **natives**.  Open your jar and put the library jar files into the **jar** folder and the natives into the **natives** folder.  The natives are stored in a *.jar* file, so you will need to   If you chose to add source files and JavaDocs to your download, add these files to the **lib** directory (you could also create a new subdirectory for each, named **javadocs** and **sources** respectively).  Once added, you will need to go back into Eclipse and refresh the project (Windows hotkey F5).  The **lib** folder should be in the navigator and package explorer views.  If not, refresh again.  Once you see the folder, open **Project Properties** dialogue.  This can be found in *Project -> Properties*.  Go to the **Java Build Path** tab on the right.  You will add the JARs from wherever you saved them.  Following along with this tutorial, you will add the JARs from the *Add JARs...* button, because we saved them in the Workspace.  In the file dialogue that opens, select all the JARs that you desire to add.  Once added, you will need to expand each JAR, selecting the location of the native to each JAR.  If you added sources and JavaDocs to your **lib** folder, you will add these here as well.

[![Java Build Path][4]][4]

If you create a new Java Class in the project now, you should have access to the libraries.  You can test this by seeing if GLFW is in the project and usable.  Here is a code snippet for doing this:

[![enter image description here][5]][5]


  [1]: https://www.lwjgl.org/download
  [2]: https://i.stack.imgur.com/xOLqE.png
  [3]: https://i.stack.imgur.com/1JCZI.png
  [4]: https://i.stack.imgur.com/RfZWN.png
  [5]: https://i.stack.imgur.com/rjFea.png

