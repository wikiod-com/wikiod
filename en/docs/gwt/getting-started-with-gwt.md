---
title: "Getting started with gwt"
slug: "getting-started-with-gwt"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Download and unzip the [GWT SDK][1]. This contains the core libraries, compiler, and development server that you need to write web applications.

On Windows, extract the files from the compressed folder gwt-2.7.0.zip. On Mac or Linux, you can unpack the package with a command like:

    unzip gwt-2.7.0.zip

The GWT SDK doesn’t have an installer application. All the files you need to run and use the SDK are located in the extracted directory.

Also, you need to have the the [Apache ant][2] installed on your system in order to be able to run the web application locally. On mac you can install it using following command.It installs the apache using mac port.

 `sudo port install apache-ant`


  [1]: http://www.gwtproject.org/download.html
  [2]: http://ant.apache.org/manual/install.html

## Create your first web application
GWT ships with a command line utility called `webAppCreator` that automatically generates all the files you’ll need in order to start a GWT project. It also generates Eclipse project files and launch config files for easy debugging in GWT’s development mode.

You can create a new demo application in a new MyWebApp directory by running webAppCreator:

**Windows**

    cd gwt-2.7.0
    webAppCreator -out MyWebApp com.mycompany.mywebapp.MyWebApp

**Mac or Linux** 

    cd gwt-2.7.0
    
    chmod u+x webAppCreator
    
    ./webAppCreator -maven -out MyWebApp com.mycompany.mywebapp.MyWebApp

The `webAppCreator` script will generate a number of files in `MyWebApp/`, including some basic “Hello, world” functionality in the class `MyWebApp/src/com/mycompany/mywebapp/client/MyWebApp.java`. 

