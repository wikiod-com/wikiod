---
title: "Getting started with Cordova"
slug: "getting-started-with-cordova"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Mobile App Development using Cordova


Apache Cordova is an open-source mobile development framework. It allows you to use standard web technologies - HTML5, CSS3, and JavaScript for cross-platform development. 



## Creating Android build (.apk)

Install cordova using the following command **npm install -g cordova**.

Use cordova -version to check the **cordova version**.

Set path variables ANDROID_HOME and JAVA_HOME.

Example: 

> export ANDROID_HOME  =  /home/geethu/android-sdk-linux

> export PATH          = $PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools 

> export JAVA_HOME     =  /usr/lib/jvm/java-7-openjdk-amd64

> export PATH          =  $PATH:$JAVA_HOME/bin

        
Create a blank Cordova project using the command-line tool. Navigate to the directory where you wish to create your project and type.
                         
**cordova create workshop com.yourname.workshop Workshop**

Cordova CLI, create a Cordova project named Workshop in a directory named workshop.

Navigate to the project directory cd workshop.

Add Android platform as **cordova platform add android**.

[![As seen in the image, if we get this file missing error try adding cordova.js file externally. Add [cordova.js][2] file to the www folder][1]][1]

     
    
Add these script files to your index.html in the following order.

    <script type  = "text/javascript" src = "cordova.js"></script>

To change the app name and icon edit the config.xml file in www folder.

To build the project in the workshop/platforms/android folder 

**cordova build android**

Run it on an Android device connected to your computer using a USB cable, type:


**cordova run android**

***

 1. **> Cordova .apk installation on device**

-------------------------------------

***

In addition, I would like to add some more detail about How you can install .apk file in your android mobile, while deploying your application on the server.

    cordova plugin add cordova-plugin-mfp

run the above command, then try to add the server on which you want to deploy the application.

    mfpdev server add

then follow the instructions by CLI.
run the command below to change your configuration settings.

    mfpdev app config server <profile Name of server>

    cordova build

it will create .apk file in the 

/platforms/android folder

download the apk in your mobile, install it and play with your app.



  [1]: https://i.stack.imgur.com/I9AgU.png
  [2]: https://github.com/apache/cordova-js/blob/master/src/cordova.js

