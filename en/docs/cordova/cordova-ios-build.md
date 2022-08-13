---
title: "Cordova ios build"
slug: "cordova-ios-build"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Hope you are familiar with the basics of cordova. Let's try to build a cordova ios build, ios build is bit different as that of android build, we need mac machine to perform this task. No can also prepare the ios build online, but to test and debug your application on a mac  simulator you have to have the mac machine with you.

let's start with an example.



you face any problem during preparing the ios build please reach to me, will try to assist you.


## cordova HelloWorld Project
To prepare the ios build first we need to create the cordova project.
lets create the project by the command line tool.

      cordova create hello com.example.hello "HelloWorld"

Go to the project dir by  `cd hello`.

we are now in the project directory, lets check which platforms are available to us

    cordova platform ls

As, we need to prepare the ios build, we will add ios platform to the project.

    cordova platform add ios@version    => put the desired version you want to add.
    cordova platform add ios@latest.    => add the latest version available.

Now we need **Xcode** on mac machine to prepare the ios build.

Download the latest Xcode available to us via app store. Please ignore the beta versions of xcode as they have compatibility issue sometime.

After Xcode installation install the Xcode cli using terminal by the command below.

    xcode-select --install 
 It will popup the window, please follow the instructions and install the cli properly. Now we just need one more tool to deploy the ios build, install it as well.

    npm install -g ios-deploy
during the installation of deployment tool if you get the permission denied error then please try with the sudo command.

    sudo npm install -g ios-deploy

lets prepare the ios build using the commands

    cordova platform add ios

 if you didnt add the platform before, add it.

    cordova prepare => it will move the all required files to platform folder and create a .xcworkspace file in the platform.ios folder.

    cordova build ios => to build the ios application.



 But wait, we haven't run the build on simulator, lets do it also.

you have two ways to do it 

1. using cli

2. using Xcode

**first lets do it with CLI.**

    corodva run ios => it will run the application on the default simulator available.

if you want  to play with the simulators then please explore the `cordova emulator` command 



**using Xcode**

jump the project folder and move into `/platform/ios folder` of your project.
open the file .xcworkspce using command `open <ProjectName>.xcworkspcae` eg `open MyApp.xcworkspcace.`

it will redirect you to the Xcode window, there on the window top left you can see your project, click on the project, and on top header you can see the run button, click it and play with your app.

Thank you.



 


