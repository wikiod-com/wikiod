---
title: "Getting started with selenium-ide"
slug: "getting-started-with-selenium-ide"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup for selenium IDE in Firefox.
**Installation :** 

 - Open Firefox browser.click on menu and select Add-ons


[![enter image description here][1]][1]
 - Search for selenium IDE 
 - Install selenium IDE Add-on in firefox.
 - Restart the Firefox.
 - If the selenium IDE is installed successfully then you can see the icon on top of the browser
[![enter image description here][2]][2]

 - By clicking on this icon, you can launch the selenium IDE.

**Recording and Executing a script**

 - After launching the IDE, Click on record button to record a script.
[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/qMbFF.png
  [2]: https://i.stack.imgur.com/vPXaq.png
  [3]: https://i.stack.imgur.com/6LYhn.png

## How to export the testcase as Java class from selenium IDE
Lets take fb login as testcase,will see how to capture and export as JAVA class

 - Click on the selenium IDE icon on top right corner of your browser
 - Enter the url `https://facebook.com`
 - Click on Record button , as shown in the screenshot
[![enter image description here][1]][1]
 - Export the usecase as java file
[![enter image description here][2]][2]
 - Save the file in you local machine with .java extension
 - Open that file it will looks like this:
[![enter image description here][3]][3]
 - Now you can import this file into any IDE
 - Assume that we are going to user eclispe,create a new package `com.example.tests`
 - Just copy this file and paste there
 - Add the required jar files (Selenium jar + TesgNG jar)

Now you are ready to run the test.

  [1]: https://i.stack.imgur.com/YFYM3.png
  [2]: https://i.stack.imgur.com/sWR1A.png
  [3]: https://i.stack.imgur.com/4fBOm.png

