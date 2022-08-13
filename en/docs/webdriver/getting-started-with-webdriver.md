---
title: "Getting started with webdriver"
slug: "getting-started-with-webdriver"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting webdriver set up or installed.

## Setting up webdriver with node
NOTE: we're only doing this for Chrome at the moment
   #### Windows
   - For Windows we need to download version 2.29 http://chromedriver.storage.googleapis.com/index.html?path=2.29/
   - Once downloaded, extract the zip to a location of your choice
   - Add the location of the driver to the PATH
   - Make sure the latest version of JAVA is installed and up to date
   - Download the latest selenium standalone server from here http://docs.seleniumhq.org/download/
   - Run this:
   ```
   java -jar <wherever you downloaded this file>/selenium-server-standalone-3.0.1.jar

   ```

   - Alternatively use selenium standalone npm package https://www.npmjs.com/package/selenium-standalone
   #### MAC OSX or linux
   - Download the latest driver following from here http://chromedriver.storage.googleapis.com/index.html
   - Copy the driver to your user folder ``` cp <where you downloaded the driver>/<driver file name> /usr/bin```
   - Download the latest selenium standalone server from here http://docs.seleniumhq.org/download/
   - Run this:
   ```
   java -jar <wherever you downloaded this file>/selenium-server-standalone-3.0.1.jar

   ```

