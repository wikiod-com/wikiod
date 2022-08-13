---
title: "Selenium Grid"
slug: "selenium-grid"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Node configuration
Selenium Grid Node configuration resides on the Node itself and holds the information about network configuration and Node capabilities. The configuration can be applied in various ways:

 - Default Configuration
 - JSON Configuration
 - Command line Configuration


**JSON Configuration**

The node configuration in JSON file is split into 2 sections:

 - Capabilities
 - Configuration

***Capabilities*** defines areas like what Browser types and versions are supported, locations of browser binaries, number of maximum instances of each browser type.

***Configuration*** deals with settings like hub and node addresses and ports. 

Below is a an example of a JSON configuration file:


    {
      "capabilities": [
        {
          "browserName": "firefox",
          "acceptSslCerts": true,
          "javascriptEnabled": true,
          "takesScreenshot": false,
          "firefox_profile": "",
          "browser-version": "27",
          "platform": "WINDOWS",
          "maxInstances": 5,
          "firefox_binary": "",
          "cleanSession": true 
        },
        {
          "browserName": "chrome",
          "maxInstances": 5,
          "platform": "WINDOWS",
          "webdriver.chrome.driver": "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
        },
        {
          "browserName": "internet explorer",
          "maxInstances": 1,
          "platform": "WINDOWS",
          "webdriver.ie.driver": "C:/Program Files (x86)/Internet Explorer/iexplore.exe" 
        }
      ],
      "configuration": {
        "_comment" : "Configuration for Node",
        "cleanUpCycle": 2000,
        "timeout": 30000,
        "proxy": "org.openqa.grid.selenium.proxy.WebDriverRemoteProxy",
        "port": 5555,
        "host": ip,
        "register": true,
        "hubPort": 4444,
        "maxSessions": 5
      }
    }

## How to create a node
To create a node, you first need to have a hub. If you don't have a hub, you can create it like this:

    java -jar selenium-server-standalone-<version>.jar -role hub

Then you're able to create a node:

    java -jar selenium-server-standalone-<version>.jar -role node -hub http://localhost:4444/grid/register // default port is 4444

More info here: https://github.com/SeleniumHQ/selenium/wiki/Grid2


