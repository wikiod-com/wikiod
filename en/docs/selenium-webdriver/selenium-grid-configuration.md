---
title: "Selenium Grid Configuration"
slug: "selenium-grid-configuration"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

Selenium Grid is a framework to run test distributed over a range of test devices. It's used for testing web applications. Its possible to write tests in different popular programming languages, including C#, Groovy, Java, Perl, PHP, Python and Ruby. The tests can be run against a range of webbrowsers on platforms like  Windows, Linux, and OS X.

It is open-source software, released under the Apache 2.0 license: web developers can download and use it without charge.

## Syntax
 - for to run the jar file the following is the syntax for every jar file
 - `java -jar <jar-file-full-name>.jar -<your parameters if any>`

## Parameters
  | **Parameters**| **Details**|
  | ---------------------------------------- | ---------------------------------------- |
  | role | Is what tells the selenium which it was `hub` or `node`|
  | port | This is to specify which port the `hub` or `node` should be listening.|
  | hub | This parameter is used in `node` to specify the hub url|
 | browserName | Its been used in `node` to specify the browser name like firefox, chrome, internet explorer|
 | maxInstances | Its where the instance of the browser is being specified eg. 5 means there will be 5 instance of the browser which user specified will be present.|
| nodeConfig | A Json configuration file for the node. You can specify the role, port etc. in here |
| hubConfig | A Json configuration file for the node. You can specify the role, port, max instances etc. in here |


## Creating a Selenium Grid hub and node
# Creating a hub #
A quick configuration for a hub and node setup in selenium grid. For more information see: [Grid 2 docs][1]
## Requirements ##
To set up a grid hub you need the flowing:

 - [Selenium-server-standalone-<Version>.jar][2] 

## Creating the hub ##
To Create a Hub you need to run the selenium server.
 
 1. Download Selenium-server-standalone-<Version>.jar 
 2. Open your terminal and navigate to the folder where Selenium-server-standalone-<Version>.jar is
 2. Execute the folowing command:
    1. For default configuration `java -jar selenium-server-standalone-<Version>.jar -role hub`
    2. For Json configuration `java -jar selenium-server-standalone-<Version>.jar -role hub -hubConfig hubConfig.json` 
 4. Open http://localhost:4444/ you will see a message a follows

[![enter image description here][3]][3]

On clicking `console` -> `View config` for to view the Configuration for the hub details.

# Creating a Node #
## Requirements ##
To set up a grid hub you need the flowing:

 - Selenium-server-standalone-<Version>.jar 
 - Webdrivers 
    - [Chrome driver][4]
    - [FireFox driver][5]
    - [Microsoft Edge driver][6]
 - Browsers
    - [Chrome][7]
    - [FireFox][8]
    - [Microsoft Edge][9] (Windows 10)

## Creating the Node ##
Now To create Nodes for the Hub
 1. Download Selenium-server-standalone-<Version>.jar 
 2. Download the browsers you want to test in
 3. Download the drivers for the browsers you want to test in
 4. Open new terminal and navigate to the selenium server jar file location
 5. Execute the folowing command:
    1. for default configuration `java -jar selenium-server-standalone-<VERSION NUMBER>.jar -role node`
    2. For Json configuration `java -jar selenium-server-standalone-<Version>.jar -role node -nodeConfig nodeConfig.json` 
 6. Now go to http://localhost:4444/grid/console to view the node details 


  [1]: https://github.com/SeleniumHQ/selenium/wiki/Grid2 "Grid2 docs"
  [2]: http://selenium-release.storage.googleapis.com/index.html "selenium-server-standalone-version.jar"
  [3]: http://i.stack.imgur.com/GzPVL.png
  [4]: https://sites.google.com/a/chromium.org/chromedriver/downloads "Chrome driver"
  [5]: https://github.com/mozilla/geckodriver/releases "FireFox driver"
  [6]: https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/ "Microsoft Edge driver"
  [7]: https://www.google.com/chrome/browser/desktop/index.html "Chrome"
  [8]: https://www.mozilla.org/nl/firefox/new/ "FireFox'"
  [9]: https://www.microsoft.com/nl-nl/windows/microsoft-edge "Microsoft Edge"

## Java code for Selenium Grid
    String hubUrl = "http://localhost:4444/wd/hub"       
    DesiredCapabilities capability = DesiredCapabilities.firefox(); //or which browser you want
    RemoteWebDriver driver = new RemoteWebDriver(hubUrl, capability);

## Configuragtion via Json
An example configuration for a hub:

`java -jar selenium-server-standalone-<version>.jar -role hub -hubConfig hubConfig.json`

    {
        "_comment" : "Configuration for Hub - hubConfig.json",
        "host": ip,
        "maxSessions": 5,
        "port": 4444,
        "cleanupCycle": 5000,
        "timeout": 300000,
        "newSessionWaitTimeout": -1,
        "servlets": [],
        "prioritizer": null,
        "capabilityMatcher": "org.openqa.grid.internal.utils.DefaultCapabilityMatcher",
        "throwOnCapabilityNotPresent": true,
        "nodePolling": 180000,
        "platform": "WINDOWS"
    }

An example configuration for a node

`java -jar selenium-server-standalone-<version>.jar -role node -nodeConfig nodeConfig.json`

    {
      "capabilities":
      [
        {
          "browserName": "opera",
          "platform": "WINDOWS",
          "maxInstances": 5,
          "seleniumProtocol": "WebDriver",
          "webdriver.opera.driver": "C:/Selenium/drivers/operadriver.exe",
          "binary":"C:/Program Files/Opera/44.0.2510.1159/opera.exe"
        },
        {
          "browserName": "chrome",
          "platform": "WINDOWS",
          "maxInstances": 5,
          "seleniumProtocol": "WebDriver",
          "webdriver.chrome.driver": "C:/Selenium/drivers/chromedriver.exe",
          "binary":"C:/Program Files/Google/Chrome/Application/chrome.exe"
        },
        {
          "browserName": "firefox",
          "platform": "WINDOWS",
          "maxInstances": 5,
          "seleniumProtocol": "WebDriver",
          "webdriver.gecko.driver": "C:/Selenium/drivers/geckodriver.exe",
          "binary":"C:/Program Files/Mozilla Firefox/firefox.exe"
        }
      ],
      "proxy": "org.openqa.grid.selenium.proxy.DefaultRemoteProxy",
      "maxSession": 5,
      "port": 5555,
      "register": true,
      "registerCycle": 5000,
      "hub": "http://localhost:4444",
      "nodeStatusCheckTimeout": 5000,
      "nodePolling": 5000,
      "role": "node",
      "unregisterIfStillDownAfter": 60000,
      "downPollingLimit": 2,
      "debug": false,
      "servlets" : [],
      "withoutServlets": [],
      "custom": {}
    }

