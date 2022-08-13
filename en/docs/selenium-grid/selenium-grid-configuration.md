---
title: "Selenium Grid Configuration"
slug: "selenium-grid-configuration"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

# Downloads 

This chapter contains useful downloads like the webdrivers and link to browsers

#
## Drivers ##
Place all the drivers in your path variable

 - [Chrome driver][1]
 - [FireFox driver][2]
 - [Microsoft Edge driver][3]
 - [Opera driver][4]
 
## Browsers ##
 - [Chrome][5]
 - [FireFox][6]
 - [Microsoft Edge][7]
 - [Opera][8]

  [1]: https://sites.google.com/a/chromium.org/chromedriver/downloads "Chrome driver"
  [2]: https://github.com/mozilla/geckodriver/releases "Firefox driver"
  [3]: https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/ "Microsoft Edge driver"
  [4]: https://github.com/operasoftware/operachromiumdriver/releases "Opera driver"
  [5]: https://www.google.com/chrome/browser/desktop/index.html "Chrome"
  [6]: https://www.mozilla.org/nl/firefox/new/ "Firefox"
  [7]: https://www.microsoft.com/nl-nl/windows/microsoft-edge "Microsoft Edge"
  [8]: http://www.opera.com/nl "Opera"

## Installation or Setup
Before setting up a Selenium grid you need to make sure you have *Java* installed and configured in your computer’s environment path.

## **Configure the Hub** ##

 - Download latest stable [Selenium Server](http://selenium-release.storage.googleapis.com/index.html) version.
 - Start the command prompt and navigate to the location in which you placed the Selenium server jar file.
 - Type: (FYI: your version number may be different than mine)
java –jar selenium-server-standalone-2.53.0.jar –role hub
 - It should now look something like this:

[![enter image description here][1]][1]

Basically what happened is selenium webserver started and is now listening on a port - in this case default **4444** (FYI - This port number can be changed by passing the *-port* parameter followed by the port number on which you want to run the server).

 - Now open a browser and navigate to http://localhost:4444/grid/console
 - If everything is working, server should come up and you would see something like this:

[![Selenium Server Grid Console][2]][2]

Next, we need to setup some node machines.

## Configure the Nodes ##

 - Just like we downloaded [Selenium Server](http://selenium-release.storage.googleapis.com/index.html) for **Hub**, we also need to download it on all our **Node** machines.
 - Once you have the Selenium-server jar file on the node machine, navigate to the directory where jar is downloaded and open up cmd prompt.
 - Type: java –jar selenium-server-standalone-2.53.0.jar –role node –hub http://hubIP:4444/grid/register
     - **hubIP** :- in case the hub and nodes are running on a different machine
     - **localhost** :- in case the hub and nodes are running on same machine

As you can see the node is now registered to the hub, by default node starts on **-port 5555** but you can change the same by using **-port** parameter followed by port number.

[![enter image description here][3]][3]

If everything works as expected you should now see the IP address of the node you just started and registered in the hub console view:

[![enter image description here][4]][4]

## Things to Notice ##

 - If we don't specify the `seleniumProtocol`, Node will be registered with both Remote Control (Legacy) and Webdriver Protocol (as seen in the screenshot above).
 - If the browsers type and number of instances aren't mentioned, Node will launch 5 instance of Firefox, 5 Instance of Chrome and 1 Instance of IE driver.

That's all you would be needing to do for an up and running Selenium Grid.

  [1]: https://i.stack.imgur.com/q1nSC.png
  [2]: https://i.stack.imgur.com/2iSdv.png
  [3]: https://i.stack.imgur.com/PZmf0.png
  [4]: https://i.stack.imgur.com/KSJMf.png

## Json configuration
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

## Configuration and useage in C#

## Configuration ##
In the following paragraphs there wil be an example per browser for the configuration in Json and setup in C#.

This example expects you to have all the drivers in your path variable and browsers installed.
### Microsoft Edge ###

C# code to create a remote webdriver

    // Defining webdriver variable
    RemoteWebDriver _webDriver;
    // Creating Capabilities and chosing browser
    capabiliteiten = DesiredCapabilities.Edge();
    // Setting platform 
    capabiliteiten.Platform = new Platform(PlatformType.Windows);
    // Requesting remote webdriver
    _webDriver = new RemoteWebDriver(_gridServerUri, capabiliteiten);

Node configuration in Json

    {
        "browserName":"MicrosoftEdge",
        "platform": "WINDOWS",
        "maxIstances": 1,
        "seleniumProtocol": "WebDriver"
    }
### Chrome ###   
 
C# code to create a remote webdriver

    // Defining webdriver variable
    RemoteWebDriver _webDriver;
    // Creating Capabilities and chosing browser
    capabiliteiten = DesiredCapabilities.Chrome();
    // Setting platform 
    capabiliteiten.Platform = new Platform(PlatformType.Windows);
    // Requesting remote webdriver
    _webDriver = new RemoteWebDriver(_gridServerUri, capabiliteiten);

Node configuration in Json

    {
        "browserName": "chrome",
        "platform": "WINDOWS",
        "maxInstances": 5,
        "seleniumProtocol": "WebDriver"
    }
    
### Firefox ### 
   
C# code to create a remote webdriver

    // Defining webdriver variable
    RemoteWebDriver _webDriver;
    // Creating Capabilities and chosing browser
    capabiliteiten = DesiredCapabilities.Firefox();
    // Setting platform 
    capabiliteiten.Platform = new Platform(PlatformType.Windows);
    // Requesting remote webdriver
    _webDriver = new RemoteWebDriver(_gridServerUri, capabiliteiten);

Node configuration in Json

    {
        "browserName": "firefox",
        "platform": "WINDOWS",
        "maxInstances": 5,
        "seleniumProtocol": "WebDriver"
    }    

### Opera ###    

C# code to create a remote webdriver
**This is for OperaChromium**

    // Defining webdriver variable
    RemoteWebDriver _webDriver;
    // Creating Capabilities
    capabiliteiten = new DesiredCapabilities();
    // Setting platform 
    capabiliteiten.Platform = new Platform(PlatformType.Windows);
    // Chosing browser
    capabiliteiten.SetCapability(CapabilityType.BrowserName, "operablink");
    // Requesting remote webdriver
    _webDriver = new RemoteWebDriver(_gridServerUri, capabiliteiten);

Node configuration in Json

    {
        "browserName": "operablink",
        "platform": "WINDOWS",
        "maxInstances": 5,
        "seleniumProtocol": "WebDriver"
    }

Platform type can be one of the following:
 - PlatformType.Android;
 - PlatformType.Any;
 - PlatformType.Linux;
 - PlatformType.Mac;
 - PlatformType.Unix;
 - PlatformType.Vista;
 - PlatformType.Windows;
 - PlatformType.WinNT;
 - PlatformType.XP;

## Configuration Json and C# mulitple browsers

## Configuration ##
In the following paragraphs there wil be an example per browser for the configuration in Json and setup in C#.

This example expects you to have all the browsers installed and the drivers in your path variable

### Microsoft Edge ###

C# code to create a remote webdriver

    // Defining webdriver variable
    RemoteWebDriver _webDriver;
    // Creating Capabilities and chosing browser
    capabiliteiten = DesiredCapabilities.Edge();
    // Setting platform 
    capabiliteiten.Platform = new Platform(PlatformType.Windows);
    // Requesting remote webdriver
    _webDriver = new RemoteWebDriver(_gridServerUri, capabiliteiten);

Node configuration in Json

    {
        "browserName":"MicrosoftEdge",
        "platform": "WINDOWS",
        "maxIstances": 1,
        "seleniumProtocol": "WebDriver"
    }
### Chrome ###   
 
C# code to create a remote webdriver

    // Defining webdriver variable
    RemoteWebDriver _webDriver;
    // Creating Capabilities and chosing browser
    capabiliteiten = DesiredCapabilities.Chrome();
    // Setting platform 
    capabiliteiten.Platform = new Platform(PlatformType.Windows);
    // Requesting remote webdriver
    _webDriver = new RemoteWebDriver(_gridServerUri, capabiliteiten);

Node configuration in Json

    {
        "browserName": "chrome",
        "platform": "WINDOWS",
        "maxInstances": 5,
        "seleniumProtocol": "WebDriver"
    }
    
### Firefox ### 
   
C# code to create a remote webdriver

    // Defining webdriver variable
    RemoteWebDriver _webDriver;
    // Creating Capabilities and chosing browser
    capabiliteiten = DesiredCapabilities.Firefox();
    // Setting platform 
    capabiliteiten.Platform = new Platform(PlatformType.Windows);
    // Requesting remote webdriver
    _webDriver = new RemoteWebDriver(_gridServerUri, capabiliteiten);

Node configuration in Json

    {
        "browserName": "firefox",
        "platform": "WINDOWS",
        "maxInstances": 5,
        "seleniumProtocol": "WebDriver"
    }    

### Opera ###    

C# code to create a remote webdriver

    // Defining webdriver variable
    RemoteWebDriver _webDriver;
    // Creating Capabilities
    capabiliteiten = new DesiredCapabilities();
    // Setting platform 
    capabiliteiten.Platform = new Platform(PlatformType.Windows);
    // Chosing browser
    capabiliteiten.SetCapability(CapabilityType.BrowserName, "operablink");
    // Requesting remote webdriver
    _webDriver = new RemoteWebDriver(_gridServerUri, capabiliteiten);

Node configuration in Json

    {
        "browserName": "operablink",
        "platform": "WINDOWS",
        "maxInstances": 5,
        "seleniumProtocol": "WebDriver"
    }

Platform type can be one of the following:

     PlatformType.Android;
     PlatformType.Any;
     PlatformType.Linux;
     PlatformType.Mac;
     PlatformType.Unix;
     PlatformType.Vista;
     PlatformType.Windows;
     PlatformType.WinNT;
     PlatformType.XP;

