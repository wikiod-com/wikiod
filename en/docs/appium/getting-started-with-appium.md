---
title: "Getting started with appium"
slug: "getting-started-with-appium"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
## Pre-requirements ##
Check the requirements for each device type you wish to automate and make sure they're installed before attempting to use Appium!

**iOS Requirements**

 - Mac OS X 10.10 or higher, 10.11.1 recommended
 - XCode >= 6.0, 7.1.1 recommended
 - Apple Developer Tools (iPhone simulator SDK, command line tools)
 - [Ensure you read the documentation on setting yourself up for iOS testing!][1]

**Android Requirements**

 - [Android SDK][2] API >= 17 (Additional features require 18/19)

 - Appium supports Android on OS X, Linux and Windows. Make sure you follow the directions for setting up your environment properly for testing on different OSes:

   - [linux][3]
   - [osx][1]
   - [windows][4]

**FirefoxOS Requirements**

 - [Firefox OS Simulator][5]


----------


## Installation of Appium ##

**Global installation using Node.js**

    $ npm install -g appium
    $ appium

**Local installation from Github's master branch**

    $ git clone git@github.com:appium/appium.git
    $ cd appium
    $ npm install
    $ node .

**Using the App for Mac or Windows**

 - [Download the Appium app][6]
 - Run it!

## Writing Tests for Appium ##

Formatted version of the Appium [docs][7] can be found [here][8] with the ability to choose code example language from the top right corner.


  [1]: https://github.com/appium/appium/blob/master/docs/en/appium-setup/running-on-osx.md
  [2]: http://developer.android.com/
  [3]: https://github.com/appium/appium/blob/master/docs/en/appium-setup/running-on-linux.md
  [4]: https://github.com/appium/appium/blob/master/docs/en/appium-setup/running-on-windows.md
  [5]: https://developer.mozilla.org/en/docs/Tools/Firefox_OS_Simulator
  [6]: https://bitbucket.org/appium/appium.app/downloads/
  [7]: https://github.com/appium/appium/tree/master/docs/en
  [8]: http://appium.io/slate/en/master/?java#about-appium

## Launching Appium for Android platform and creating sample test
Environment Setup:
•    Download android sdk of API level 17 or more
•    Node.js (https://nodejs.org/)
•    Appium software (http://appium.io/)
•    Selenium jars (http://www.seleniumhq.org/download/)
•    Appium jar (https://search.maven.org/#search%7Cga%7C1%7Cg%3Aio.appium%20a%3Ajava-client)
•    .apk file of the application which needs to be tested

Preconditions:
•    make sure Eclipse is downloaded from www.eclipse.org/downloads/
•    java is installed (both jdk and jre)
•    android sdk is installed
•    Make sure your environment variable (Path) for Java, Android SDK, Platform and platform-tools is set.

Steps to set Path on windows OS:
    Right Click “My Computer”. 
    “Properties”
    On left panel “Advance System Settings”
    Select Environment Variables
    System Variables-> Type Path-> “Path” double click
    Enter the path to JAVA jdk in your system followed by (;) then path to your android sdk (;) path to your android platform (;) path to your android platform tools-> Click OK.

•    Make sure Eclipse Plug-in is installed

Steps to install Eclipse Plug-in for Android:
    Start Eclipse, then select Help > Install New Software.
    Click Add, in the top-right corner.
    In the Add Repository dialog that appears, enter "ADT Plugin" for the Name and the following URL for the Location:  https://dl-ssl.google.com/android/eclipse/
    Click OK (If you have trouble acquiring the plugin, try using "http" in the Location URL, instead of "https" (https is preferred for security reasons).

•    Make sure ANDROID_HOME variable is set.

Steps to set ANDROID_HOME variable:
    Go to Eclipse->Window on top panel->Preferences-> Double click Android on left panel
    In the Android preferences, Copy the SDK Location
    Right Click “My Computer”. 
    “Properties”
    On left panel “Advance System Settings”
    Select Environment Variables
    On the top User Variables-> Select new-> Variable Name, Enter ANDROID_HOME, Variable Path-> Enter copied SDK location from Eclipse-> Click OK
    Then System Variables-> Select new-> Variable Name, Enter ANDROID_HOME, Variable Path-> Enter copied SDK location from Eclipse-> Click OK
    Exit

•    Make sure Android Virtual Device Manager can be launched. 
Eclipse->Window on top panel->Android Virtual Device Manager-> Click on the existing virtual device if it exists/ Create a new one with customized configurations.-> Click on “Start” on the right panel of the window.-> Launch

Launching Appium:
•    Install node.js (“http://nodejs.org/”).
•    Launch Appium from command  line from the below location:
Goto Appium folder node_modules appiumbinshift+right clickopen command prompttype node appiumenter

Following should be displayed:
info: Welcome to Appium v1.3.4 (REV c8c79a85fbd6870cd6fc3d66d038a115ebe22efe) 
info: Appium REST http interface listener started on 0.0.0.0:4723 
info: Console LogLevel: debug
info: Appium REST http interface listener started on 0.0.0.0:4723info: Console LogLevel: debug

Write a Program to launch Appium in Eclipse:
package appium.com;

import java.net.MalformedURLException;
import java.net.URL;

import org.openqa.selenium.remote.CapabilityType;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.remote.RemoteWebDriver;


public class AppiumLaunch 
{
    public static void main(String args[]) throws MalformedURLException
    {
    RemoteWebDriver  driver;
    DesiredCapabilities capabilities =new DesiredCapabilities();
    
    capabilities.setCapability("platformName", "Android");
    capabilities.setCapability("deviceName","");
    
    capabilities.setCapability("version","4.4.2");
    capabilities.setCapability("device ID","");
    capabilities.setCapability("app-package","");
    capabilities.setCapability(CapabilityType.BROWSER_NAME, "");                                                
    capabilities.setCapability("app-activity","");
    capabilities.setCapability("takesScreenshot",true);
    
    capabilities.setCapability("app","C:/Users/.......apk");
    
    driver=new RemoteWebDriver( new URL("http://127.0.0.1:4723/wd/hub"), capabilities);
    System.out.println("app is launched on the device");
     
    
    
    }
    
    
}

•    Make sure the path of the apk file in the system is correct 
•    Make sure the path to the apk file in your system is correct in the program. Use correct package and activity which can be found by decompiling the apk file. For decompiling apk file, go to http://www.decompileandroid.com.

Steps to launch appium for android:
1.    First start the appium server on command prompt or by running the appium.exe file.
2.    Check whether the device is connected and displayed in adb: adb devices
3.    Execute the program on the Eclipse. The program will get executed and .apk file which was installed in the device will launch the app. 




