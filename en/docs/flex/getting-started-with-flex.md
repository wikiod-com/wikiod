---
title: "Getting started with flex"
slug: "getting-started-with-flex"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup (Apache Flex)
There are two ways to setup the Apache Flex SDK. You can use the provided Apache Flex SDK Installer, an Adobe AIR application that automates the process (on Windows or OS X/macOS).  Or you can install it manually which obviously requires a greater comfort with your platform but provides more flexibility with the installation details.

Installing Apache Flex SDK Using the Apache Flex Installer
================================


 1. Download the [installer][1] binary for your operating system (Window, Mac
    or Linux).  
 2. Run the installer
 3. Select the version of the SDK you want to download. The Apache Flex project suggests that you download latest released version. 
 4. Select the Flash Player version. The Flex project suggests that you leave it at the as the default. Click **Next**.
 5. Select a directory where you want the SDK installed. Click **Next**.
 6. Check all of the license agreements. Click **Install**.
 7. Wait for the installer to download and install the Apache Flex SDK.


Installing Apache Flex SDK Manually
======= 
NOTE: This can be a lengthy and involved process.

# Prepare your system
1. Download and install the latest Java JDK
2. Download and install the latest version of Ant
3. Download and install the latest version of Git
4. Download and install the latest JUnit

# Setup the Folder Structure
 1. Create the following directory structure on your computer for the Flex SDK, usually stored in the root or Documents directory.

<!-- language: lang-bash -->

    /ApacheFlex
    /ApacheFlex/dependencies
    /ApacheFlex/source

# Download Source 
 1. Open a terminal and change the directory to:

<!-- language: lang-bash -->

    /ApacheFlex/source

2. Run the following git command to download the Flex SDK source:
<!-- language: lang-bash -->
    git clone https://git-wip-us.apache.org/repos/asf/flex-sdk.git flex-sdk

 3. Run the following git command to download the TLF source:
<!-- language: lang-bash -->
    git clone https://git-wip-us.apache.org/repos/asf/flex-tlf.git flex-tlf

 4. Change to the flex-sdk directory:
<!-- language: lang-bash -->

    /ApacheFlex/source/flex-sdk

 5. Switch to the 'develop' branch (Optional):
<!-- language: lang-bash -->

    git checkout develop

# Edit the Environment Properties
For the build scripts to work you need to set the **Environment Variables**. The environment properties file contains the environment variables which point to the locations of the dependencies you will use throughout the process. These are used by the various build scripts to run and compile the SDK. You will add additional environment variables through out the rest of this tutorial. When you run the build script later the script will validate the paths provided and warn you if they are incorrect.

 1. Create the environment properties file:

In the `flex-sdk` directory create a copy of the `env-template.properties` file and name it `env.properties`. 

Since you've already installed Java and Ant, set the path of those by appending the following at the end of the `env.properties` file:
 
    env.JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/Current/Commands/java
 
    env.ANT_HOME=/Applications/Ant/bin
 
 2. Next add the path to the TLF directory:
<!-- language: lang-bash -->
    env.TLF_HOME=/ApacheFlex/source/flex-tlf


# Add Required Dependencies

## Adobe AIR SDK

 1. Download the Air SDK Flex Edition (*Be sure to download the edition specifically made for Flex*).
 2. Extract to:
<!-- language: lang-bash -->
    /ApacheFlex/dependencies/AIRSDK/4.0
where the final directory name refers to the version of the AIR SDK you downloaded.

 3. Set the `AIR_HOME` environment variable to the AIR SDK path you chose in the previous step. For example,
<!-- language: lang-bash -->

    env.ANT_HOME=/ApacheFlex/dependencies/AIRSDK/4.0

## Adobe Flash Player Projector

 1. [Download the Flash Player Projector][3] 
 2. Run the installer. The Projector Content Debugger install program does not install into anywhere (for me at least). Once you run the installer you must copy it to the location you want. In this case, copy it to the `/ApacheFlex/dependencies/player` directory. 
Note: on OSX, append '.app/Contents/MacOS/Flash Player Debugger' to the path to the Flash Player.
<!-- language: lang-bash -->
    /ApacheFlex/dependencies/player/Flash Player.app/Contents/MacOS/Flash Player Debugger
 3. Set the `FLASHPLAYER_DEBUGGER` environment variable to the installation path of the projector.
<!-- language: lang-bash -->
    env.FLASHPLAYER_DEBUGGER=/ApacheFlex/dependencies/player/Flash Player.app/Contents/MacOS/Flash Player Debugger
 
You may also be able to use the Flash Player Plug-in. It is usually located here:
    /Library/Internet Plug-ins/Flash Player/
The full path would be:
    /Library/Internet Plug-ins/Flash Player.app/Contents/MacOS/Flash Player Debugger
    
## Adobe Flash Player playerglobal.swc

 1. [Download playerglobal.swc][3] - This is on the same page as the content debugger you just downloaded.
 2. Copy it to:
<!-- language: lang-bash -->
    /ApacheFlex/dependencies/PlayerGlobal/player/12.0
where the number corresponds to the version you download.
 3. Rename copied file to 'playerglobal.swc'
 4. Set the `PLAYERGLOBAL_HOME` environment variable to:
<!-- language: lang-bash -->
    env.PLAYERGLOBAL_HOME=/ApacheFlex/Dependencies/PlayerGlobal/player

    
## Adobe Pixel Bender Toolkit
 1. Download Pixel Bender for [Windows][4] or for [Mac][5]
 2. Install Pixel Bender. When you install the program it gives you the option to choose the install location. Copy this location before you install.
 3. Set the `PIXELBENDER_HOME` environment variable to the installation directory.

    
# Prepare System

For testing, the Adobe Flash Player's `mm.cfg` file must have the proper properties set and a `FlashPlayerTrust` file must allow local SWFs to access local files.

 1. Open the `mm.cfg` file in an editor. The location of the `mm.cfg` file depends on your operating system. [See this page][2]. 
 2. If it doesn't exist, manually launch the Adobe Flash Player Projector content debugger and it should create the `mm.cfg` at that time.
 3. Add or verify that the following entries are in the file:
<!-- language: lang-bash -->
    ErrorReportingEnable=1
    TraceOutputFileEnable=1
 4. In the same directory as the mm.cfg file is the FlashPlayerTrust directory.
 5. Open that directory and create a new text file called ApacheFlex.cfg.
 6. In that file add the parent directory of Apache Flex SDK. For example:
<!-- language: lang-bash -->

    #Path to Apache Flex Source
    /ApacheFlex/source
 7. Save the file.
 
 
# Set the Build Properties

 1. In the flex-sdk directory is the build properties file. This contains the version numbers of the software you downloaded.
 2. Open this file and set the following values to the version of the Flash Player and AIR SDK you are using:
<!-- language: lang-bash -->
    # Flash player version for which player global swc to use
    playerglobal.version = 12.0
 
    # AIR version number
    air.version = 4.0
These values are used in building the path to the SDK. So if you saved the playerglobal.swc to the following directory:
    /ApacheFlex/dependencies/PlayerGlobal/player/12.0
the value of the playerglobal.version would be 12.0. 

>Note: Since this value builds the path be sure to use the full value, 12.0, instead of 12 if your path contains it.

> Note: If you are using a new version of the Flash Player that is not yet mentioned in the version of the Flex SDK you are working with you may get the message:
    Error: configuration variable 'swf-version' value contains unknown token 'playerglobal.swfversion'
You'll need to update the `/ApacheFlex/source/flex-sdk/frameworks/build.xml` file with an updated version check code. 






  [1]: http://flex.apache.org/installer.html
  [2]: http://helpx.adobe.com/flash-player/kb/configure-debugger-version-flash-player.html
  [3]: http://www.adobe.com/support/flashplayer/downloads.html
  [4]: http://www.adobe.com/go/pixelbender_toolkit_zip/
  [5]: http://www.adobe.com/go/pixelbender_toolkit_dmg/

## Hello World (runs in browser)
    <?xml version="1.0" encoding="utf-8"?>
    <s:Application xmlns:fx="http://ns.adobe.com/mxml/2009" 
        xmlns:s="library://ns.adobe.com/flex/spark" 
        xmlns:mx="library://ns.adobe.com/flex/mx">
    
        <s:Label text="Hello World!" />
    
    </s:Application>


## Hello World (runs in application)
    <?xml version="1.0" encoding="utf-8"?>
    <s:WindowedApplication xmlns:fx="http://ns.adobe.com/mxml/2009" 
                           xmlns:s="library://ns.adobe.com/flex/spark" 
                           xmlns:mx="library://ns.adobe.com/flex/mx">

        <s:Label text="Hello World!" />

    </s:WindowedApplication>


