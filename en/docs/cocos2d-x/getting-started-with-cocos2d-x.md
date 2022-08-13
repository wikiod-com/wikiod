---
title: "Getting started with cocos2d-x"
slug: "getting-started-with-cocos2d-x"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup(Mac OS X)
**OVERVIEW**
------------

cocos2d-x is an open source, cross-platform game engine. It allows developers to code in C++, Lua and Javascript deployment into iOS, Android, Windows Phone, Mac OS X, Windows Desktop and Linux.


----------


**PREREQUISITES**
-----------------
**Build Requirements**

 - Mac OS X 10.7+, Xcode 4.6+
 - Windows 7+, VS 2012+
 - Ubuntu 12.10+, CMake 2.6+
 - Python 2.7.5
 - NDK, JDK, ANT (Android development)


**Runtime Requirements**
 - iOS 5.0+ for iPhone / iPad games
 - Android 2.3+ for Android games
 - Windows Phone 8+ for Windows Phone games
 - OS X v10.6+ for Mac games
 - Windows 7+ for Windows games


----------

**Installation**
------------
Download everything below:
 - [Cocos2d-x v3.x][1]
 - [JDK/SDK 1.6+][2]
 - [Android command-line tools][3]
 - [NDK][4]
 - [Apache Ant][5]
 - [Python 2.7.5][6]


  [1]: http://cocos2d-x.org/download
  [2]: http://www.oracle.com/technetwork/java/javase/downloads/index.html
  [3]: http://%20https://developer.android.com/studio/index.html
  [4]: https://developer.android.com/tools/sdk/ndk/index.html
  [5]: http://ant.apache.org/bindownload.cgi
  [6]: https://www.python.org/downloads/

After downloading everything above,

 - unzip Cocos2d-x
 - install the JDK.
 - unzip the Android SDK and NDK to the same root location. (maybe:
   ~/AndroidDev)
 - verify that Python 2.7 is installed and is accessible.
 - install Apache Ant and verify that it works.

> Inside your Cocos2d-x directory run:

 `python setup.py`

**caution**: You must **not** use the ~ sign. Use the full path to your **home** directory. Otherwise, the scripts will fail due to error path value.
 
- Now it will ask for **NDK_ROOT** path. Here, enter the extracted NDK directory
   path: 

> ~/android-ndk-r10e

 - **SDK_ROOT**. Here, enter the Android SDK path:

> ~/android-sdk-macosx

 - **ANT_ROOT**. Here, enter the Apache ANT bin path:

> ~/apache-ant-1.9.6/bin

After setting the above environment variables,run:
 

    source ~/.bash_profile

Vola, the cocos2d-x has been successfully setup on your system.


----------


## Installation or Setup(Windows)
**OVERVIEW**
------------

cocos2d-x is an open source, cross-platform game engine. It allows developers to code in C++, Lua and Javascript deployment into iOS, Android, Windows Phone, Mac OS X, Windows Desktop and Linux.


----------


**PREREQUISITES**
-----------------
**Build Requirements**

 - Mac OS X 10.7+, Xcode 4.6+
 - Windows 7+, VS 2012+
 - Ubuntu 12.10+, CMake 2.6+
 - Python 2.7.5
 - NDK, JDK, ANT (Android development)


**Runtime Requirements**
 - iOS 5.0+ for iPhone / iPad games
 - Android 2.3+ for Android games
 - Windows Phone 8+ for Windows Phone games
 - OS X v10.6+ for Mac games
 - Windows 7+ for Windows games


----------

**Installation**
------------
Download everything below:
 - [Cocos2d-x v3.x][1]
 - [JDK/SDK 1.6+][2]
 - [Android command-line tools][3]
 - [NDK][4]
 - [Apache Ant][5]
 - [Python 2.7.5][6]


After downloading everything above,

 - unzip Cocos2d-x
 - install the JDK and at the same time create a new variable called: JAVA_HOME and give it the path to where you installed the JDK above.
[![enter image description here][7]][7]
 - unzip the Android SDK and NDK to the same root location. (maybe:
   ~/AndroidDev)
 - install Python and take note of where you placed it.
 - extract Apache Ant and place the entire folder where you wish to keep it. Take note of where you placed it.
 - add the paths for Apache Ant and Python to your PATH variable.
[![enter image description here][8]][8]
 - reboot
 - now, test your environment before continuing. Launch a command prompt and execute:

   ` ant `

    `python`


----------
If everything works it is time to run setup.py to configure your Android development environment. This will set the necessary environment variables needed. If you haven't configured this environment before, you will be prompted to enter paths for variables that are not found. You run setup.py from the directory Cocos2d-x is in.

Inside your Cocos2d-x directory run:

 `python setup.py`

**caution**: You must **not** use the ~ sign. Use the full path to your **home** directory. Otherwise, the scripts will fail due to error path value.
 
- Now it will ask for **NDK_ROOT** path. Here, enter the extracted NDK directory
   path: 

> ~/android-ndk-r10e

 - **SDK_ROOT**. Here, enter the Android SDK path:

> ~/android-sdk-macosx

 - **ANT_ROOT**. Here, enter the Apache ANT bin path:

> ~/apache-ant-1.9.6/bin

 - Reboot

Vola, the cocos2d-x has been successfully setup on your system.


----------


  [1]: http://cocos2d-x.org/download
  [2]: http://www.oracle.com/technetwork/java/javase/downloads/index.html
  [3]: http://%20https://developer.android.com/studio/index.html
  [4]: https://developer.android.com/tools/sdk/ndk/index.html
  [5]: http://ant.apache.org/bindownload.cgi
  [6]: https://www.python.org/downloads/
  [7]: http://i.stack.imgur.com/xfalu.png
  [8]: http://i.stack.imgur.com/9449Q.png



