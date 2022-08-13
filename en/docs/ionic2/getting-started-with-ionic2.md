---
title: "Getting started with ionic2"
slug: "getting-started-with-ionic2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Since Ionic 2 is getting better and better every day, please always check the **[official documentation](http://ionicframework.com/getting-started)** to keep track of the latest changes and improvements.

**Prerequisites:**
You will need NodeJS in order to build Ionic 2 projects. You can download and install node [here](https://nodejs.org/en/) and learn more about npm and the packages Ionic 2 uses [here](http://ionicframework.com/docs/resources/using-npm/).
---
## **1. Installing Ionic 2**

Like Ionic 1, you can use the Ionic CLI or GUI to quickly build and test apps right in the browser. It even has all the functionality to work with your Ionic 1 apps, so you won't need to change a thing!

To use Ionic 2 simply install ionic from npm:

    $ npm install -g ionic

If you get an EACCES error, follow the instructions [here](https://docs.npmjs.com/getting-started/fixing-npm-permissions) to give node the permissions it needs.
---
## **2. Creating Your First App**

Once the CLI is installed, run the following command to start your first app:

    $ ionic start MyIonic2Project

The [tabs template](https://github.com/driftyco/ionic2-starter-tabs) is used by default, but you can choose another template by passing in a flag. For example:

    $ ionic start MyIonic2Project tutorial
    $ cd MyIonic2Project
    $ npm install

This will use the [tutorial](https://github.com/driftyco/ionic2-starter-tutorial) template.

To run your app, change into your projects directory and run `ionic serve -lc`:

    $ ionic serve -lc

The -l activates the live reload of the page, the -c displays the console logs.
If you're having issues building your app, make sure your package.json matches the one in the [ionic2-app-base](https://github.com/driftyco/ionic2-app-base/blob/master/package.json)

You can play with your new app right there in the browser!
---
## **3. Building to a Device** 

You can also build your new app on a physical device or a device emulator. You will need [Cordova](http://ionicframework.com/docs/v2/resources/what-is/#cordova) to proceed. 

To install Cordova, run:

    $ npm install -g cordova

Check out the [iOS simulator ](http://ionicframework.com/docs/v2/resources/developer-tips/#using-ios-simulator) docs for building iOS applications (NOTE: you cannot build to iOS devices or emulators on any operating system other than OSX), or the [Genymotion](http://ionicframework.com/docs/v2/resources/developer-tips/#using-genymotion-android) docs to build an Android application.

**Running on iOS device:**
  
To build an iOS app, it is necessary for you to work on an OSX computer, because you will need the cocoa framework to be able to build for ios, if it's the case you will first need to add the platform to cordova by running the following command:

    $ ionic cordova platform add ios

You will need [Xcode](http://ionicframework.com/docs/v2/resources/what-is/#xcode) to compile to an iOS device.

Finally, run your app with the following command:

    $ ionic cordova run ios

**Running on an Android device:**  

The steps for Android are almost identical. First, add the platform:

    $ ionic cordova platform add android

Then install the [Android SDK](http://ionicframework.com/docs/v2/resources/what-is/#android-sdk) which allows you to compile to an Android device. Although the Android SDK comes with an emulator, it's really slow. [Genymotion](http://ionicframework.com/docs/v2/resources/what-is/#genymotion) is much faster. Once installed, simply run the following command:

    $ ionic cordova run android

And that's it! Congratulations on building your first Ionic 2 app!

Ionic has live reloading too. So if you want to develop your app and see changes taking place live on the emulator / device, you can do that by running the following commands:
  
**For iOS:**

    $ ionic cordova emulate ios -lcs

Be careful, on iOS 9.2.2 the livereload doesn't work. If you want to work with livereload, edit the config.xml file by adding the following :

    <allow-navigation href="*"/>

Then in the `<platform name="ios">` :

    <config-file parent="NSAppTransportSecurity" platform="ios" target="*-Info.plist">
      <dict>
        <key>NSAllowsArbitraryLoads</key>
        <true/>
      </dict>
    </config-file>

**For Android:**

    $ ionic cordova run android -lcs

The `l` stands for live-reload, `c` for console logs, and `s` for server logs.
This will allow you to see if there are any errors / warnings during execution.

**Building for Windows**

If you want to build your project for windows, you need to work on a windows computer. To start, install the windows platform to your ionic2 project by running the following command : 

    $ionic cordova platform add windows

Then just run the following command : 

    $ionic cordova run windows
To run in browser
    
    $ionic serve

for chrome browser inspect device.(type in address bar of chrome browser)
    
    chrome://inspect/#devices

