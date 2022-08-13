---
title: "Debugging the application"
slug: "debugging-the-application"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Important thing to remember when debugging cordova apps, if you have an OnDeviceReady event and code that executes there, by the time the app launches, your debugger will still not be attached(unlike say Visual Studio C# debugging where application waits for the debug process to attach before continuing with launching the program).

This means that any initial set up console messages or breakpoints will not be captured.

Solution for this can be a delayed set up or delayed console logging with setTimeout when DeviceReady event is fired.

## Debug on Android Device using USB
A Cordova application runs as a website on a WebView component within the native mobile platform. Debugging a cordova application can therefore be done by utilizing your favourite browsers development tools. The following steps are needed to hook the application, running on the device, to the Chrome browser on a development machine:

 1. Enable USB Debugging on your Device ([you can follow this guide][1])
 2. Install the Android Debug Bridge `adb` (not required on recent versions of Chrome) ([guide for OSX][2])
 3. Connect your phone and execute `adb devices` in your terminal (not required on recent versions of Chrome), and select `ok` in the popup on your phone `Allow USB debugging?`. 
 4. Open Chrome
 5. Browse to `chrome://inspect`, or choose [More tools => Inspect Devices...][3]
 5. Select your device and debug using Chrome's developer tools

Depending on your device you may need to download USB drivers first. 

You also need to enable 'unknown sources' under Security in Settings if you want to load the app on to your phone.

  [1]: https://www.kingoapp.com/root-tutorials/how-to-enable-usb-debugging-mode-on-android.htm
  [2]: http://stackoverflow.com/questions/31374085/installing-adb-on-mac-os-x
  [3]: https://developers.google.com/web/tools/chrome-devtools/debug/remote-debugging/remote-debugging?hl=en

## Debug on iOS device using USB
 1. **Disable Private Browsing**
    
Open your device’s Safari settings and ensure that **Private Browsing is turned off**. Remote debugging will not work if _Private Browsing_ is enabled.
 
 2. **Enable Web Inspector**

Tap the _Advanced_ tab on your device’s Safari settings and ensure that **Web Inspector is turned on**.

 3. **Enable Safari’s Develop Menu**

On your desktop or laptop, open Safari’s Preferences and click on the Advanced tab. Check the box to **Show Develop menu in menu bar**.

 4. **Start Web Inspector**

Launch your app either in the iOS simulator or on a physical device. If you are using a physical device you’ll need to connect it to your desktop or laptop with the standard USB cable. Once the app has launched, switch to Safari, select the **Develop** menu item, then find the entry corresponding to the web page you want to debug.

[![enter image description here][1]][1]

Now you can use web inspector just like you would to debug a web page.

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/YDTyl.png
  [2]: http://i.stack.imgur.com/KaGnp.png

## Debug Cordova apps using GapDebug
https://www.genuitec.com/products/gapdebug/ 

GapDebug is a comprehensive mobile debugging tool that bridges the gap left by other debugging options. Operating on both the Windows and Mac platforms, GapDebug allows debugging of hybrid mobile apps, such as PhoneGap and Cordova, on modern iOS and Android devices. And, GapDebug is always free for local debugging.

Step for first time configuration are given in this following link:

https://www.genuitec.com/products/gapdebug/learning-center/configuration/



