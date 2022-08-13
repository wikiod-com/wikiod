---
title: "Getting started with nativescript"
slug: "getting-started-with-nativescript"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Windows
1. Ensure you have the latest [nodeJS LTS](https://nodejs.org/en/download/) installed
2. Open command prompt and type `$ npm install -g nativescript`
3. In the command prompt type `$ @powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((new-object net.webclient).DownloadString('https://www.nativescript.org/setup/win'))"` - this might take a while
4. To verify the above has worked, type `$ tns doctor` in command prompt (your cmd)
5. If there are any errors, follow up with the [troubleshooting guide](https://docs.nativescript.org/start/troubleshooting)

## Installation or Setup
Detailed instructions on getting Nativescript set up or installed.

The following examples show the required steps to set up a Windows or OSX system and then sign post to troubleshooting guides in case you have any trouble. 

In addition, there are examples of how to set up recommended workflows, IDEs and emulators.

## macOS
1. Ensure you have the [most recent](https://nodejs.org/en/download/) Node.js LTS installed. If you use [Homebrew](http://brew.sh/) this can be done with `brew install node4-lts`.
2. Open Terminal and type `npm install -g nativescript`. If you get an `EACCES` error, use `sudo npm install -g nativescript`.
3. In the command prompt type `ruby -e "$(curl -fsSL https://www.nativescript.org/setup/mac)"`. (This might take a while.)
4. To verify that the above has worked, type `tns doctor` in Terminal.
5. If there are any errors, follow up with the [troubleshooting guide](https://docs.nativescript.org/start/troubleshooting).

## Using Visual Studio Code for NativeScript development
[Visual Studio Code](https://code.visualstudio.com) is an open-source and feature-rich code editor from Microsoft. To set it up it for NativeScript development, open the Command Palette (<kbd>F1</kbd> or <kbd>⌘</kbd>+<kbd>Shift</kbd>+<kbd>P</kbd>) and type `ext install NativeScript`.
 
Once the NativeScript extension is installed, the debugger should allow you to set breakpoints in your code. When a device is connected or an emulator is running, you can start your app from the Debug tab. ![Device selection menu][1]


  [1]: http://i.stack.imgur.com/QGpdm.png

## Your first Hello World program
    $ mkdir hello-world
    $ cd hello-world
    $ tns create hello-world --ng
    $ tns platform add android #You can only add ios on an OSX machine

Then ensure you have a device connected or an emulator running (if you don't, the default emulator should start or an error will be raised. I would recommend genymotion for android).

    $ tns run android 

If you want to use the default android emulator, add the `--emulator` flag. 

As of tns 2.5 livesync is now the default action for `tns run <platform>`, which will automatically re-compile when you save file changes. This can dramatically improve your development time, however, if you make changes to your plugins, you will need to recompile properly. 

## How to Debug nativescript-android App over WiFi (without Root)
1-You need to connect your device to your computer via USB cable. Make sure USB debugging is working. You can check if it shows up when running `adb devices`(or `tns device`).

[![enter image description here][1]][1]

2-Run `adb tcpip 5555`

[![enter image description here][2]][2]

3-Disconnect your device (remove the USB cable).

4-Go to the Settings -> About phone -> Status to view the IP address of your phone.

5-Run `adb connect <IP address of your device>:5555`

[![enter image description here][3]][3]

6-If you run `adb devices` (or `tns device`) again, you should see your device. 

[![enter image description here][4]][4]

7- Now you can use `tns run android` , `tns livesync android` commands.


**NOTES :**

1-when WiFi network changes you do not have to repeat steps 1 to 3 (these set your phone into wifi-debug mode). You do have to connect to your phone again by executing steps 4 to 6.

2-Android phones lose the wifi-debug mode when restarting. Thus, if your battery died, you have to start over. Otherwise, if you keep an eye on your battery and do not restart your phone, you can live without a cable for weeks!

**WARNING :** 

leaving the option enabled is dangerous, anyone in your network can connect to your device in debug, even if you are in data network. Do it only when connected to a trusted Wi-Fi and remember to disconnect it when done!


**reference**:

1-Norman Peitek. 2014. How to Debug Your Android App over WiFi (without Root!). [ONLINE] Available at: https://futurestud.io/blog/how-to-debug-your-android-app-over-wifi-without-root. [Accessed 8 August 2016].

2-usethe4ce. 2012. Run/install/debug Android applications over Wi-Fi?. [ONLINE] Available at: http://stackoverflow.com/a/10236938/4146943. [Accessed 8 August 2016].


  [1]: http://i.stack.imgur.com/IFF2O.png
  [2]: http://i.stack.imgur.com/zIMCq.png
  [3]: http://i.stack.imgur.com/Xz59K.png
  [4]: http://i.stack.imgur.com/t6Ojy.png

