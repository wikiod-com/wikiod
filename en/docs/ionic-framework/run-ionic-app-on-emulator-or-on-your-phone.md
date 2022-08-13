---
title: "Run Ionic App on Emulator or on your Phone"
slug: "run-ionic-app-on-emulator-or-on-your-phone"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Run Ionic App on Emulator or on your Phone

## **1. Add a platform target** ## 

**iOS:**

    $ ionic platform add ios 
    
**Android:**

    $ ionic platform add android

**Windows:**

    $ ionic platform add windows
___
## **2. Build your app** ## 
 

**iOS:**

    $ ionic build ios 
    
**Android:**

    $ ionic build android

**Windows:**

    $ ionic build windows


____

## Live Reload App During Development (beta)

The `run` or `emulate` command will deploy the app to the specified platform devices/emulators. You can also run **live reload** on the specified platform device by adding the `--livereload` option. The live reload functionality is similar to `ionic serve`, but instead of developing and debugging an app using a standard browser, the compiled hybrid app itself is watching for any changes to its files and reloading the app when needed. This reduces the requirement to constantly rebuild the app for small changes. However, any changes to plugins will still require a full rebuild. For live reload to work, the dev machine and device must be on the same local network, and the device must support [web sockets](http://caniuse.com/websockets).

With live reload enabled, an app’s console logs can also be printed to the terminal/command prompt by including the `--consolelogs` or `-c` option. Additionally, the development server’s request logs can be printed out using `--serverlogs` or `-s` options.

### Command-line flags/options for `run` and `emulate`

    [--livereload|-l] .......  Live Reload app dev files from the device (beta)
    [--consolelogs|-c] ......  Print app console logs to Ionic CLI (live reload req.)
    [--serverlogs|-s] .......  Print dev server logs to Ionic CLI (live reload req.)
    [--port|-p] .............  Dev server HTTP port (8100 default, live reload req.)
    [--livereload-port|-i] ..  Live Reload port (35729 default, live reload req.)
    [--debug|--release]

While the server is running for live reload, you can use the following commands within the CLI:

    restart or r to restart the client app from the root
    goto or g and a url to have the app navigate to the given url
    consolelogs or c to enable/disable console log output
    serverlogs or s to enable/disable server log output
    quit or q to shutdown the server and exit
____    
## **3. Emulating your app** ## 
Deploys the Ionic app on specified platform emulator. This is simply an alias for `run --emulator`.

**iOS:**

    $ ionic emulate ios [options]

**Android:**

    $ ionic emulate android [options]

**Windows:**

    $ ionic emulate windows [options]

During emulating app in AVD or mobiles, you can inspect that app in chrome browser. Type following command in address bar of the chrome browser.
    
    chrome://inspect/#devices
___
## **4. Running your app** ## 

Deploys the Ionic app on specified platform devices. If a device is not found it'll then deploy to an emulator/simulator.

**iOS:**

    $ ionic run ios [options]

**Android:**

    $ ionic run android [options]

**Windows:**

    $ ionic run windows [options]

## ***4.1. Specifying  your target*** ## 

`$ ionic run [ios/android/windows] --target="[target-name]"`

You can check the target name of your device/emulator running `$ adb devices`.

