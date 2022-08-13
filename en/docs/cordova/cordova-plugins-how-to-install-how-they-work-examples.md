---
title: "Cordova plugins how to install, how they work, examples"
slug: "cordova-plugins-how-to-install-how-they-work-examples"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## What are cordova plugins?
Cordova plugins are in simple words a layer on top of the respective native platform.

Plugins provide an interface between to access the native platform. 

## How can Cordova plugins be useful?
Cordova Plugins provide a common interface to interact with the native code.

Each plugin has an intermediary JavaScript file that provides access to platform specific features.  

## Installing Cordova Plugin
  
    cordova plugin add <plugin-name>

   Example: cordova plugin add cordova-plugin-camera

   The plugin should be installed in the root directory of the project. 

   Note: 

> Before adding the plugin replace the platform specific www folder
> content with the outer www folder in the root directory.   This is
> because on adding a plugin the contents of the outer www folder are
> replaced with the platform specific www folder.

   Note: 

> When you add a new platform and if you have any installed plugins on your project is not necessary to install the existing plugins. Cordova automatically will add the installed plugins for the new platform.

 

## Most Popular Plugins
 1. **[cordova-plugin-battery-status][1]** - used for monitoring device's battery status.

 2. **[cordova-plugin-camera][2]** - provides an API for taking pictures and for choosing images from the system's image library.

 3. **[cordova-plugin-contacts][3]** - provides access to the device contacts database.

 4. **[cordova-plugin-device][4]** - describes the device's hardware and software.

 5. **[cordova-plugin-device-motion][5]** - access to the device's accelerometer.

 6. **[cordova-plugin-file][6]** - implements a File API allowing read/write access to files residing on the device.

 7. **[cordova-plugin-geolocation][7]** - provides information about the device's location, such as latitude and longitude.

 8. **[cordova-plugin-globalization][8]** - obtains information and performs operations specific to the user's locale, language, and timezone.

 9. **[cordova-plugin-inappbrowser][9]** - show helpful articles, videos, and web resources inside of your app. Users can view web pages without leaving your app.

 10. **[cordova-plugin-network-information][10]** - provides information about the device's cellular and wifi connection, and whether the device has an internet connection.

 11. **[cordova-plugin-vibration][11]** - provides a way to vibrate the device.

 12. **[cordova-plugin-statusbar][12]** - provides some functions to customize the iOS and Android StatusBar.

 13. **[cordova-plugin-whitelist][13]** - implements a whitelist policy for navigating the application webview on Cordova 4.0. **Recommended plugin!**

For cordova specific plugins follow below link https://cordova.apache.org/plugins/


  [1]: https://github.com/apache/cordova-plugin-battery-status
  [2]: https://github.com/apache/cordova-plugin-camera
  [3]: https://github.com/apache/cordova-plugin-contacts
  [4]: https://github.com/apache/cordova-plugin-device
  [5]: https://github.com/apache/cordova-plugin-device-motion
  [6]: https://github.com/apache/cordova-plugin-file
  [7]: https://github.com/apache/cordova-plugin-geolocation
  [8]: https://github.com/apache/cordova-plugin-globalization
  [9]: https://github.com/apache/cordova-plugin-inappbrowser
  [10]: https://github.com/apache/cordova-plugin-network-information
  [11]: https://github.com/apache/cordova-plugin-vibration
  [12]: https://github.com/apache/cordova-plugin-statusbar
  [13]: https://github.com/apache/cordova-plugin-whitelist

