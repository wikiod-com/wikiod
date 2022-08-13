---
title: "What's the difference between “ionic build” and “ionic prepare”?"
slug: "whats-the-difference-between-“ionic-build”-and-“ionic-prepare”"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## ionic build vs ionic prepare
From the official documentation:

> If you want to get advanced, you can also open up the project file for a specific platform by opening the required XCode or Android Eclipse project in platforms/PLATFORM inside the root of your project. Then, you can build and test from inside the platform-specific IDE. Note: if you go this route, I recommend still working inside of the root www folder, and when you've made changes to this folder, run the command: `$ cordova prepare ios` which will update the iOS specific project with the code from the `www` folder. Note: this will overwrite any changes you've made to the `platforms/ios/www` and other platform-specific folders.

So, to summarize this part - if you're using XCode to test and run your code, after you change some part of the code you just have to run `ionic prepare` to update the iOS project which then again you continue to use in XCode.

`ionic build` command actually prepares the final (for example in Android it's the .apk file) file which then could be copied to your device and test by running it manually on the device (or by using the `ionic emulate` command to test it on the emulator).

