---
title: "From code to the App store - Android"
slug: "from-code-to-the-app-store---android"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

You will find step by step instructions on how to prepare and upload production ionic app onto Google Play.

## Production ready
**Creating app project**

When creating an Android app ready for the app store it's important
when using `ionic start` that we add `--appname|-a` and `--id|-i` flags
which is used for google play to identify your app from other apps.

If you're starting a new mobile app project you can use the cli example below.

```sh
$ ionic start --v2 -a "App Example" -i "com.example.app" -t "tabs"
```

 **1. App configuration file**

if you want to set this info inside an existing app you can modify `config.xml`.
I recommend those who used the command above to modify `config.xml` as well.

Confirm/edit `widget id`, `name`, `description`, and `author` attributes.

Example:

    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    <widget id="com.example.app" version="1.0.0" xmlns="http://www.w3.org/ns/widgets" xmlns:cdv="http://cordova.apache.org/ns/1.0">
      <name>Example App</name>
      <description>Example app for stackoverflow users</description>
      <author email="admin@example.com" href="http://example.com/">Your name or team</author>
      ...
    </widget>

**2. icon and splash screen**

Both icon and splash image supported file types are png, psd or ai and must have a file name that corresponds to what it is `icon` or `splash` and placed under the resources dir at the root of your project. The icon image’s minimum dimensions should be 192x192 px, and should have no rounded corners. and the splash screen is much more complicated so click here to read more. Nonetheless, minimum dimensions should be 2208x2208 px.

if you have icon file to generate use this command `ionic resources --icon`
if you have splash file to generate use this command `ionic resources --splash`

**3. Building production app**

Before building your production app remove any sensitive log data.

To build a release version with all default optimizations in place use the --release & --prod tag

 `ionic build android --release --prod`

For a full list of available optimizations you may visit the [@ionic/app-scripts repository][1]

**4. Create private key**

Now, we need to sign the unsigned APK (`android-release-unsigned.apk`) and run an alignment utility on it to optimize it and prepare it for the app store. If you already have a signing key, skip these steps and use that one instead.


Next, locate your unsigned APK file `android-release-unsigned.apk` inside project dir`/platforms/android/build/outputs/apk/` and use 
`keytools` command that will be used to sign our apk file. You can use the example below:

    $ keytool -genkey -v -keystore my-release-key.keystore -alias androidKey -keyalg RSA -keysize 2048 -validity 10000

you can find `my-release-key.keystore` in your current directory.

Let’s generate our private key using the keytool command that comes with the JDK. If this tool isn’t found, refer to the installation guide:

You’ll first be prompted to create a password for the keystore. Then, answer the rest of the nice tools’s questions and when it’s all done, you should have a file called my-release-key.keystore created in the current directory.

Note: Make sure to save this file somewhere safe, if you lose it you won’t be able to submit updates to your app!

 **5. Sign APK**

To sign the unsigned APK, run the jarsigner tool which is also included in the JDK:

    $ jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore my-release-key.keystore HelloWorld-release-unsigned.apk alias_name

This signs the apk in place. Finally, we need to run the zip align tool to optimize the APK. The zipalign tool can be found in /path/to/Android/sdk/build-tools/VERSION/zipalign.

    $ zipalign -v 4 HelloWorld-release-unsigned.apk HelloWorld.apk

Now we have our final release binary called HelloWorld.apk and we can release this on the Google Play Store for all the world to enjoy!

**Publish your app on Google Play Store.** Now that we have our release APK ready for the Google Play Store, we can create a Play Store listing and upload our APK. To start, you'll need to visit the Google Play Store Developer Console and create a new developer account. It will cost $25 one time fee.

Once you have a developer account, you can go ahead and click "Publish an Android App on Google Play" and follow the on-screen instruction.


  [1]: https://github.com/driftyco/ionic-app-scripts

