---
title: "Publishing your Ionic app"
slug: "publishing-your-ionic-app"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Building release version from macOS
**Build the APK**

First of all we need to build the APK.

`ionic build --release android`       

**Generate private Key**    

Then we will create a keystore to sign the APK.
`keytool -genkey -v -keystore my-release-key.keystore -alias alias_name -keyalg RSA -keysize 2048 -validity 10000`

* Change **my-release-key** with your key name.
* Change **alias_name** with your key alias.

**Sign the APK**

`jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore my-release-key.keystore HelloWorld-release-unsigned.apk alias_name`

* Change **my-release-key** with your key name.
* Change **HelloWorld-release-unsigned** with your unsigned apk. `ionic-project/platforms/android/build/outputs/apk`.
* Change alias_name with your key alias.

**Zip the APK**

`zipalign -v 4 HelloWorld-release-unsigned.apk HelloWorld.apk`

* You can find zipalign in `/Users/username/Library/Android/sdk/build-tools/XXX/`  
* Change **HelloWorld-release-unsigned** with your unsigned apk. `ionic-project/platforms/android/build/outputs/apk`.  
* Change **HelloWorld.apk** with your prefered apk file name. This will be uploaded to Google Play.

