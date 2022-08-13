---
title: "Sign Android build with Cordova 5"
slug: "sign-android-build-with-cordova-5"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Add the build configuration to sign the .apk file
1. Add a keystore using:

       keytool -genkey -v -keystore example.keystore -alias example -keyalg RSA -keysize 2048 -validity 10000 

Note: This should be at root of project. Though not a hard requirement, it eases the file referencing


2. Add a build.json with release/dev configuration for keystore, at the root of project:

       {
         "android": {
           "debug": {
             "keystore": "..\android.keystore",
             "storePassword": "android",
             "alias": "mykey1",
             "password" : "password",
             "keystoreType": ""
           },
           "release": {
             "keystore": "..\android.keystore",
             "storePassword": "",
             "alias": "mykey2",
             "password" : "password",
             "keystoreType": ""
           }
         }
       }

3. Add the --buildConfig switch to Cordova/ Ionic build command:

       cordova build android --release --buildConfig=build.json

or with Ionic as

      ionic build android --release --buildConfig=build.json

The signed file will be generated under the new folder structure at

    /platforms/android/build/outputs/apk/android-release.apk

