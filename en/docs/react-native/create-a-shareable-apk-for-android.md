---
title: "Create a shareable APK for android"
slug: "create-a-shareable-apk-for-android"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Steps to create an APK (signed and unsigned) which you can install on a device using CLI and share as well:

**Problem statement:** I've built my app, I can run it on my local emulator (and also on my android device by changing debug server).
But, I want to build an apk that I can send to someone without access to development server and I want them to be able to test application.

A more detailed description is also mentioned here: https://facebook.github.io/react-native/docs/signed-apk-android.html

## Create a key to sign the APK
    keytool -genkey -v -keystore my-app-key.keystore -alias my-app-alias -keyalg RSA -keysize 2048 -validity 10000

Use a password when prompted

## Once the key is generated, use it to generate the installable build:
    react-native bundle --platform android --dev false --entry-file index.android.js \
    --bundle-output android/app/src/main/assets/index.android.bundle \
    --assets-dest android/app/src/main/res/



## Generate the build using gradle
    cd android && ./gradlew assembleRelease

## Upload or share the generated APK
Upload the APK to your phone. The -r flag will replace the existing app (if it exists)

    adb install -r ./app/build/outputs/apk/app-release-unsigned.apk

The shareable signed APK is located at:

    ./app/build/outputs/apk/app-release.apk

