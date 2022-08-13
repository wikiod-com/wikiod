---
title: "Run an app on device (Android Version)"
slug: "run-an-app-on-device-android-version"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Troubleshootings : <br>
`Could not connect to development server` => Do this : `adb reverse tcp:8081 tcp:8081`, make sure that your phone is connected (adb devices). Verify also that there is a local server launched, if not run `react-native start`

## Running an app on Android Device.
1) `adb devices`
    - Is your phone displaying? If not, enable developer mode on your phone, and connect it by USB.
2) `adb reverse tcp:8081 tcp:8081` : 
    - In order to link correctly your phone and that React-Native recognize him during build. (**NOTE:`Android Version 5` or above.**)
3) `react-native run-android` : 
    - To run the app on your phone.
4) `react-native start` : 
    - In order to start a local server for development (mandatory). This server is automatically started if you use the last version of React-native.

