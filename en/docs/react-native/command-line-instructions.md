---
title: "Command Line Instructions"
slug: "command-line-instructions"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Check version installed

    $ react-native -v

Example Output

    react-native-cli: 0.2.0
    react-native: n/a - not inside a React Native project directory //Output from  different folder
    react-native: react-native: 0.30.0 // Output from the react native project directory

## Initialize and getting started with React Native project
**To initialize**

    react-native init MyAwesomeProject

**To initialize with a specific version of React Native**

    react-native init --version="0.36.0" MyAwesomeProject

**To Run for Android**
    
    cd MyAwesomeProject
    react-native run-android

**To Run for iOS**

    cd MyAwesomeProject
    react-native run-ios

## Upgrade existing project to latest RN version
In the app folder find `package.json` and modify the following line to include the latest version, save the file and close.

    "react-native": "0.32.0"

In terminal: 

    $ npm install
Followed by

    $ react-native upgrade

## Logging
   **Android**

    $ react-native log-android

   **iOS**    

    $ react-native log-ios
    

## Start React Native Packager
    $ react-native start

On latest version of React Native, no need to run the packager. It will run automatically.

By default this starts the server at port 8081. To specify which port the server is on

    $ react-native start --port PORTNUMBER

## Add android project for your app
If you either have apps generated with pre-android support or just did that on purpose, you can always add android project to your app.

    $ react-native android

This will generate `android` folder and `index.android.js` inside your app.

