---
title: "Make application released from Cordova CLI"
slug: "make-application-released-from-cordova-cli"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Android
***Step 1:*** Go to root directory of project and open command line prompt
    
     cordova build --release android

This generates an unsigned apk under \platforms\android\build\outputs\apk with the name 

**android-release-unsigned.apk**

***Step 2:*** Key generation for obtaining signed apk
        
   Syntax:

     keytool -genkey -v -keystore <keystoreName>.keystore -alias <Keystore AliasName> -keyalg <Key algorithm> -keysize <Key size> -validity <Key Validity in Days>

   Example:

    keytool -genkey -v -keystore ExampleApp.keystore -alias TestExampleApp -keyalg RSA -keysize 2048 -validity 10000


    keystore password? : xxxxxxx
    What is your first and last name? :  xxxxxx
    What is the name of your organizational unit? :  xxxxxxxx
    What is the name of your organization? :  xxxxxxxxx
    What is the name of your City or Locality? :  xxxxxxx
    What is the name of your State or Province? :  xxxxx
    What is the two-letter country code for this unit? :  xxx

   
The keystore is generated in the same folder with the name ExampleApp.keystore


**Step 3:** Move the generated keystore to ***\platforms\android\build\outputs\apk***

Run the jarsigner tool in the command prompt under ***\platforms\android\build\outputs\apk***
 
Syntax:

    jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore <keystorename <Unsigned APK file> <Keystore Alias name>   

Example:

    jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore ExampleApp.keystore android-release-unsigned.apk TestExampleApp

This generates the signed apk with the same name.

***Step 4:*** zip align tool to optimize the APK

     zipalign -v 4 android-release-unsigned.apk android.apk

The zipalign is located under \Android\sdk\build-tools\23.0.3\zipalign

This generates a signed apk with the name android.apk which can now be uploaded to app store

## iOS
***Step 1:*** Create a build.json file in the root directory of the project.

Sample of build.json

    {
      "ios": {
        "debug": {
          "codeSignIdentity": "iPhone Developer",
          "provisioningProfile": "your-developer-provisioning-profile-UUID-here"
        },
        "release": {
          "codeSignIdentity": "iPhone Distribution",
          "provisioningProfile": "your-distribution-provisioning-profile-UUID-here"
        }
      }
    }
    

> Note: The UUID can be obtained by opening the .mobileprovision file on
> a text editor and search for 'UUID'.

***Step 2:*** Run the following command from the root folder of the project on the terminal

    cordova build ios --device --release



