---
title: "Sign your Android App for Release"
slug: "sign-your-android-app-for-release"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Android requires that all APKs be signed for release.



## Sign your App
1. In the menu bar, click Build > Generate Signed APK.

2. Select the module you would like to release from the drop down and click Next.

3. To Create a new keystore, click Create new. Now fill the required information and press ok in New Key Store.

[![New Key Store][1]][1]

[![Generate Signed APK][2]][2]

4. On the Generate Signed APK Wizard fields are already populated for you if you just created new key store otherwise fill it and click next.

5.  On the next window, select a destination for the signed APK, select the build type
and click finish.


  [1]: https://i.stack.imgur.com/LcUK6.png
  [2]: https://i.stack.imgur.com/am4I5.png

## Configure the build.gradle with signing configuration
You can define the  signing configuration to sign the apk in the `build.gradle` file.

You can define:

- `storeFile` : the keystore file
- `storePassword`: the keystore password
- `keyAlias`: a key alias name
- `keyPassword`: A key alias password

You have to **define** the `signingConfigs` block to create a signing configuration:

    android {
        signingConfigs {
    
            myConfig {
                storeFile file("myFile.keystore")
                storePassword "xxxx"
                keyAlias "xxxx"
                keyPassword "xxxx"
            }
        }
        //....
    }

Then you can **assign** it to one or more build types.

    android {
    
        buildTypes {
            release {
                signingConfig signingConfigs.myConfig
            }
        }
    }

