---
title: "Shrink Code and Resources"
slug: "shrink-code-and-resources"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

To make your APK file as small as possible, you should enable shrinking to remove unused code and resources in your release build.

## Shrink the code with ProGuard
To enable code shrinking with ProGuard, add `minifyEnabled` true to the appropriate build type in your `build.gradle` file.

    android {
        buildTypes {
            release {
                minifyEnabled true
                proguardFiles getDefaultProguardFile(‘proguard-android.txt'),
                        'proguard-rules.pro'
            }
        }
    }

where:

- `minifyEnabled true` : enable code shrinking
- The `getDefaultProguardFile(‘proguard-android.txt')` method gets the default ProGuard settings from the Android SDK
- The `proguard-rules.pro` file is where you can add custom ProGuard rules



## Remove unused alternative resources
All libraries come with resources that are not necessary useful to your application.
For example Google Play Services comes with translations for languages your own application don’t even support.

You can configure the build.gradle file to specify which resource you want to keep.  
For example:

    defaultConfig {
        // ...
    
        resConfigs "en", "de", "it"
        resConfigs "nodpi", "xhdpi", "xxhdpi", "xxxhdpi"
    }

## Shrink the resources
To enable resource shrinking, set the `shrinkResources` property to true in your `build.gradle` file.

    android {
        ...
    
        buildTypes {
            release {
                minifyEnabled true
                shrinkResources true
                proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
            }
        }
    }

Pay attention because resource shrinking **works only** in conjunction with [code shrinking][1].

You can customize which resources to keep or discard creating an XML file like this:

    <?xml version=1.0" encoding="utf-8"?>
    <resources xmlns:tools="http://schemas.android.com/tools"
        tools:keep="@layout/mylayout,@layout/custom_*"
        tools:discard="@layout/unused" />

Save this file in `res/raw` folder.


  [1]: https://www.wikiod.com/android-gradle/shrink-code-and-resources#Shrink the code with ProGuard

