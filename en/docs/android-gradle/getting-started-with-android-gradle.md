---
title: "Getting started with android-gradle"
slug: "getting-started-with-android-gradle"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Android Plugin for Gradle
As described in the remarks section the Android build system uses the Android Plugin for Gradle to support building Android applications with Gradle.

You can specify the Android Plugin for Gradle version in the top-level `build.gradle` file. The plugin version applies to all modules built in that Android Studio project.

    buildscript {
      ...
      dependencies {
        classpath 'com.android.tools.build:gradle:2.2.0'
      }
    }

## Gradle wrapper
As described in the remarks section you can specify the Gradle version used by each project editing the Gradle distribution reference in the `gradle/wrapper/gradle-wrapper.properties` file. 

For example:

    ...
    distributionUrl = https\://services.gradle.org/distributions/gradle-2.14.1-all.zip
    ...

## Initial Setup with Android Studio
To setup for using Android Gradle Plugin you need many things:

 - java
 - gradle
 - the Android project folder structure
 - an Android Manifest
 - initial plugin setup

The easiest way to get all of them is to follow these steps:

 1. Donwload and Install [Java OpenJDK version 6 or 7][1] (you can use 8 with additional settings of the gradle plugin)
 2. Download and Install [Android Studio][2]
 3. Create a new project (if you need help see [Creating a New Project][3])

Check *Remarks* section for more informations.

  [1]: http://openjdk.java.net/install/
  [2]: https://developer.android.com/studio/index.html
  [3]: https://www.wikiod.com/android/getting-started-with-android#Creating a New Project

