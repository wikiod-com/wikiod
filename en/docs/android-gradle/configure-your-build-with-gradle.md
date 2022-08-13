---
title: "Configure Your Build with Gradle"
slug: "configure-your-build-with-gradle"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

The Android build system compiles app resources and source code, and packages them into APKs that you can test, deploy, sign, and distribute. Android Studio uses Gradle, an advanced build toolkit, to automate and manage the build process, while allowing you to define flexible custom build configurations.

Official Documentation
----------------------
https://developer.android.com/studio/build/index.html



## Why are there two build.gradle files in an Android Studio project?
`<PROJECT_ROOT>\app\build.gradle` is specific for app module.

`<PROJECT_ROOT>\build.gradle` is a "Top-level build file" where you can add configuration options common to all sub-projects/modules.

If you use another module in your project, as a local library you would have another build.gradle file: `<PROJECT_ROOT>\module\build.gradle`

**The Top-level Build File**

The top-level build.gradle file, located in the root project directory, defines build configurations that apply to all modules in your project. By default, the top-level build file uses the `buildscript {} block` to define the Gradle repositories and dependencies that are common to all modules in the project. The following code sample describes the default settings and DSL elements you can find in the top-level build.gradle after creating a new project.

    buildscript {
        repositories {
            mavenCentral()
        }
    
        dependencies {
           classpath 'com.android.tools.build:gradle:2.2.0'
           classpath 'com.google.gms:google-services:3.0.0'
        }
    }

    ext {
        compileSdkVersion = 23
        buildToolsVersion = "23.0.1"
    }

**The Module-level Build File**

The module-level build.gradle file, located in each `<project>/<module>/` directory, allows you to configure build settings for the specific module it is located in. Configuring these build settings allows you to provide custom packaging options, such as additional build types and product flavors, and override settings in the `main/ app` manifest or top-level `build.gradle` file.

    apply plugin: 'com.android.application'
    
    
    android {
        compileSdkVersion rootProject.ext.compileSdkVersion
        buildToolsVersion rootProject.ext.buildToolsVersion
    }
    
    dependencies {
        //.....
    }



## Use archivesBaseName to change the apk name
You can use the `archivesBaseName` to set the name of apk.

For example:

      defaultConfig {
          ....
          project.ext.set("archivesBaseName", "MyName-" + defaultConfig.versionName);
    
      }

You will obtain this output.

    MyName-X.X.X-release.apk﻿

## The module file example
```gradle
/**
 * The first line in the build configuration applies the Android plugin for
 * Gradle to this build and makes the android {} block available to specify
 * Android-specific build options.
 */

apply plugin: 'com.android.application'

/**
 * The android {} block is where you configure all your Android-specific
 * build options.
 */

android {

  /**
   * compileSdkVersion specifies the Android API level Gradle should use to
   * compile your app. This means your app can use the API features included in
   * this API level and lower.
   *
   * buildToolsVersion specifies the version of the SDK build tools, command-line
   * utilities, and compiler that Gradle should use to build your app. You need to
   * download the build tools using the SDK Manager.
   */

  compileSdkVersion 23
  buildToolsVersion "23.0.3"

  /**
   * The defaultConfig {} block encapsulates default settings and entries for all
   * build variants, and can override some attributes in main/AndroidManifest.xml
   * dynamically from the build system. You can configure product flavors to override
   * these values for different versions of your app.
   */

  defaultConfig {

    /**
     * applicationId uniquely identifies the package for publishing.
     * However, your source code should still reference the package name
     * defined by the package attribute in the main/AndroidManifest.xml file.
     */

    applicationId 'com.example.myapp'

    // Defines the minimum API level required to run the app.
    minSdkVersion 14

    // Specifies the API level used to test the app.
    targetSdkVersion 23

    // Defines the version number of your app.
    versionCode 1

    // Defines a user-friendly version name for your app.
    versionName "1.0"
  }

  /**
   * The buildTypes {} block is where you can configure multiple build types.
   * By default, the build system defines two build types: debug and release. The
   * debug build type is not explicitly shown in the default build configuration,
   * but it includes debugging tools and is signed with the debug key. The release
   * build type applies Proguard settings and is not signed by default.
   */

  buildTypes {

    /**
     * By default, Android Studio configures the release build type to enable code
     * shrinking, using minifyEnabled, and specifies the Proguard settings file.
     */

    release {
        minifyEnabled true // Enables code shrinking for the release build type.
        proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
    }
  }

  /**
   * The productFlavors {} block is where you can configure multiple product
   * flavors. This allows you to create different versions of your app that can
   * override defaultConfig {} with their own settings. Product flavors are
   * optional, and the build system does not create them by default. This example
   * creates a free and paid product flavor. Each product flavor then specifies
   * its own application ID, so that they can exist on the Google Play Store, or
   * an Android device, simultaneously.
   */

  productFlavors {
    free {
      applicationId 'com.example.myapp.free'
    }

    paid {
      applicationId 'com.example.myapp.paid'
    }
  }
}

/**
 * The dependencies {} block in the module-level build configuration file
 * only specifies dependencies required to build the module itself.
 */

dependencies {
    compile project(":lib")
    compile 'com.android.support:appcompat-v7:24.1.0'
    compile fileTree(dir: 'libs', include: ['*.jar'])
}
```

## Top Level File example
```gradle
/**
 * The buildscript {} block is where you configure the repositories and
 * dependencies for Gradle itself--meaning, you should not include dependencies
 * for your modules here. For example, this block includes the Android plugin for
 * Gradle as a dependency because it provides the additional instructions Gradle
 * needs to build Android app modules.
 */

buildscript {

    /**
     * The repositories {} block configures the repositories Gradle uses to
     * search or download the dependencies. Gradle pre-configures support for remote
     * repositories such as JCenter, Maven Central, and Ivy. You can also use local
     * repositories or define your own remote repositories. The code below defines
     * JCenter as the repository Gradle should use to look for its dependencies.
     */

    repositories {
        jcenter()
    }

    /**
     * The dependencies {} block configures the dependencies Gradle needs to use
     * to build your project. The following line adds Android Plugin for Gradle
     * version 2.0.0 as a classpath dependency.
     */

    dependencies {
        classpath 'com.android.tools.build:gradle:2.0.0'
    }
}

/**
 * The allprojects {} block is where you configure the repositories and
 * dependencies used by all modules in your project, such as third-party plugins
 * or libraries. Dependencies that are not required by all the modules in the
 * project should be configured in module-level build.gradle files. For new
 * projects, Android Studio configures JCenter as the default repository, but it
 * does not configure any dependencies.
 */

allprojects {
   repositories {
       jcenter()
   }
}
```

