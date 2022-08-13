---
title: "Configure Build Types"
slug: "configure-build-types"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Parameters
| Parameter | Detail |
|-----------|--------|
|applicationIdSuffix  |Application id suffix applied to this base config|
|consumerProguardFiles    |ProGuard rule files to be included in the published AAR|
|debuggable    |Whether this build type should generate a debuggable apk|
|embedMicroApp    |Whether a linked Android Wear app should be embedded in variant using this build type|
|jniDebuggable    |Whether this build type is configured to generate an APK with debuggable native code|
|manifestPlaceholders    |The manifest placeholders|
|minifyEnabled    |Whether Minify is enabled for this build type|
|multiDexEnabled    |Whether Multi-Dex is enabled for this variant|
|name    |Name of this build type|
|proguardFiles    |Returns ProGuard configuration files to be used|
|pseudoLocalesEnabled    |Whether to generate pseudo locale in the APK|
|renderscriptDebuggable    |Whether the build type is configured to generate an apk with debuggable RenderScript code|
|renderscriptOptimLevel   | Optimization level to use by the renderscript compiler|
|shrinkResources    |Whether shrinking of unused resources is enabled. Default is false|
|signingConfig    |The signing configuration|
|testCoverageEnabled    |Whether test coverage is enabled for this build type|
|versionNameSuffix    |Version name suffix|
|zipAlignEnabled    |Whether zipalign is enabled for this build type|
|------|--------|
|**Method**| **Detail**|
|buildConfigField(type, name, value)    |Adds a new field to the generated BuildConfig class|
|consumerProguardFile(proguardFile)    |Adds a proguard rule file to be included in the published AAR|
|consumerProguardFiles(proguardFiles)   |Adds proguard rule files to be included in the published AAR|
|proguardFile(proguardFile)    |Adds a new ProGuard configuration file|
|proguardFiles(proguardFiles)    |Adds new ProGuard configuration files|
|resValue(type, name, value)    |Adds a new generated resource|
|resValue(type, name, value)    |Adds a new generated resource|
|setProguardFiles(proguardFileIterable)    |Sets the ProGuard configuration files|
|shrinkResources(flag)    |Whether shrinking of unused resources is enabled. Default is false|

By default, the Android plugin for gradle automatically sets up the project to build both a debug and a release version of the application.

This configuration is done through an object called a `BuildType`

Official Documentation:
-----------------------
http://google.github.io/android-gradle-dsl/current/com.android.build.gradle.internal.dsl.BuildType.html

## How to configure build types in the build.gradle
You can create and configure build types in the module-level `build.gradle` file inside the `android {}` block.

```gradle
    android {
        ...
        defaultConfig {...}
    
        buildTypes {
            release {
                minifyEnabled true
                proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
            }
    
            debug {
                applicationIdSuffix ".debug"
            }
        }
    }


