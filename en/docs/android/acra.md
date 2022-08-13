---
title: "ACRA"
slug: "acra"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Syntax
- android:name=".ACRAHandler"
- ACRA.init(this, config);
- public class ACRAHandler extends Application {




## Parameters
| Parameter | Description |
| --- | --- |
| @ReportCrashes| Defines the ACRA settings such as where it is to be reported, custom content, etc|
| formUri | the path to the file that reports the crash |

* ACRA no longer supports Google forms, so you need a backend: https://github.com/ACRA/acra/wiki/Backends


## ACRAHandler
Example Application-extending class for handling the reporting:

    @ReportsCrashes(
    
            formUri = "https://backend-of-your-choice.com/",//Non-password protected.
            customReportContent = { /* */ReportField.APP_VERSION_NAME, ReportField.PACKAGE_NAME,ReportField.ANDROID_VERSION, ReportField.PHONE_MODEL,ReportField.LOGCAT },
            mode = ReportingInteractionMode.TOAST,
            resToastText = R.string.crash
    
    )
    public class ACRAHandler extends Application {
        @Override
        protected void attachBaseContext(Context base) {
            super.attachBaseContext(base);

            final ACRAConfiguration config = new ConfigurationBuilder(this)
    
                    .build();
    
            // Initialise ACRA
            ACRA.init(this, config);
    
    
        }
    
    }



## Example manifest
    <?xml version="1.0" encoding="utf-8"?>
    <manifest xmlns:android="http://schemas.android.com/apk/res/android"
        <!-- etc -->

    >
    

    
        <!-- Internet is required. READ_LOGS are to ensure that the Logcat is transmitted-->
        <uses-permission android:name="android.permission.INTERNET"/>
        <uses-permission android:name="android.permission.READ_LOGS"/>
    
        <application
            android:allowBackup="true"
            android:name=".ACRAHandler"<!-- Activates ACRA on startup -->
            android:icon="@drawable/ic_launcher"
            android:label="@string/app_name"
            android:theme="@style/AppTheme" >
    
           
    
            <!-- Activities -->
        </application>
    
    </manifest>

## Installation
Maven

    <dependency> 
        <groupId>ch.acra</groupId> 
        <artifactId>acra</artifactId> 
        <version>4.9.2</version> 
        <type>aar</type> 
    </dependency>

Gradle

    compile 'ch.acra:acra:4.9.2'


