---
title: "Getting started with android-activity"
slug: "getting-started-with-android-activity"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Activity
Activity is complete screen. UI is **XML** based and 

    package com.example.android.activity;

    import android.os.Bundle;
    import android.app.Activity;

    public class MainActivity extends Activity {

    @Override
       public void onCreate(Bundle savedInstanceState) {
          super.onCreate(savedInstanceState);
          setContentView(R.layout.activity_main);
          
       }
    }

**NOTE** Activity must be declared in `AndroidManifest.xml` before using it.

E.g:

    <activity android:name=".MainActivity">
         <intent-filter>
             <action android:name="android.intent.action.MAIN" />

             <category android:name="android.intent.category.LAUNCHER" />
         </intent-filter>
    </activity>

Every activity has its layout file in `xml` format, we include its layout using setContentView method of Activity class. E.g. `setContentView(R.layout.activity_main)`

**Layout file example** 

    <?xml version="1.0" encoding="utf-8"?>
    <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:tools="http://schemas.android.com/tools"
        android:id="@+id/activity_dashboard"
        android:layout_width="match_parent"
        android:layout_height="match_parent">
        
        //Add other views here

    </LinearLayout>

## Installation or Setup
Detailed instructions on getting android-activity set up or installed.

