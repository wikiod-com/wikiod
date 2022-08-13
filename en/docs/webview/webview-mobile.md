---
title: "WebView Mobile"
slug: "webview-mobile"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

`WebView` is an `UI` component that could be used to embed web page within your app. Both [iOS](https://developer.apple.com/reference/uikit/uiwebview) and [Android](https://developer.android.com/reference/android/webkit/WebView.html) provide `WebView` components as part of their `SDK`. `WebView` components mostly based on `Chromium` project and powered by [Blink](https://www.chromium.org/blink) rendering engine (previously [WebKit](https://webkit.org/)).

## Android WebView minimal example
In your activity layout `activity_main.xml` specify `WebView` component as following:

    <?xml version="1.0" encoding="utf-8"?>
    <RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:tools="http://schemas.android.com/tools"
        android:id="@+id/activity_main"
        android:layout_width="match_parent"
        android:layout_height="match_parent">
    
       <WebView
           android:id="@+id/webview"
           android:layout_width="match_parent"
           android:layout_height="match_parent">
       </WebView>
    </RelativeLayout>


Then in your `MainActivity.java` get `WebView` component and load some URL:


    package com.j2ko.webviewapp;
    
    import android.os.Bundle;
    import android.support.v7.app.AppCompatActivity;
    import android.webkit.WebView;

    public class MainActivity extends AppCompatActivity {
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_main);
    
            WebView webView = (WebView) findViewById(R.id.webview);
            webView.loadUrl("http://stackoverflow.com/");
        }
    }

Also you need to specify `INTERNET` permission in your `AndroidManifest.xml` file otherwise `WebView` will not be able to load page:


    <uses-permission android:name="android.permission.INTERNET"/>

    

 

