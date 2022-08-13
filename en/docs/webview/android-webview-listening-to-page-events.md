---
title: "Android WebView listening to page events"
slug: "android-webview-listening-to-page-events"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

`WebView` component allow you to provide custom client object that extends [WebViewClient](https://developer.android.com/reference/android/webkit/WebViewClient.html) class to listen and control `WebView` behavior. 

## Register custom WebViewClient


    package com.j2ko.webviewapp;
    
    import android.graphics.Bitmap;
    import android.os.Build;
    import android.os.Bundle;
    import android.support.annotation.RequiresApi;
    import android.support.v7.app.AppCompatActivity;
    import android.util.Log;
    import android.webkit.WebResourceRequest;
    import android.webkit.WebView;
    import android.webkit.WebViewClient;
    
    public class MainActivity extends AppCompatActivity {
    
        private static final String TAG = "MainActivity";
    
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_main);
    
            WebView webView = (WebView) findViewById(R.id.webview);
            webView.setWebViewClient(new WebViewClient(){
                @Override
                public void onPageStarted(WebView view, String url, Bitmap favicon) {
                    super.onPageStarted(view, url, favicon);
                    Log.d(TAG, "onPageStarted: with url " + url);
                }
    
                @Override
                public void onPageFinished(WebView view, String url) {
                    super.onPageFinished(view, url);
                    Log.d(TAG, "onPageFinished: for " + url);
                }
    
                @Override
                public void onPageCommitVisible(WebView view, String url) {
                    super.onPageCommitVisible(view, url);
                    Log.d(TAG, "onPageCommitVisible: for " + url);
                }
    
                @Override
                public boolean shouldOverrideUrlLoading(WebView view, String url) {
                    Log.d(TAG, "shouldOverrideUrlLoading: for " + url);
                    return super.shouldOverrideUrlLoading(view, url);
                }
    
                @RequiresApi(api = Build.VERSION_CODES.LOLLIPOP)
                @Override
                public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest request) {
                    Log.d(TAG, "shouldOverrideUrlLoading: for " +  request.getUrl());
                    return super.shouldOverrideUrlLoading(view, request);
                }
    
                @Override
                public void onLoadResource(WebView view, String url) {
                    super.onLoadResource(view, url);
                    Log.d(TAG, "onLoadResource: " + url);
                }
            });
            webView.loadUrl("http://stackoverflow.com/");
        }
    }

Output:

    D/MainActivity: onPageStarted: with url http://stackoverflow.com/
    D/MainActivity: onLoadResource: http://stackoverflow.com/
    D/MainActivity: onLoadResource: https://cdn.sstatic.net/Sites/stackoverflow/mobile.css?v=88186b623ab1
    D/MainActivity: onPageFinished: for http://stackoverflow.com/
    D/MainActivity: onLoadResource: https://cdn.sstatic.net/Sites/stackoverflow/img/favicon.ico?v=4f32ecc8f43d
    D/MainActivity: onPageCommitVisible: for http://stackoverflow.com/



