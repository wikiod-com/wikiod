---
title: "WebView"
slug: "webview"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Add a WebView to the UI
    <Grid Background="{ThemeResource ApplicationPageBackgroundThemeBrush}">
        <WebView x:Name="MyWebView" />
    </Grid>

## Open a website
    MyWebView.Navigate(new Uri("http://www.url.com"));

## Open local html page
    MyWebView.Navigate(new Uri("ms-appdata:///local/Downloads/index.html"));

