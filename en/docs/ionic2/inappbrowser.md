---
title: "InAppBrowser"
slug: "inappbrowser"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Sometimes client just require to open a web app in mobile app, for this we can use InAppBrowser in such a way that it looks like an app instead we are opening a website/webApp in mobile, as soon as the user will tap the app icon than instead of opening the first app view we can open InAppBrowser directly.

## A live example of this usage is this app:
In this app i am directly opening InAppBrowser when the user tap the app icon instead of loading first page of app. So that would look like to user that they are viewing the app of the same website/webapp.

## Code example to use InAppBrowser
      platform.ready().then(() => {
          // Okay, so the platform is ready and our plugins are available.
          // Here you can do any higher level native things you might need.
          var url= "https://blog.knoldus.com/";
          var browserRef = window.cordova.InAppBrowser.open(url, "_self", "location=no", "toolbar=no");
          browserRef.addEventListener("exit", (event) => {
              return navigator["app"].exitApp();
            }
          );

