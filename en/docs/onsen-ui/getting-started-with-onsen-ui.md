---
title: "Getting started with onsen-ui"
slug: "getting-started-with-onsen-ui"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
[Onsen UI][1] is an open-source framework that helps you build hybrid apps with native like performance. It can be used along with several well known JavaScript frameworks such as AngularJS (1 & 2), ReactJS and jQuery. 

Loading OnsenUI in a project is as easy as writing some standard tags of HTML in your index.html file:

  

    <!doctype html>
        <html lang="en">
          <head>
            <meta charset="utf-8">
            <!-- load Onsen structure CSS file -->
            <link rel="stylesheet" href="lib/onsen/css/onsenui.css"/>
            <!-- load Onsen theme CSS file -->
            <link rel="stylesheet" href="lib/onsen/css/onsen-css-components.css"/>
            <!-- load Onsen main Javascript file -->
            <script src="lib/onsen/js/onsenui.js"></script>
            <script>
              ons.ready(function() {
                // Init code here
              });
            </script>
          </head>
          <body>
            <ons-navigator>
              <ons-page>
                Hello World!
              </ons-page>
            </ons-navigator>
          </body>
        </html>

`ons.ready` function is the main function that can safely tell us when Onsen UI initialization is done. Then you can use any of OnsenUI [components][2].


  [1]: https://onsen.io/v2/
  [2]: https://onsen.io/v2/docs/guide/js/

