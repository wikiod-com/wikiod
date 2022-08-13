---
title: "Getting started with polymer-1.0"
slug: "getting-started-with-polymer-10"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Element structure
Defining an element with no content.    
    <link rel="import" href="path/to/bower_components/polymer/polymer.html">
    
    <dom-module id="empty-element">
      <template>
        <style>
        </style>
      </template>
    
      <script>
        Polymer({
          is: 'empty-element',
        });
      </script>
    </dom-module>

And then you can use the new element in any other pages.

    <!DOCTYPE html>
    <html>
        <head>
            <!-- Using lite version as Polymer does not require Polyfill for Shadow Dom -->
            <script src="path/to/bower_components/webcomponentsjs/webcomponents-lite.min.js"></script>
            <!-- Importing the element assuming file name is also empty-element.html -->
            <link rel="import" href="empty-element.html">
        </head>

        <body>
            <empty-element></empty-element>
        </body>
    </html>

