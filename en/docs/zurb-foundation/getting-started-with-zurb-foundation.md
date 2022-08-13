---
title: "Getting started with zurb-foundation"
slug: "getting-started-with-zurb-foundation"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Downloading:**

You can download Foundation from  [Foundation download page][1]

Or install it using bower 

    bower install foundation-sites
or npm 

    npm install foundation-sites 

You can also include foundation in your webpages using a cdn

    <!-- Compressed CSS -->
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/foundation/6.3.1/css/foundation.min.css" integrity="sha256-itWEYdFWzZPBG78bJOOiQIn06QCgN/F0wMDcC4nOhxY=" crossorigin="anonymous" />

    <!-- Compressed JavaScript -->
    <script src="https://cdnjs.cloudflare.com/ajax/libs/foundation/6.3.1/js/foundation.min.js" integrity="sha256-Nd2xznOkrE9HkrAMi4xWy/hXkQraXioBg9iYsBrcFrs=" crossorigin="anonymous"></script>


**Installing:** 


Within your HTML page, include Foundation's CSS, JS, and the dependency of jQuery. 

You can either include the locally download css and javascript or use a cdn.

A basic foundation document looks like this: 

    <!doctype html>
    <html class="no-js" lang="en" dir="ltr">
      <head>
        <meta charset="utf-8">
        <meta http-equiv="x-ua-compatible" content="ie=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Your page title </title>
        <link rel="stylesheet" href="css/foundation.css">
        <link rel="stylesheet" href="css/app.css">
      </head>
      <body>
    
    
    
    
    
        <script src="js/vendor/jquery.js"></script>
        <script src="js/vendor/what-input.js"></script>
        <script src="js/vendor/foundation.js"></script>
        <script src="js/app.js"></script>
      </body>
    </html>


  [1]: http://foundation.zurb.com/sites/download.html


