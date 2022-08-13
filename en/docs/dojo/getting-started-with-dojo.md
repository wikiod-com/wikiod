---
title: "Getting started with dojo"
slug: "getting-started-with-dojo"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Use Dojo from CDN**

Load Dojo through `<script>` tags in your HTML page pointing to Google CDN. 

Example:

    <script src="//ajax.googleapis.com/ajax/libs/dojo/1.11.2/dojo/dojo.js"></script>


**Install Dojo with Bower**

Type the following command in your project directory:

    bower install dojo/dojo dojo/dijit dojo/dojox dojo/util


Bower installs to a bower_components sub-directory by default, but if you'd like to install to the current directory instead add a `.bowerrc` with the following:

  
    {
        "directory": "."
    }



## Use dojo themes from CDN
Dojo provides us various themes like tundra, claro etc.  

Load themes using `link` tag in your HTML page pointing to Google CDN.

<link rel="stylesheet"
href="http://ajax.googleapis.com/ajax/libs/dojo/1.11.2/dijit/themes/claro/claro.css" />


## Sample page
This example is a sample page that shows how to use **Dojo** to display a **"Hello world"** text inside `<h1>` tag.

    <!DOCTYPE html>
    <html>
    
      <head>
        <meta charset="utf-8">
        <title>Dojo sample</title>
        <script src="//ajax.googleapis.com/ajax/libs/dojo/1.12.1/dojo/dojo.js" data-dojo-config="async: true"></script>
      </head>
    
      <body>
        <h1 id="Hello"></h1>
        
    
        <script>
          require([
            'dojo/dom'
          ], function(dom) {
          
            dom.byId('Hello').innerHTML = 'Hello world';
          });
    
        </script>
      </body>
    
    </html>

