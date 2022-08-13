---
title: "HTML 5 Cache"
slug: "html-5-cache"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

The manifest file is a simple text file, which tells the browser what to cache (and what to never cache).
The recommended file extension for manifest files is: ".appcache"
The manifest file has three sections:

CACHE MANIFEST - Files listed under this header will be cached after they are downloaded for the first time

NETWORK - Files listed under this header require a connection to the server, and will never be cached


FALLBACK - Files listed under this header specifies fallback pages if a page is inaccessible


## Basic Example of Html 5 cache
this is our index.html file

    <!DOCTYPE html>
    <html manifest="index.appcache">
    <body>
        <p>Content</p>
    </body>
    </html>


then we will create index.appcache file with below codes 


    CACHE MANIFEST
    index.html
write those files that you want to be cached 
load index.html then go for offline mode and reload the tab 


**Note:** The two files must be in the same folder in this example


