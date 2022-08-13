---
title: "Screen"
slug: "screen"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Getting the screen resolution
To get the physical size of the screen (including window chrome and menubar/launcher):

    var width  = window.screen.width,
        height = window.screen.height;

## Getting the “available” area of the screen
To get the “available” area of the screen (i.e. not including any bars on the edges of the screen, but including window chrome and other windows:

    var availableArea = {
        pos: {
            x: window.screen.availLeft,
            y: window.screen.availTop
        },
        size: {
            width: window.screen.availWidth,
            height: window.screen.availHeight
        }
    };


## Getting color information about the screen
To determine the color and pixel depths of the screen:

    var pixelDepth = window.screen.pixelDepth,
        colorDepth = window.screen.colorDepth;

## Window innerWidth and innerHeight Properties
Get the window height and width

    var width = window.innerWidth
    var height = window.innerHeight


## Page width and height
To get current page width and height (for any browser), e.g. when programming responsiveness:

    function pageWidth() {
      return window.innerWidth != null? window.innerWidth : document.documentElement && document.documentElement.clientWidth ? document.documentElement.clientWidth : document.body != null ? document.body.clientWidth : null;
    }
    
    function pageHeight() {
      return  window.innerHeight != null? window.innerHeight : document.documentElement && document.documentElement.clientHeight ? document.documentElement.clientHeight : document.body != null? document.body.clientHeight : null;
    }


