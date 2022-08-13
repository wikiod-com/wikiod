---
title: "Remote function - use Electron functions in JavaScript"
slug: "remote-function---use-electron-functions-in-javascript"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

If you have to change some things in renderer.js or main.js but you want to do the changes in index.html, you can use the remote function. It lets you access all the electron functions you need!

## Syntax
 - use remote like `require("electron")`:
   - main.js:&nbsp;&nbsp;&nbsp;&nbsp;`const electron = require("electron");`

     index.html: `const electron = require("electron").remote;`

## Using remote by setting the progress bar
<!-- language: lang-js -->

    const { remote } = require("electron"); // <- The Node.js require() function is
                                          // added to JavaScript by electron
    
    function setProgress(p) { // p = number from 0 to 1
      const currentWindow = remote.getCurrentWindow();
      currentWindow.setProgressBar(p);
    }

## Using remote by setting window to fullscreen
<!-- language: lang-js -->

    const { remote } = require("electron"); // <- The Node.js require() function is
                                          // added to JavaScript by electron
    
    function fullscreen(f) { // p = false or true
      const currentWindow = remote.getCurrentWindow();
      currentWindow.maximize();
    }

