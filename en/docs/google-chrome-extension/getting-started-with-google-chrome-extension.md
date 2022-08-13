---
title: "Getting started with google-chrome-extension"
slug: "getting-started-with-google-chrome-extension"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Absolute minimum example
Any Chrome extension starts as an _unpacked extension_: a folder containing the extension's files.

One file it must contain is [`manifest.json`][1], which describes the basic properties of the extension. Many of the properties in that file are optional, but here is an absolute minimum `manifest.json` file:

<!-- language-all: lang-js -->

    {
      "manifest_version": 2,
      "name": "My Extension",
      "version": "1.0"
    }

Create a folder (for example, `myExtension`) somewhere, add `manifest.json` as listed above to it.

Then, you need to load the extension in Chrome.

1. Open the `chrome://extensions/` page, accessible though **Menu > More tools > Extensions**.
2. Enable **Developer Mode** with a checkbox in the top right, if it's not enabled already.
3. Click on **Load unpacked extension...** button and select the created `myExtension` folder.
[![enter image description here][2]][2]

That's it! Your first extension is loaded by Chrome:

[![enter image description here][3]][3]

Of course, it doesn't do anything yet, so it's a good moment to read an [overview of extension architecture][4] to start adding parts you need.

**Important:** When you do any changes to your extension, do not forget to return to `chrome://extensions/` and press the **Reload** link for your extension after you make changes. In case of content scripts, reload the target page as well.


  [1]: https://developer.chrome.com/extensions/manifest
  [2]: http://i.stack.imgur.com/ketf6.png
  [3]: http://i.stack.imgur.com/FYha1.png
  [4]: https://developer.chrome.com/extensions/overview

## Background Page


## Content Scripts


## Options Page


## Create a new tab
In the extension code you can use any `chrome.*` API if you decalared the required permissions. In addition, some API's works only from background pages, and some API's works only from content scripts.

You can use most of `chrome.tabs` methods declaring any permissions. Now we focus on `chrome.tabs.create`

    
<sub>Note: The new tab will be opened without any `popup` warning.</sub>

<!-- language: lang-js -->    

    chrome.tabs.create({
           url:"http://stackoverflow.com",
           selected:false  // We open the tab in the background
    })

You can learn more about tab object, in the [official chrome developer](https://developer.chrome.com/extensions/tabs#method-create)

