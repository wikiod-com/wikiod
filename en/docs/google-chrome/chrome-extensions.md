---
title: "Chrome Extensions"
slug: "chrome-extensions"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Google Chrome supports [extensions][1] that augment the way the browser works. They can add functionality to web pages or to the browser UI.


  [1]: https://developer.chrome.com/extensions

## Browser Action running executeScript on a page.
**manifest.json**

    {
      "name": "Hello Page",
      "description": "Add 'Hello' to the current page.",
      "version": "1.0",
      "permissions": [
        "activeTab"
      ],
      "background": {
        "scripts": ["background.js"],
        "persistent": false
      },
      "browser_action": {
        "default_title": "Say Hello on this page"
      },
      "manifest_version": 2
    }

**background.js**

    chrome.browserAction.onClicked.addListener(function(tab) {
      chrome.tabs.executeScript({
        code: 'document.body.insertAdjacentText("beforeBegin", "Hello!")'
      });
    });




