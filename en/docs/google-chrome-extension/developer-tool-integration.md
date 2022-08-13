---
title: "Developer Tool Integration"
slug: "developer-tool-integration"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Debugging the background page/script
The background script is like any other JavaScript code. You can debug it using same tools you debug other JavaScript code in Chrome.

To open the Chrome Developer Tools, go to `chrome://extensions`, and turn on **Developer mode**:
[![Developer mode toggle][1]][1]


Now you can debug any extension that have a background page or script. Just scroll to the extension you want to debug and click on the **background page** link to inspect it.
[![Background page link][2]][2]


  [1]: http://i.stack.imgur.com/7NCn6.png
  [2]: http://i.stack.imgur.com/5qFGT.png


**Tip:** To reload the extension, you can press <kbd>F5</kbd> inside the developer tools window. You can put breakpoints in the initialization code before reloading.

**Tip:** Right-clicking the extension action button and selecting "Manage extensions" will open `chrome://extensions` page scrolled to that extension.


## Programmatic Breakpoint Hinting


## Debugging the popup window
You have 2 ways to debug the popup window. Both ways are by using the Chrome DevTools. 

**Option 1:** Right click the extension's action button, and choose **Inspect popup**

[![enter image description here][1]][1]

**Option 2:** Open the popup window, directly in your browser as a tab.

For example, if you extension id is `abcdefghijkmnop`, and your popup html file is `popup.html`. Go to the address and navigate to:

    chrome-extension://abcdefghijklmnop/popup.html

Now you see the poup in regular tab. And you can press <kbd>F12</kbd> to open the developer tools.

  [1]: http://i.stack.imgur.com/f8Fzd.png

