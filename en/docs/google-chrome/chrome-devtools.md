---
title: "Chrome DevTools"
slug: "chrome-devtools"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

[Chrome DevTools product documentation][1].


  [1]: https://developer.chrome.com/devtools

## DOM node index($0, $1, etc.. )
In Google Chrome's developer tools "Elements", you can see that the selected line shows `==$0` that is the DOM node index(as shown below):

[![DOM node Index][1]][1]

> $0 returns the most recently selected element or JavaScript object, $1
> returns the second most recently selected one, and so on.

This is useful in debugging. The $0, $1, $2, $3 and $4 commands work as a historical reference to the last five DOM elements inspected within the Elements panel or the last five JavaScript heap objects selected in the Profiles panel.


  [1]: http://i.stack.imgur.com/C2eGI.jpg

## Inspect HTML Structure
On a desktop version of Chrome, the contents of the page can be inspected. This shows the document object model (DOM) of the HTML, the Cascading Style Sheet styles (CSS), and much more.

Enter inspection by one of many options:

 - Right click on a web page, and select **Inspect**
 - From the Chrome Menu, select **More Tools**, **Developer Tools**
 - Use a keyboard shortcut, e.g. Ctrl+Shift+I on Windows.

[![Screenshot of Chrome Developer Tools showing the 'Elements' tab][1]][1]

  [1]: http://i.stack.imgur.com/pEO0r.png

