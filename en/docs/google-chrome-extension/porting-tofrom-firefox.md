---
title: "Porting tofrom Firefox"
slug: "porting-tofrom-firefox"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

> If you're using a *Firefox* version before 48, you'll also need an
> additional key in `manifest.json` called applications:

    "applications": {
      "gecko": {
        "id": "borderify@example.com",
        "strict_min_version": "42.0",
        "strict_max_version": "50.*",
        "update_url": "https://example.com/updates.json"
      }
    }

[applications][1]

----------


Note:

[Extension Signing][2]:

> With the release of Firefox 48, extension signing can no longer be
> disabled in the release and beta channel builds by using a preference.
> As outlined when extension signing was announced, we are publishing
> specialized builds that support this preference so developers can
> continue to test against the code that beta and release builds are
> generated from.

Status of `WebExtensions`:
> WebExtensions are currently in an experimental alpha state. From
> Firefox 46, you can publish WebExtensions to Firefox users, just like
> any other add-on. We're aiming for a first stable release in Firefox 48.

**UPD**: *Firefox* 48 released 02.08.2016.

----------


Links:

[API support status][3] - The list of APIs and their status.

[Chrome incompatibilities][4]

[WebExtensions][5] - JavaScript APIs, keys of manifest.json, tutorials, etc.


  [1]: https://developer.mozilla.org/en-US/Add-ons/WebExtensions/manifest.json/applications
  [2]: https://blog.mozilla.org/addons/2016/07/29/extension-signing-availability-of-unbranded-builds/
  [3]: http://arewewebextensionsyet.com/
  [4]: https://developer.mozilla.org/en-US/Add-ons/WebExtensions/Chrome_incompatibilities
  [5]: https://developer.mozilla.org/ru/Add-ons/WebExtensions

## Porting through WebExtensions
Before talking about porting *Firefox* extensions from/to, one should know what `WebExtensions` is.

`WebExtensions` - is a platform that represents an API for creating *Firefox* extensions.

It uses the same architecture of extension as *Chromium*, as a result, this API is compatible in many ways with API in *Google Chrome* and *Opera* (Opera which based on Chromium). In many cases, extensions developed for these browsers will work in *Firefox* with a few changes or even without them at all.

MDN [recommends][1] to use `WebExtension` for new extensions:

> In the future, WebExtensions will be the recommended way to develop
> Firefox add-ons, and other systems will be deprecated.

In view of the foregoing, if you want to port extensions to *Firefox*, you have to know, how the extension was written.

Extensions for *Firefox* can be based on `WebExtension`, `Add-on SDK` or `XUL`.


----------


Compatible extensions based on WebExtension
===========================================

When using `WebExtension`, one has to look through the list of [incompatibilities][2], because some functions are supported fully or partially, that is in other words, one should check one’s `manifest.json`.

It also enables to use the same [namespace][3]:

> At this time, all APIs are accessible through the chrome.* namespace.
> When we begin to add our own APIs, we expect to add them to the
> browser.* namespace. Developers will be able to use feature detection
> to determine if an API is available in browser.*.

A simple extension that can work in Firefox and Google Chrome
-------------------------------------------------------------

`manifest.json`:
<!-- language: lang-json --> 
    {
      "manifest_version": 2,
      
      "name": "StackMirror",
      
      "version": "1.0",
      
      "description": "Mirror reflection of StackOverflow sites",
      
      "icons": {
        "48": "icon/myIcon-48.png"
      },
      
      "page_action": {
        "default_icon": "icon/myIcon-48.png"
      },
      
      "background": {
        "scripts"   : ["js/background/script.js"],
        "persistent": false
      },
      
      "permissions": ["tabs", "*://*.stackoverflow.com/*"]
    }

`background` script:
<!-- language: lang-js --> 
    function startScript(tabId, changeInfo, tab) {
    
        if (tab.url.indexOf("stackoverflow.com") > -1) {
    
            chrome.tabs.executeScript(tabId, 
                
                {code: 'document.body.style.transform = "scaleX(-1)";'}, function () {
    
                if (!chrome.runtime.lastError) {
    
                    chrome.pageAction.show(tabId);
                }
            });
        }
    }

    chrome.tabs.onUpdated.addListener(startScript);

Pack project as standard `zip` file, but with `.xpi` extensions.

[![Pack project][4]][4]

Then,you have to load the extension in *Firefox*.

Open the `about:addons` page, accessible through **Menu > Add-ons**.

Click on **Tools for all add-ons** button.

[![Install extension][5]][5]

When the extension is loaded the page `about:addons` will look like this:

[![Result of install][6]][6]

Directions on loading the extension in Google Chrome is in other topic - [Getting started with Chrome Extensions][7].

The result of extension operation will be same in both browsers (*Firefox*/*Google Chrome*):

[![Result of work][8]][8]


----------


If the current add-on is based on Add-on SDK or XUL
===================================================

When extension being ported is based on `Add-on SDK` one has to look through the comparison table for `Add-on SDK` **=>** `WebExtensions`, because these technologies have similar features, but differ in implementation. Each section of table describes the equivalent of `Add-on SDK` for `WebExtension`.

[Comparison with the Add-on SDK][9]

A similar approach and for XUL extensions.

[Comparison with XUL/XPCOM extensions][10]


  [1]: https://developer.mozilla.org/en-US/Add-ons/WebExtensions/What_are_WebExtensions
  [2]: https://developer.mozilla.org/en-US/Add-ons/WebExtensions/Chrome_incompatibilities
  [3]: https://wiki.mozilla.org/WebExtensions#Namespacing
  [4]: http://i.stack.imgur.com/Ekfow.png
  [5]: http://i.stack.imgur.com/ACxMK.png
  [6]: http://i.stack.imgur.com/Vxlvu.png
  [7]: https://www.wikiod.com/google-chrome-extension/getting-started-with-google-chrome-extension#Absolute minimum example
  [8]: http://i.stack.imgur.com/v0T9k.png
  [9]: https://developer.mozilla.org/en-US/Add-ons/WebExtensions/Comparison_with_the_Add-on_SDK
  [10]: https://developer.mozilla.org/en-US/Add-ons/WebExtensions/Comparison_with_XUL_XPCOM_extensions

