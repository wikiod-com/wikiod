---
title: "Content scripts"
slug: "content-scripts"
draft: false
images: []
weight: 9834
type: docs
toc: true
---

Official documentation
====

* [Content Scripts][1]
* [Content Security Policy > Content Scripts][2]


  [1]: https://developer.chrome.com/extensions/content_scripts
  [2]: https://developer.chrome.com/extensions/contentSecurityPolicy#interactions

## Declaring content scripts in the manifest
Content scripts can be declared in [`manifest.json`][1] to be always injected into pages that match a set of [URL patterns][2].

Minimal example
===

<!-- language-all: lang-js -->

    "content_scripts" : [
      {
        "js": ["content.js"],
        "css": ["content.css"]
        "matches": ["http://example.com/*"]
      }
    ]

This manifest entry instructs Chrome to inject a content script `content.js`, along with the CSS file `content.css`, after any navigation to a page matching the [match pattern][2] `http://example.com/*`

Both `js` and `css` keys are optional: you can have only one of them or both depending on what you need.

`content_scripts` key is an array, and you can declare several content script definitions:

    "content_scripts" : [
      {
        "js": ["content.js"],
        "matches": ["http://*.example.com/*"]
      },
      {
        "js": ["something_else.js"],
        "matches": ["http://*.example.org/*"]
      }
    ]

Note that both `js` and `matches` are arrays, even if you only have one entry.

More options are available in the [official documentation][3] and other Examples.

Important note
===

Content scripts declared in the manifest **will only be injected on new navigations after the extension load**. They will not be injected in existing tabs. This also applies to extension reloads while developing, and extension updates after release.

If you need to ensure that currently opened tabs are covered, consider also doing programmatic injection on startup.


  [1]: https://www.wikiod.com/google-chrome-extension/manifestjson
  [2]: https://developer.chrome.com/extensions/match_patterns
  [3]: https://developer.chrome.com/extensions/content_scripts#registration

## Injecting content scripts from an extension page
If, instead of always having a content script injected based on the URL, you want to directly control when a content script is injected, you can use [Programmatic Injection][1].

Minimal example
---

- JavaScript

  <!-- language-all: lang-js -->

      chrome.tabs.executeScript({file: "content.js"});

- CSS
 
  <!-- language-all: lang-js -->

      chrome.tabs.insertCSS({file: "content.css"});

Called from an extension page (e.g. background or popup), and assuming you have permission to inject, this will execute `content.js` or insert `content.css` as a content script in the top frame of the current tab.

Inline code
---

You can execute inline code instead of a file as a content script:

    var code = "console.log('This code will execute as a content script');";
    chrome.tabs.executeScript({code: code});


Choosing the tab
---

You can provide a tab ID (usually from other `chrome.tabs` methods or messaging) to execute in a tab other than the currently active.

    chrome.tabs.executeScript({
      tabId: tabId,
      file: "content.js"
    });

More options are available in the [`chrome.tabs.executeScript()` documentation][2] and in other Examples.

Permissions
====

Using `chrome.tabs.executeScript()` does not require `"tabs"` permission, but requires [host permissions][3] for the page's URL.

Checking for errors
====

If script injection fails, one can catch it in the optional callback:

    chrome.tabs.executeScript({file: "content.js"}, function() {
      if(chrome.runtime.lastError) {
        console.error("Script injection failed: " + chrome.runtime.lastError.message);
      }
    });


  [1]: https://developer.chrome.com/extensions/content_scripts#pi
  [2]: https://developer.chrome.com/extensions/tabs#method-executeScript
  [3]: https://developer.chrome.com/extensions/xhr#requesting-permission

## Multiple content scripts in the manifest
Same conditions, multiple scripts
===

If you need to inject multiple files with all other conditions being the same, for example to include a library, you can list all of them in the `"js"` array:

<!-- language-all: lang-js -->

    "content_scripts" : [
      {
        "js": ["library.js", "content.js"],
        "matches": ["http://*.example.com/*"]
      }
    ]

**Order matters:** `library.js` will be executed before `content.js`.

Same scripts, multiple sites
===

If you need to inject the same files into multiple sites, you can provide multiple match patterns:

    "matches": ["http://example.com/*", "http://example.org/*"]

If you need to inject in basically every page, you can use broad match patterns such as `"*://*/*"` (matches every HTTP(S) page) or `"<all_urls>"` (matches every [supported page][1]).

Different scripts or different sites
===

`"content_scripts"` section is an array as well, so one can define more than one content script block:

    "content_scripts" : [
      {
        "js": ["content.js"],
        "matches": ["http://*.example.com/*"]
      },
      {
        "js": ["something_else.js"],
        "matches": ["http://*.example.org/*"]
      }
    ]


  [1]: https://developer.chrome.com/extensions/match_patterns

