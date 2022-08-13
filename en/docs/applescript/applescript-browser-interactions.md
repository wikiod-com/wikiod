---
title: "AppleScript Browser Interactions"
slug: "applescript-browser-interactions"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

AppleScript is able to retrieve the URL of the current tab of a browser.

## Returning tab URLs
# Safari

To return the URL of the current tab in Safari, use `URL of current tab`:

    tell application "Safari"
        return URL of current tab of window 1
    end tell

# Google Chrome

To return the URL of the current tab in Google Chrome, use `URL of active tab`:

    tell application "Google Chrome"
        return URL of active tab of window 1
    end tell

