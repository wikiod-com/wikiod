---
title: "Applescript from the Terminal command line"
slug: "applescript-from-the-terminal-command-line"
draft: false
images: []
weight: 9999
type: docs
toc: true
---

Applescript is a powerful scripting language that can be used directly from the Terminal command line to accomplish a multitude of tasks. 

**All examples listed in this section are meant to be used from the Terminal application.** 


## Get the current URL in Safari or Google Chrome
From the **Terminal** command line

Get the *current* URL from **Safari**

    osascript -e 'tell app "safari" to get the url of the current tab of window 1'

Get the *active* URL in **Google Chrome**

    osascript -e 'tell app "google chrome" to get the url of the active tab of window 1'




## Get the Title of the current page in Safari or Google Chrome
Get the *name* of the web page in **Safari**

    osascript -e 'tell app "safari" to get the name of the current tab of window 1'

Get the *title* of the web page in **Google Chrome**

    osascript -e 'tell app "google chrome" to get the title of the active tab of window 1'

## Use an Applescript as a shell function
You're not limited to single line Applescript code. Here we take the previous two examples and combine them into a single function.

    #!/bin/bash    

    pageinfo() {
      osascript -e \
      'tell app "safari" 
        tell the current tab of window 1
          return {url & "\n" & name}
        end tell
      end tell'
    }



