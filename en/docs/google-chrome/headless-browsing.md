---
title: "Headless browsing"
slug: "headless-browsing"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Chrome supports headless browsing that's exposed through a switch that can be used when starting the process. This enables opening pages without creating a browser window, thus a graphical environment is not required.

When used in conjuction with an appliance that connects to the remote debugging port, it enables interacton with the document, which is particularly useful for testing and CI automation, where a graphical environment is not necessary to get results or isn't available.



## Syntax
- chrome --headless $SWITCHES https://stackoverflow.com

Historically, other's have succeeded at using Chrome as a headless browser by running the process in a [hidden display](https://chromium.googlesource.com/chromium/src/+/lkgr/headless/README.md).

Invoking Chrome directly is not the only available option for using it as a headless browser. The [Embedder API](https://chromium.googlesource.com/chromium/src/+/lkgr/headless/README.md#Embedder-API) also alows one to use Chrome directly from the application's process.



## Taking screenshots
The following will produce a PNG image in the current directory of the loaded page.

`chrome  --headless --screenshot https://stackoverflow.com`

## Interacting with documents
Using the `--remote-debugging-port` to expose a debugger accessible over HTTP is one way appliances can connect and interact with the document using the Chrome Debugging Protocol.

`chrome --headless --remote-debugging-port=9222 https://stackoverflow.com`

You can then navigate to http://localhost:9222 and use Chrome DevTools interactively.

