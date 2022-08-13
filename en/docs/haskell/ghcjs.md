---
title: "GHCJS"
slug: "ghcjs"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

GHCJS is a Haskell to JavaScript compiler that uses the GHC API.

## Running "Hello World!" with Node.js
`ghcjs` can be invoked with the same command line arguments as `ghc`. The generated programs can be run directly from
the shell with [Node.js](http://nodejs.org/) and [SpiderMonkey jsshell](http://download.cdn.mozilla.net/pub/firefox/nightly/latest-mozilla-central/).
for example:

    $ ghcjs -o helloWorld helloWorld.hs
    $ node helloWorld.jsexe/all.js
    Hello world!


