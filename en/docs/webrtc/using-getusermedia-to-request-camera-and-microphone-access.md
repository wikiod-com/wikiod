---
title: "Using getUserMedia() to request camera and microphone access"
slug: "using-getusermedia-to-request-camera-and-microphone-access"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Using getUserMedia()
As we knew, WebRTC is all based on the JavaScript development and coding and for more information and examples please refer [here][1] and [here][2].


  [1]: https://www.wikiod.com/webrtc/getting-started-with-webrtc
  [2]: https://www.wikiod.com/webrtc/webrtc-simple-examples

And now, let me show you a very simple example to use `getUserMedia();` 

For what `getUserMedia()` is used?
----------------------------------

getUserMedia() is used to get the user/visitor's camera and microphone detection.

---------------------------------------------------

Supported browsers for `getUserMedia()`
---------------------------------------

* Mozilla Firefox 22 (PC) or higher.
* Microsoft Edge 21 (PC) or higher.
* Google Chrome 23 (PC) or higher.
* Opera 18 (PC) or higher.

* Google Chrome 28 (Android) or higher.
* Mozilla Firefox 24 (Android) or higher.
* Opera Mobile 12 (Android) or higher.

* iOS (**Bowser**).
* Chrome OS
* Firefox OS
* Default BlackBerry 10 browser.


----------

Required files to use `getUserMedia`
------------------------------------

* https://github.com/webrtc/samples/blob/gh-pages/src/content/getusermedia/gum/js/test.js
* https://github.com/webrtc/samples/blob/gh-pages/src/content/getusermedia/gum/js/main.js
* https://github.com/webrtc/samples/blob/gh-pages/src/js/adapter.js
* https://github.com/webrtc/samples/blob/gh-pages/src/js/common.js
* https://github.com/webrtc/samples/blob/gh-pages/src/js/lib/ga.js


----------

1. Start coding in a normal HTML file.
2. In the `<body></body>` tags, include the required WebRTC API files:

I'm not able to use the Stackoverflow Editor's Code, so here is the code: http://pastebin.com/2fQzJhuG

***Good job! you're just a great starter now and it should work normally.***

Last code:

    <html>
    <body>
    <script src="js/adapter.js"></script>
    <script src="js/common.js"></script>
    <script src="js/main.js"></script>
    <script src="js/lib/ga.js"></script>
    <body>
    </html>

It was very easy, right?

