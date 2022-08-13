---
title: "Message Passing"
slug: "message-passing"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Official documentation
===

* [Message Passing][1]
* [Native Messaging][2]
* [`chrome.runtime` API][3] (most messaging functions and all messaging events)


  [1]: https://developer.chrome.com/extensions/messaging
  [2]: https://developer.chrome.com/extensions/nativeMessaging
  [3]: https://developer.chrome.com/extensions/runtime

## Send a response asynchronously
<!-- language-all: lang-js -->

In attempt to send a response asynchronously from [`chrome.runtime.onMessage`](https://developer.chrome.com/extensions/runtime#event-onMessage) callback we might try this **wrong code**: <!-- EDITORS: please keep "wrong code" on a new line -->

    chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        $.ajax({
            url: 'https://www.google.com',
            method: 'GET',
            success: function(data) {
                // data won't be sent
                sendResponse(data);
            },
        });
    });

However, we would find that `data` is never sent. This happens because we have put `sendResponse` inside an asynchronous ajax call, when the `success` method is executed, the message channel has been closed.

**The solution would be simple,** as long as we explicitly `return true;` at the end of the callback, which indicates we wish to send a response asynchronously, so the message channel will be kept open to the other end (caller) until `sendResponse` is executed.

    chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        $.ajax({
            url: 'https://www.google.com',
            method: 'GET',
            success: function(data) {
                // data would be sent successfully
                sendResponse(data);
            },
        });

        return true; // keeps the message channel open until `sendResponse` is executed
    });

Of course, it applies to an explicit `return` from the onMessage callback as well:

    chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        if (request.action == 'get') {
            $.ajax({
                url: 'https://www.google.com',
                method: 'GET',
                success: function(data) {
                    // data would be sent successfully
                    sendResponse(data);
                },
            });
    
            return true; // keeps the message channel open until `sendResponse` is executed
        }

        // do something synchronous, use sendResponse

        // normal exit closes the message channel
    });


