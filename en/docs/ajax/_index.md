---
title : ajax Tutorial
slug : ajax-tutorial
weight : 9848
draft : false
images : []
type : docs
---

AJAX (**a**synchronous **J**avaScript **a**nd **X**ML) allows you to request external data **without** blocking the execution of code. In many cases this is implemented in requesting pieces of a page or information from a server (via XMLhttpRequests) and then processing and displaying it using javascript.

The non-blocking nature of AJAX is what makes it such a widespread software pattern. Since javascript is blocking in the browser, a synchronous external call would make the browser unresponsive for the duration of the call until it either returned data or timed out. In effect making your application entirely dependent on external architecture and how well it will perform.

AJAX calls are usually abstracted out to provide additional functionality or readability, but implementations are (usually) built upon the [XMLHttpRequest][1] [Specification][2]. 


  [1]: https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest
  [2]: https://xhr.spec.whatwg.org/

