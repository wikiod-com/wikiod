---
title: "Glyphicons"
slug: "glyphicons"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

This section provides an overview on Bootstrap glyphicons and describes how to use glyphicons.

## How to Use Glyphicons
Twitter Bootstrap supports icons called glyphicons and they can be used with all tags of HTML.

All icons require a base class and individual icon class. 

Keep in mind that icon classes cannot be directly combined with other components, so always use inner `<span></span>` tag.

If your HTML code has inner child elements then you are not able to use icon classes for that particular tag.

**[Examples][1]**

For example, you are creating a bootstrap button, then the syntax for this button should be like this:

    <button type="button" class="btn btn-default btn-lg">
       Star
    </button>
So in above example a simple bootstrap button is created but now you want to add a glyphicon in this button, for this simply add a `<span>` element inside a `<button>` tag. Like this:

    <button type="button" class="btn btn-default btn-lg">
           <span class="glyphicon glyphicon-star" aria-hidden="true"></span>Star
    </button>

[![enter image description here][2]][2]


  [1]: http://getbootstrap.com/components/#glyphicons-examples
  [2]: https://i.stack.imgur.com/KAIIj.png

