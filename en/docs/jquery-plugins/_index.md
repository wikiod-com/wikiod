---
title : jquery-plugins Tutorial
slug : jquery-plugins-tutorial
weight : 9989
draft : false
images : []
type : docs
---

A jQuery plugin is simply a new method that we use to extend jQuery's prototype object. By extending the prototype object you enable all jQuery objects to inherit any methods that you add. As established, whenever you call `jQuery()` you're creating a new jQuery object, with all of jQuery's methods inherited.

The idea of a plugin is to do something with a collection of elements. You could consider each method that comes with the jQuery core a plugin, like `.fadeOut()` or `.addClass()`.

You can make your own plugins and use them privately in your code or you can release them to the public. There are thousands of jQuery plugins available online. The barrier to creating a plugin of your own is so low that you can author one easily!

