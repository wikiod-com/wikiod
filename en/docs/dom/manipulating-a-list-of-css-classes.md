---
title: "Manipulating a list of CSS classes"
slug: "manipulating-a-list-of-css-classes"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Testing for a class
Modern browsers provide a `classList` object to ease manipulation of the element's class attribute. Older browsers require direct manipulation of the element's `className` property.

\* Note class names are not stored in the element's property in any particular order

<!-- if version <W3C DOM> [lt 4] -->

Testing if an element contains a class requires a bit of manipulation of the `className` property. This example is using an array method to test for the class.

    function hasClass(el, name) {
      var classes = (el && el.className || "").split(/\s+/);
      return classes.indexOf(name) > -1;
    }
    var el = document.getElementById("link1");
    console.log(hasClass(el, "foo"));

Testing for multiple class names would require a loop.

    function hasClass(el, name) {
      name = name.split(/[\s.]+/);
      var hasClass = true,
        classes = (el && el.className || "").split(/\s+/),
        index = name.length;
      while (index--) {
        hasClass = hasClass && classes.indexOf(name[index]) > -1;
      }
      return hasClass;
    }
    var el = document.getElementById("link1");
    console.log(hasClass(el, "foo"));

Instead of using `.indexOf()`, you may also consider using a regular expression.

    function hasClass(el, name) {
      return new RegExp("\\b" + name+ "\\b").test(el.className);
    }
    var el = document.getElementById("link1");
    console.log(hasClass(el, "foo"));

<!-- end version if -->
<!-- if version <W3C DOM> [gte 4] -->

Testing for a single class name is done as follows:

    var hasClass = document.querySelector("#link1").classList.contains("foo");

For multiple class names, it is easier to use `matches`. Note the use of the class selector; The selector can be any valid string selector (id, attribute, pseudo-classes, etc).

    var hasClass = document.querySelector("#link1").matches('.foo.bar');
    var hasClass = document.querySelector("#link2").matches('a.bar[href]');

<!-- end version if -->



## Adding a class
Modern browsers provide a `classList` object to ease manipulation of the element's class attribute. Older browsers require direct manipulation of the element's `className` property.

<!-- if version <W3C DOM> [lt 4] -->

A simple method to add a class to an element is to append it to the end of the `className` property. This will not prevent duplicate class names, and spaces **must** be included between class names.

    document.getElementById("link1").className += " foo";
    document.getElementById("link2").className += " foo bar";

For multiple elements, you'll need to add the class names inside of a loop

    var els = document.getElementsByClassName("foo"),
      indx = els.length;
    while (indx--) {
      els[indx].className += " bar baz";
    }

<!-- end version if -->
<!-- if version <W3C DOM> [gte 4] -->

A single class name may be added as a string. To add multiple class names, use ES6's spread operator:

    document.querySelector("#link1").classList.add("foo");
    document.querySelector("#link2").classList.add(...['foo', 'bar']);

For multiple elements, you'll need to add the class names inside of a loop

    document.querySelectorAll(".foo").forEach(el => {
      el.classList.add(...['bar', 'baz']);
    });

<!-- end version if -->



## Removing a class
Modern browsers provide a `classList` object to ease manipulation of the element's class attribute. Older browsers require direct manipulation of the element's `className` property.

\* Note class names are not stored in the element's property in any particular order

<!-- if version <W3C DOM> [lt 4] -->

Removing one class from an element requires a bit of manipulation of the `className` property.

    var toRemove = "bar",
      el = document.getElementById("link1");
    el.className = el.className.replace(new RegExp("\\b" + toRemove + "\\b", "g"), "").trim();

Removing multiple class names would require a loop. The remaining examples will use a function to isolate the work

    function removeClass(el, name) {
      name = name.split(/\s+/);
      var index = name.length,
        classes = el.className;
      while (index--) {
        classes = classes.replace(new RegExp("\\b" + name[index] + "\\b", "g"), "").trim();
      }
      el.className = classes;
    }
    var el = document.getElementById("link1");
    removeClass(el, "bar baz");

Multiple elements with multiple class names to remove would require two loops

    function removeClass(els, name) {
      name = name.split(/\s+/);
      var regex, len,
        index = name.length;
      while (index--) {
        regex = new RegExp("\\b" + name[index] + "\\b", "g");
        len = els.length;
        while (len--) {
          els[len].className = els[len].className.replace(regex, "").trim();
        }
      }
    }
    var els = document.getElementsByTagName("a");
    removeClass(els, "bar baz");

<!-- end version if -->
<!-- if version <W3C DOM> [gte 4] -->

A single class name may be removed as a string. To remove multiple class names, use ES6's spread operator:

    document.querySelector("#link1").classList.remove("foo");
    document.querySelector("#link2").classList.remove(...['foo', 'bar']);

For multiple elements, you'll need to remove the class names inside of a loop

    document.querySelectorAll(".foo").forEach(el => {
      el.classList.remove(...['bar', 'baz']);
    });

<!-- end version if -->


