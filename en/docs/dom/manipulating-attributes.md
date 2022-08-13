---
title: "Manipulating Attributes"
slug: "manipulating-attributes"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Attributes are a specific type of object in the DOM API. In earlier versions of the DOM API, they inherited from the `Node` type, but this was changed in version 4.

*In the examples referring to `dataset`, "modern browsers" specifically excludes versions of Internet Explorer less than 11. See [caniuse.com](http://caniuse.com/#search=dataset) for more up to date information.*

## Getting an attribute
Some attributes are directly accessible as properties of the element (e.g. `alt`, `href`, `id`, `title` and `value`).

    var a = document.querySelector("a"),
       url = a.href;

Other attributes, including data-attributes can be accessed as follows:

    var a = document.querySelector("a"),
       tooltip = a.getAttribute("aria-label");

Data attributes can also be accessed using `dataset` (modern browsers)

    // <a href="#" data-tracking-number="ABC-123">Widget</a>
    var a = document.querySelector("a"),
      tracker = a.dataset.trackingNumber;


## Setting an attribute
Some attributes are directly accessible as properties of the element (e.g. alt, href, id, title and value).

    document.querySelector("a").href = "#top";

Other attributes, including data-attributes can be set as follows:

    document.querySelector("a").setAttribute("aria-label", "I like turtles");

Data attributes can also be set using dataset (modern browsers)

    var a = document.querySelector("a");
    a.dataset.test = "123";
    a.dataset['test-2'] = "456";

results in

    <a href="#" data-test="123" data-test-2="456">Widget</a>


## Removing an attribute
To remove an attribute, including directly accessible properties

    document.querySelector("a").removeAttribute("title");

Data attributes can also be removed as follows (modern browsers):

    // remove "data-foo" attribute
    delete document.querySelector("a").dataset.foo;



