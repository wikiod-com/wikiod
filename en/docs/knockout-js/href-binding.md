---
title: "Href binding"
slug: "href-binding"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

There is no `href` binding in the core KnockoutJS library, which is the reason all examples showcase *other* features of the library to get the same effect.

See also [this Stack Overflow question on the same topic](http://stackoverflow.com/q/8123142/419956).

## Using attr binding
<!-- language: lang-html -->

    <a data-bind="attr: { href: myUrl }">link with dynamic href</a>

<!-- language: lang-js -->

    ko.applyBindings({
      myUrl: ko.observable("http://www.stackoverflow.com")
    });

Since there is no native `href` binding in KnockoutJS, you need to use a different feature to get dynamic links. The above example showcases [the built-in `attr` binding][1] to get a dynamic link.


  [1]: https://www.wikiod.com/knockout-js/bindings#Attr

## Custom binding handler
`href` binding is not native to KnockoutJS, so to get dynamic links use a custom binding handler:

<!-- language: lang-html -->

    <a data-bind="href: myUrl">link with dynamic href</a>

<!-- language: lang-js -->

    ko.bindingHandlers['href'] = {
      update: function(element, valueAccessor) {
        element.href = ko.utils.unwrapObservable(valueAccessor());
      }
    };


  [1]: https://www.wikiod.com/knockout-js/bindings#Attr

