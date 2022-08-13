---
title: "Bindings - Text and appearance"
slug: "bindings---text-and-appearance"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Text
The `text` binding can be used with any element to update it's innerText.

<!-- language: lang-html -->

    <p>
      <span data-bind="text: greeting"></span>,
      <span data-bind="text: subject"></span>.
    </p>

<!-- language: lang-js -->

    ko.applyBindings({
      greeting: ko.observable("Hello"),
      subject: ko.observable("world")
    });

The `text` binding may also be used with virtual elements.

    <p>
      <!--ko text: greeting--><!--/ko-->, 
      <!--ko text: subject--><!--/ko-->.
    </p>

## Attr
Use the `attr` binding to apply any additional attributes to your element. Most commonly used for setting an href, src, or any data-attributes.

<!-- language: lang-html -->

    <img data-bind="attr: { src: url, title: title }"/>

<!-- language: lang-js -->
     
    var viewModel = {
        url: ko.observable("images/example.png"),
        title: "example title"
    };


## Visible
Can be used to show/hide DOM elements. Similar to using `if`, except that `visible` will still build the element and set `display:none`.

    <div data-bind="visible: shouldShowMessage">
        You will see this message only when "shouldShowMessage" holds a true value.
    </div>
     
    <script type="text/javascript">
        var viewModel = {
            shouldShowMessage: ko.observable(true);
        };
    </script>

## HTML
This binding updates the innerHTML of the element using `jQuery.html()`, if jQuery has been referenced, otherwise, KO's own parsing logic. It can be useful if retrieving HTML from an API, RSS feed, etc.  Be mindful of using this tag with user input HTML.

page.html

    <p>
      <span data-bind="html: demoLink"></span>
    </p>

page.js

    ko.applyBindings({
      demoLink: ko.observable("<a href='#'>Make a link</a>")
    });

## CSS
This binding will apply the supplied CSS class to the element. Static classes are applied when the given conditions are loosely-evaluated to true. Dynamic classes use the value of an observable or computed.


page.html
<!-- language: lang-html -->

    <p data-bind="css: { danger: isInDanger }">Checks external expression</p>
    <p data-bind="css: { danger: dangerLevel() > 10 }">Expression can be inline</p>
    <p data-bind="css: { danger: isInDanger, glow: shouldGlow }">Multiple classes</p>
    <p data-bind="css: dynamicObservable">Dynamic CSS class from observable</p>
    <p data-bind="css: dynamicComputed">Dynamic CSS class from computed</p>

page.js
<!-- language: lang-js -->

    ko.applyBindings({
      isInDanger: ko.observable(true),
      dangerLevel: ko.observable(5),
      isHot: ko.observable(true),
      shouldGlow: ko.observable(true),  
      dynamicObservable: ko.observable('highlighted'),
      dynamicComputed: ko.computed(function() {
            var customClass = "";
            if(dangerLevel() >= 15 ) {
                customClass += " danger";
            }
            if(dangerLevel() >= 10) {
                customClass += " glow";
            }
            if(dangerLevel() >= 5) {
                customClass += " highlighted";
            }
            return customClass;
        });
    });

page.css
<!-- language: lang-css -->

    .danger { background: red; }
    .glow { box-shadow: 5px 5px 5px gold; }
    .highlighted { color: purple; }

See also: [official documentation][1].


  [1]: http://knockoutjs.com/documentation/css-binding.html

