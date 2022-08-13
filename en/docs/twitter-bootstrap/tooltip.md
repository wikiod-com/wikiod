---
title: "Tooltip"
slug: "tooltip"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

The tooltip is a user interface element that  looks like a small pop-up box. It is usually triggered when a user hovers their pointer over an other element, without clicking it.



For performance reasons, tooltips must be initialized with jQuery. The following code will enable all tooltips in the DOM:

    <script>
       $(document).ready(function(){
           $('[data-toggle="tooltip"]').tooltip();
       });
    </script>

## Positioning Tooltips
By default, the tooltip will appear on top of the element. We can use
`data-placement` attribute to set the position of the tooltip on top, bottom, left or the right side of the element.

    <a href="#" data-toggle="tooltip" data-placement="top" title="Top tooltip">Hover</a>
    <a href="#" data-toggle="tooltip" data-placement="bottom" title="Bottom tooltip">Hover</a>
    <a href="#" data-toggle="tooltip" data-placement="left" title="Left tooltip">Hover</a>
    <a href="#" data-toggle="tooltip" data-placement="right" title="Right tooltip">Hover</a

[![enter image description here][1]][1]

 We can also use `data-placement="auto"`, to dynamically reorient the tooltip. The tooltip in the next example the tooltip will display to the left when possible, otherwise it will display right.

    <a href="#" data-toggle="tooltip" data-placement="auto left" title="To the left?">Hover</a


  [1]: http://i.stack.imgur.com/jUxd7.png

## Basic Example

To create a tooltip, we only need to add `data-toggle="tooltip"` attribute and a `title` to the HTML element that will have the tooltip. Title attribute is used to specify the text that is displayed inside the tooltip.

    <span data-toggle="tooltip" title="Hello world!">Hover over me</span>

[![enter image description here][1]][1]

  [1]: http://i.stack.imgur.com/vQQv6.png

