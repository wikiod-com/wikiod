---
title: "Using CSS styles"
slug: "using-css-styles"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The interfaces detailed herein were introduced in [DOM Level 2 Style](https://www.w3.org/TR/DOM-Level-2-Style/), which came out at approximately the same time as [DOM Level 2 Core](https://www.w3.org/TR/DOM-Level-2-Core/) and is thus considered "part of DOM version 2".

## Reading and changing inline styles
## Inline style

You can manipulate the inline CSS style of an HTML element by simply reading or editing its `style` property.

Assume the following element:

    <div id="element_id" style="color:blue;width:200px;">abc</div>

With this JavaScript applied:

    var element = document.getElementById('element_id');

    // read the color
    console.log(element.style.color); // blue

    //Set the color to red
    element.style.color = 'red';

    //To remove a property, set it to null
    element.style.width = null;
    element.style.height = null;

However, if `width: 200px;` were set in an external CSS stylesheet, `element.style.width = null` would have no effect. In this case, to reset the style, you would have to set it to `initial`: `element.style.width = 'initial'`.


## Reading and changing styles from a stylesheet
`element.style` only reads CSS properties set inline, as an element attribute. However, styles are often set in an external stylesheet. The actual style of an element can be accessed with `window.getComputedStyle(element)`. This function returns an object containing the actual computed value of all the styles.

Similar to the Reading and changing inline styles example, but now the styles are in a stylesheet:

    <div id="element_id">abc</div>
    <style type="text/css">
        #element_id {
            color:blue;
            width:200px;
        }
    </style>

JavaScript:

    var element = document.getElementById('element_id');

    // read the color
    console.log(element.style.color); // '' -- empty string
    console.log(window.getComputedStyle(element).color); // rgb(0, 0, 255)

    // read the width, reset it, then read it again
    console.log(window.getComputedStyle(element).width); // 200px
    element.style.width = 'initial';
    console.log(window.getComputedStyle(element).width); // 885px (for example)

