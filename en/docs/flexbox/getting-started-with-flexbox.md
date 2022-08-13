---
title: "Getting started with flexbox"
slug: "getting-started-with-flexbox"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Flexbox Containers and Items
Flexbox or flexible box is a layout method for arranging content on a page in a predictable manner. Flexbox provides an improvement over traditional block model positioning using floats or even table like positioning for content on the page. 

At its core, Flexbox can be broken down into a parent element (flex container) and a child element (flex item).

**Flex Container**

A flex container can be created by setting its display property to flex:

<!-- language: lang-css -->
    .container {
      display: flex;
    }

**Flex item**

Every child element of a flex container becomes a flex item. These flex items can then receive additional properties to modify how its positioned on the page.

<!-- language: lang-css -->
    .item {
      flex: 1;
    }

This `flex: 1` property is shorthand for `flex-grow: 1` enabling it to grow relative to its siblings within the container.

Putting these together this is the HTML markup:

<!-- language: lang-html -->
    <div class="container">
      <div class="item"></div>
      <div class="item"></div>
      <div class="item"></div>
    </div>

## Flexbox Introduction
The Flexbox (Flexible box) layout, introduced in CSS3 provides a more efficient way to lay out, align and distribute space among children elements(`flex items`) within a container element(`flex container`). Most importantly even when their sizes are unknown or dynamic and hence the term "flex" or "flexible".

Lets start with the basics and see how an container can be initialized as flex.

Consider the following markup:

    <div class="flex-container">
      <div class="flex-item-1"></div>
      <div class="flex-item-2"></div>
      <div class="flex-item-3"></div>
    </div>

A flexbox is initialized by simply using `display: flex`. Done! 

    .flex-container {
      height: 100px;
      padding: 10px;
      background: grey;

      display: flex; // or display: inline-flex;
    }

Now that the flex container is ready, let us play around with its flex items giving them a width of say 25% each and horizontally center aligning the flex items inside their parent container.
    
    .flex-item-1 {
      width: 25%;
      background: green;
    }
    
    .flex-item-2 {
      width: 25%;
      background: purple;
    }
    
    .flex-item-3 {
      width: 25%;
      background: pink;
    }

Please note, I have purposely given the 3 children a width of 25% each to show how easy it is to move around the children within a flexbox.

Now when we run the above code, we should see the 3 children within the container horizontally next to each other. This is because by default, a flexbox has the property `flex-direction: row`. Meaning by default the flex items will be aligned horizontally next to each other. Also there should be a gap on the right since the total width of the children didn't add upto 100%.

Now try adding the property `justify-content: center` to the `flex-container`.

    .flex-container {
      ...
      justify-content: center;
    }

The result would be the children being horizontally center aligned. The same property has other values such as `flex-end`(align to the right), `space-around`(equal space around the items), `space-between`(equal space between the items).

> *Important: Other block element properties like `text-align: center` etc. has no effect on a flex element.*

## Installation or Setup
Flexbox is a CSS3 module, standardized by the [World Wide Web Consortium](https://www.w3.org/). It is a layout mode for element arrangement such that the elements behave predictably when the page layout must accommodate different display sizes.

Because it is a part of CSS3, you don't need to install anything. It can be used as long as the browser supports it, and most modern browsers do. To check if your browser supports it or not, here is the [compatibility chart](http://caniuse.com/#feat=flexbox).

To set up and use `flexbox` in your CSS, simply add `display: flex` to a selector.


