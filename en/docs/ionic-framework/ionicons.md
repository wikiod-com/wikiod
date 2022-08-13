---
title: "Ionicons"
slug: "ionicons"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

In the modern web development it's common practice to use fonts to display icons. Since fonts are vectors, they are resolution independent and can be easily colored through CSS, just to name a few advantages compared to bitmap images etc. Ionicons was created by the same team that Ionic Framework was created by and can be used in any project since they are 100% free and open source. MIT Licensed.

Ionicons can be used on their own or with Ionics CSS components when they have certain styles according to the parent elements.

The homepage and list of the icons can be found here: http://ionicons.com/

## Basic usage
Font icons are usually placed inside a `<i>` tag. Ionic has default css styles for the icons for easy use. The most basic example of use:

    <i class="icon ion-home"></i>

## Extended usage
Ionic has some CSS components where you can use Ionicons as a default which have preset styling. The `range` class in the item `<div>` will apply correct styling to both the input and the icons inside it. Here's an example of a range slider.

    <div class="item range">
      <i class="icon ion-volume-low"></i>
      <input type="range" name="volume">
      <i class="icon ion-volume-high"></i>
    </div>

Another example of Ionicon usage in Ionic tabs which will create a tab like menu. The `tabs-striped tabs-color-assertive` classes define the style of the tabs themselves. Icons are used with simple `<i>` tags and they get their positional styling from the classes applied to the parent divs. 

    <div class="tabs-striped tabs-color-assertive">
      <div class="tabs">
        <a class="tab-item" href="#">
          <i class="icon ion-home"></i>
          Home
        </a>
        <a class="tab-item" href="#">
          <i class="icon ion-gear-b"></i>
          Settings
        </a>
    </div>
  </div>

