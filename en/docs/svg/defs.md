---
title: "defs"
slug: "defs"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
 - `<defs>` ... *defined elements* ... `</defs>`

## Parameters
|Parameter| Details|
|---------|--------|
|defs | The defs element has no parameters|

The `<defs>` element is used as a container element for elements that are intended to be used solely by reference and not rendered directly. Elements that would normally be rendered (e.g. `<rect>`, `<circle>`) that are declared inside a `<defs>` block are treated as if their style included `display:none`. 

Although it's not strictly necessary, the SVG spec. recommends putting all gradient, filter, pattern, mask, symbol, and marker definitions within a `defs` block.

## Basic <defs> example
    <svg width="400px" height="400px">
    <defs>
      <rect id="defrect" fill="blue" fill-opacity=".5" x="50" y="50" width="100" height="100"/>
    </defs>
    
    <rect fill="red" x="20" y="20" width="80" height="80"/>
    <use xlink:href="#defrect"/>
    <use xlink:href="#defrect" x="50" y="60"/>
    
    </svg>

Result

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/pbbMR.png

