---
title: "pointer-events"
slug: "pointer-events"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

With the `pointer-events` property, you can control wich part of your drawing will react to pointer events.

## none
 the most common use case is to set pointer-events to `none` to prevent certain shapes or all of your drawing to capture mouse events, and to let the shapes underneath them to receive the events.

If you hover over the area where the red circle  overlaps the blue circle, the blue circle will still receive the mouse events, as pointer-events is set to `none` 

    <svg viewBox="0 0 150 100">
        <style>
            .target:hover{fill:green}
        </style>
        <circle class="target" cx="50" cy="50" r="50" fill="blue"/>
        <circle cx="100" cy="50" r="50" fill="red" pointer-events="none"/>
    </svg> 

## fill
Setting `pointer-events="fill"` lets you receive mouse events on a shape even if its fill is set to `none`

    <svg viewBox="0 0 100 100">
        <style>
            circle:hover{fill:green}
        </style>
        <circle class="target" cx="50" cy="50" r="50" fill="none"/>
    </svg>

