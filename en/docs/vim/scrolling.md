---
title: "Scrolling"
slug: "scrolling"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Scrolling downwards
Command | Description 
--- | ---
<kbd>Ctrl+E</kbd> | Scroll one line down.
<kbd>Ctrl+D</kbd> | Scroll half a screen down (configurable using the `scroll` option).
<kbd>Ctrl+F</kbd> | Scroll a full screen down.
<kbd>z+</kbd> | Draw the first line below the window at the top of the window.

## Scrolling upwards
Command | Description 
--- | ---
<kbd>Ctrl+Y</kbd> | Scroll one line up.
<kbd>Ctrl+U</kbd> | Scroll half a screen up (configurable using the `scroll` option).
<kbd>Ctrl+B</kbd> | Scroll a full screen up.
<kbd>z^</kbd> | Draw the first line above the window at the bottom of the window.

## Scrolling relative to cursor position
Command | Description 
--- | ---
<kbd>z<CR></kbd> | Redraw current line at the top of the window and put the cursor on the first non-blank character on the line.
<kbd>zt</kbd> | Like <kbd>z<CR></kbd> but leave the cursor in the same column.
<kbd>z.</kbd> | Redraw current line at the center of the window and put the cursor on the first non-blank character on the line.
<kbd>zz</kbd> | Like <kbd>z.</kbd> but leave the cursor in the same column.
<kbd>z-</kbd> | Redraw current line at the bottom of the window and put the cursor on the first non-blank character on the line.
<kbd>zb</kbd> | Like <kbd>z-</kbd> but leave the cursor in the same column.

