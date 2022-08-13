---
title: "Cursor Styling"
slug: "cursor-styling"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

## Syntax
 - cursor: auto | default | none | context-menu | help | pointer | progress | wait | cell | crosshair | text | vertical-text | alias | copy | move | no-drop | not-allowed | e-resize | n-resize | ne-resize | nw-resize | s-resize | se-resize | sw-resize | w-resize | ew-resize | ns-resize | nesw-resize | nwse-resize | col-resize | row-resize | all-scroll | zoom-in | zoom-out | grab | grabbing;

## Changing cursor type
    cursor: value;

[![visualization][1]][1]

**Examples:**

| Value | Description |
| ----- | ----------- |
| none | No cursor is rendered for the element |
| auto | Default. The browser sets a cursor |
| help | The cursor indicates that help is available |
| wait | The cursor indicates that the program is busy |
| move | The cursor indicates something is to be moved |
| pointer | The cursor is a pointer and indicates a link |  


  [1]: http://i.stack.imgur.com/E76ws.png

## pointer-events
The pointer-events property allows for control over how HTML elements respond to mouse/touch events.

    .disabled {
      pointer-events: none;
    }

In this example, 

> 'none' prevents all click, state and cursor options on the specified HTML
> element [\[1\]]

Other valid values for HTMl elements are:
- auto;
- inherit.

1. https://css-tricks.com/almanac/properties/p/pointer-events/

Other resources:

 - https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events
   
 - https://davidwalsh.name/pointer-events

  [1]: https://css-tricks.com/almanac/properties/p/pointer-events/

## caret-color
The caret-color CSS property specifies the color of the caret, the visible indicator of the insertion point in an element where text and other content is inserted by the user's typing or editing.

HTML

    <input id="example" />
    
CSS

    #example {
      caret-color: red;
    }

Resources:

 - https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color

