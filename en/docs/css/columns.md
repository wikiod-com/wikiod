---
title: "Columns"
slug: "columns"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
 - column-count: auto|number|inherit|initial|unset;
 - column-width: auto|length;
 - column: [column-width]|[column-count];
 - column-span: none|all|inherit|initial|unset;
 - column-gap: normal|length|inherit|initial|unset;
 - column-fill: auto|balance|inherit|intial|unset;
 - column-rule-color: color|inherit|initial|unset;
 - column-rule-style: none|hidden|dotted|dashed|solid|double|groove|ridge|inset|outset|inherit|initial|unset;
 - column-rule-width: thin|medium|thick|length|inherit|initial|unset;
 - column-rule: [column-rule-width]|[columm-rule-style]|[column-rule-color];
 - break-after: auto|always|left|right|recto|verso|page|column|region|avoid|avoid-page|avoid-column|avoid-region;
 - break-before: auto|always|left|right|recto|verso|page|column|region|avoid|avoid-page|avoid-column|avoid-region;
 - break-inside: auto|avoid|avoid-page|avoid-column|avoid-region;


## Simple Example (column-count)
The CSS multi-column layout makes it easy to create multiple columns of text.

**Code**

    <div id="multi-columns">Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum</div>

<!-- language: lang-css -->

    .multi-columns {
      -moz-column-count: 2;
      -webkit-column-count: 2;
      column-count: 2;
    }

**Result**

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/bZHuU.png

## Column Width
The `column-width` property sets the minimum column width. If `column-count` is not defined the browser will make as many columns as fit in the available width.

**Code:**

    <div id="multi-columns">
        Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum
    </div>

<!-- language: lang-css -->

    .multi-columns {
      -moz-column-width: 100px;
      -webkit-column-width: 100px;
      column-width: 100px;
    }

**Result**

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/zYzAz.png

