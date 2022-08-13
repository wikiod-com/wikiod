---
title: "Dropdowns"
slug: "dropdowns"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

For more information, visit the official Bootstrap documentation located at http://getbootstrap.com/javascript/#dropdowns, where the basic HTML usage example is derived from.

## Basic HTML usage
A Bootstrap dropdown is a Bootstrap component that allows an HTML element trigger the display of a sub-menu dropdown upon the element being clicked.

Here is a basic HTML usage example:

    <div class="dropdown">
      <button id="dLabel" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
        Dropdown trigger
        <span class="caret"></span>
      </button>
      <ul class="dropdown-menu" aria-labelledby="dLabel">
        ...
      </ul>
    </div>

Dropdown sub-menu items can be specified by inserting `li` elemented within the `ul` element with the `.dropdown-menu` class.

