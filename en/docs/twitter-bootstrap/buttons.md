---
title: "Buttons"
slug: "buttons"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
 - Classes: .btn-default | .btn-primary | .btn-success | .btn-info |
   .btn-warning | .btn-danger |.btn-link;
 - Sizes: .btn-lg | .btn-md | .btn-sm | .btn-xs;
 - State: active | dissabled.

## Button Classes
Bootstrap provides multiple classes for styling buttons and making them stand out.

Bootstrap buttons can be created by adding the `.btn` class to an element.



| Bootstrap Class | Role (color) |
| ------ | ------ |
| `.btn-default`   | Standard button (white)  |
| `.btn-primary`   | Provides extra visual weight and identifies the primary action (blue) |
| `.btn-success`   | Used to indicate a successful action (green)  |
| `.btn-info`   | Contextual button for providing information (light blue)  |
| `.btn-warning`   | Indicates caution should be applied by the user (yellow)  |
| `.btn-danger`   | Indicates a dangerous or negative action (red)  |
| `.btn-link`   | Make you button look like an anchor tag.   |

**Button Sizes**

You can also create different sizes of buttons with the `.btn-size` classes

| Bootstrap Class | Result |
| ------ | ------ |
| `.btn-lg`   | Creates a larger sized button   |
| `.btn-sm`   | Creates a smaller sized button   |
| `.btn-xs`   | Creates an extra-small button   |
| `.btn-block`   | Buttons become block-level elements and span the full width of their parent   |

**Make button active**

The `active` class will make a button appear pressed.

    <button type="button" class="btn btn-primary active">Active Primary</button>

**Disable a button**

Adding the `disabled` class to a button will render the button un-clickable and show a forbidden cursor when hovering over it.

    <button type="button" class="btn btn-primary disabled">Disabled Primary</button>

**Render buttons horizontally together**

Multiple buttons can be rendered horizontally with the `.btn-group` class. Simply wrap your buttons inside a container element and give that element the btn-group class.

    <div class="btn-group">
      <button type="button" class="btn btn-primary">Apples</button>
      <button type="button" class="btn btn-primary">Oranges</button>
      <button type="button" class="btn btn-primary">Pineapples</button>
    </div>

**Render buttons vertically**

Apply the `.btn-group-vertical` class to the container element

    <div class="btn-group-vertical">
      <button type="button" class="btn btn-primary">Apples</button>
      <button type="button" class="btn btn-primary">Oranges</button>
      <button type="button" class="btn btn-primary">Pineapples</button>
    </div>

**Make button group take up full width**

Buttons wrapped inside a `.btn-group` element only take up as much width as needed. To make the group span the entire width of the screen, use `.btn-group-justified` instead.

    <div class="btn-group btn-group-justified">
      <a href="#" class="btn btn-primary">Apples</a>
      <a href="#" class="btn btn-primary">Oranges</a>
      <a href="#" class="btn btn-primary">Pineapples</a>
    </div>


