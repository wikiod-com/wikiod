---
title: "md-button"
slug: "md-button"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

This topic includes examples on how to create a button and what its' directives and other stuff do.

## Parameters
| **Attribute** | **Description** |
|---|---|
| `md-button` | Creates a rectangular button w/ no elevation. |
| `md-raised-button` | Creates a rectangular button w/ elevation |
| `md-icon-button` | Creates a circular button with a transparent background, meant to contain an icon |
| `md-fab` | Creates a circular button w/ elevation, defaults to theme's accent color |
| `md-mini-fab` | Same as `md-fab` but smaller |
| `disableRipple` | Whether the ripple effect for the button is disabled. |

For more information and more examples, visit [the docs](https://material.angular.io/components/button).

## Simple buttons
To create a button, use the `md-button` attribute for a flat button and `md-raised-button` for an elevated button:

<!-- language: lang-html -->
    
    <button md-button>Button</button>
    <button md-raised-button>Raised Button</button>
    <button md-fab><md-icon>add</md-icon></button>
    <button md-mini-fab><md-icon>check</md-icon></button>
    <button md-icon-button><md-icon>person_add</md-icon></button>

[Plunker Demo](http://plnkr.co/edit/jqMxLKMIiRxCRNInsjxc?p=preview)

**For more information about icons, see the docs on [`md-icon`](https://www.wikiod.com/angular-material2/md-icon).**

