---
title: "Button"
slug: "button"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
 - $( ".selector" ).button();
 - $( ".selector" ).button({   disabled: true });
 - $( ".selector" ).button({   icons: { primary: "ui-icon-gear",
   secondary: "ui-icon-triangle-1-s" } });
 - $( ".selector" ).button({   label: "custom label" });
 - $( ".selector" ).button({   text: false });

## Parameters
| Parameter | Type - Details - Default|
| ------ | ------ |
| `disabled` | `Boolean` - Disables the button if set to true - `false`|
| `icons`| `Object` - Icons to display - { primary: null, secondary: `null` }|
| `label`| `String` - Text to show in the button - `null`|
| `text` | `Boolean` - Whether to show the label - `true`|

## Basic usage
Create an input (or button, or anchor) html element and call `button()` method of jQuery UI.

    <script>
    $(function() {
        $( "#myButton" ).button();
    });
    </script>

HTML

    <input type="button" value="A button" id="myButton">


