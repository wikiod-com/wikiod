---
title: "Bootstrap Dropdowns"
slug: "bootstrap-dropdowns"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Parameters
| **Methods** | Example |
| ------ | ------ |
| Call Via Javascript   | `$('.dropdown-toggle').dropdown();`   |
| Toggles the dropdown   | `$('.dropdown-toggle').dropdown('toggle')`   |
| **Event Type** | **Description** |
|show.bs.dropdown | This event fires immediately when the show instance method is called.|
|shown.bs.dropdown | This event is fired when the dropdown has been made visible to the user (will wait for CSS transitions, to complete).|
|hide.bs.dropdown| This event is fired immediately when the hide instance method has been called.|
|hidden.bs.dropdown| This event is fired when the dropdown has finished being hidden from the user (will wait for CSS transitions, to complete).|
| **Event Handler Example** | `$(element).on('show.bs.dropdown', function () { // do something… })`  |


When calling Dropdown Via Javascript `$('.dropdown-toggle').dropdown()`, the  data-api i.e `data-toggle="dropdown"` still required. [Read More][1]


  [1]: http://getbootstrap.com/javascript/#via-javascript-1

## How to Use

Use `.dropdown` class on parent element of dropdown menu.

Add the .dropdown-menu class to a <ul> element to initialize the dropdown menu plugin.

Call the plugin by Using class `.dropdown-toggle` and the `data-toggle="dropdown"` attribute on a button or a Hyperlink.



## Basic Example
    <div class="dropdown">
      <button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">Dropdown Example
      <span class="caret"></span></button>
      <ul class="dropdown-menu">
        <li><a href="#">Option One</a></li>
        <li><a href="#">Option two</a></li>
        <li><a href="#">More Options</a></li>
      </ul>
    </div>

