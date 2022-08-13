---
title: "Panels"
slug: "panels"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

The panel component in bootstrap is a (bordered) box with some padding around its content, and optionally heading and footer containers.

## Basic example
By default, all the `.panel` does is apply some basic border and padding to contain some content.

    <div class="panel panel-default">
      <div class="panel-body">
        Basic panel example
      </div>
    </div>

## Panel with heading
Easily add a heading container to your panel with `.panel-heading`. You may also include any `<h1>-<h6>` with a `.panel-title` class to add a pre-styled heading. However, the font sizes of `<h1>-<h6>` are overridden by `.panel-heading`.

For proper link coloring, be sure to place links in headings within `.panel-title`.

    <div class="panel panel-default">
      <div class="panel-heading">Panel heading without title</div>
      <div class="panel-body">
        Panel content
      </div>
    </div>
    
    <div class="panel panel-default">
      <div class="panel-heading">
        <h3 class="panel-title">Panel title</h3>
      </div>
      <div class="panel-body">
        Panel content
      </div>
    </div>

## Panel with footer
Wrap buttons or secondary text in `.panel-footer`. Note that panel footers **do not** inherit colors and borders when using contextual variations as they are not meant to be in the foreground.

    <div class="panel panel-default">
      <div class="panel-body">
        Panel content
      </div>
      <div class="panel-footer">Panel footer</div>
    </div>

