---
title: "Navs"
slug: "navs"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Bootstrap Navs
Navs available in Bootstrap have shared markup, starting with the base .nav class, as well as shared states. Swap modifier classes to switch between each style.

**Tabs**

    <ul class="nav nav-tabs">
      <li role="presentation" class="active"><a href="#">Home</a></li>
      <li role="presentation"><a href="#">Profile</a></li>
      <li role="presentation"><a href="#">Messages</a></li>
    </ul>

**Pills**

    <ul class="nav nav-pills">
      <li role="presentation" class="active"><a href="#">Home</a></li>
      <li role="presentation"><a href="#">Profile</a></li>
      <li role="presentation"><a href="#">Messages</a></li>
    </ul>

**Justified**

    <ul class="nav nav-tabs nav-justified">
      ...
    </ul>
    <ul class="nav nav-pills nav-justified">
      ...
    </ul>

**With Dropdowns**

    <ul class="nav nav-tabs">
      <li role="presentation" class="dropdown">
        <a class="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
      Dropdown <span class="caret"></span>
        </a>
        <ul class="dropdown-menu">
          ...
        </ul>
      </li>
    </ul>

