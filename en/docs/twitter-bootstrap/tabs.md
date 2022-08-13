---
title: "Tabs"
slug: "tabs"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Basic HTML
<!-- language: lang-html -->
    <ul class="nav nav-tabs" role="tablist">
      <li role="presentation">
        <a href="#id-of-content-1" role="tab" data-toggle="tab">Tab 1</a>
      </li>
      <li role="presentation">
        <a href="#id-of-content-2" role="tab" data-toggle="tab">Tab 2</a>
      </li>
      <li role="presentation">
        <a href="#id-of-content-3" role="tab" data-toggle="tab">Tab 3</a>
      </li>
    </ul>

    <div class="tab-content">
      <div role="tabpanel" id="id-of-content-1" class="tab-pane">Tab content 1</div>
      <div role="tabpanel" id="id-of-content-2" class="tab-pane">Tab content 2</div>
      <div role="tabpanel" id="id-of-content-3" class="tab-pane">Tab content 3</div>
    </div>

This will create a tab set with 3 tabs and 3 associated content divs.

## Animated Tabs
To make tabs fade in, add `.fade` to each `.tab-pane`. The active tab pane must also have `.in` class to make the initial content visible.


    <ul class="nav nav-tabs" role="tablist">
        <li role="presentation">
            <a href="#id-of-content-1" role="tab" data-toggle="tab">
                Tab 1
            </a>
        </li>
        <li role="presentation" class="active">
            <a href="#id-of-content-2" role="tab" data-toggle="tab">
                Tab 2
            </a>
        </li>
        <li role="presentation">
            <a href="#id-of-content-3" role="tab" data-toggle="tab">
                Tab 3
            </a>
        </li>
    </ul>
    
    <div class="tab-content">
        <div role="tabpanel" id="id-of-content-1" class="tab-pane fade">
            Tab content 1
        </div>
        <div role="tabpanel" id="id-of-content-2" class="tab-pane fade active in">
            Tab content 2
        </div>
        <div role="tabpanel" id="id-of-content-3" class="tab-pane fade">
            Tab content 3
        </div>
    </div>



