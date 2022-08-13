---
title: "Draggable"
slug: "draggable"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Simple Example
Enable draggable functionality on any DOM element.

    <script>
      $(function() {
        $( "#draggable" ).draggable();
      });
    </script>
    <div id="draggable" class="ui-widget-content">
      <p>Drag me around</p>
    </div>


## Draggable with handle
You can use any element as an handle to drag another element around:

    <script>
      $(function() {
          $( "#draggable" ).draggable({
             handle: ".handle"
          });
      });
    </script>
    <div id="draggable">
        <span class="handle">Handle</span>
        <div>Content</div>
    </div>

----

[Fiddle](https://jsfiddle.net/m0rvpd5s/)

