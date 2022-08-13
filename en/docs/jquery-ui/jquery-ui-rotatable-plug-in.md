---
title: "jQuery UI Rotatable Plug-in"
slug: "jquery-ui-rotatable-plug-in"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| -------- | ------- |
| handle | url to a custom image for the handle |
| angle | the starting rotation for the element. |
| rotationCenterX | position about which the element will be rotated |
| rotationCenterY | position about which the element will be rotated |
| step | an angle in degrees that the rotation will snap to if the shift key is held. |
| snap | snaps to step in degrees.|
| start | triggered when rotation starts |
| stop | triggered when rotation stops |
| rotate | triggered when object is being rotated |
| wheelRotate | enable/disable mouse wheel to rotate element. |

## Initial Usage Example
> jquery-ui-rotatable is a plugin for jQuery UI that works in a similar way to Draggable and Resizable, without being as full-featured. By default, it puts a small rotation icon in the bottom left of whatever element you want to make rotatable.

    <html>
      <head>
        <title>My Rotatable</title>
        <link rel="stylesheet" href="http://code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css">
        <link rel="stylesheet" href="//cdn.jsdelivr.net/jquery.ui.rotatable/1.0.1/jquery.ui.rotatable.css">
        <script src="http://code.jquery.com/jquery-1.11.3.js"></script>
        <script src="http://code.jquery.com/ui/1.11.4/jquery-ui.js"></script>
        <script src="//cdn.jsdelivr.net/jquery.ui.rotatable/1.0.1/jquery.ui.rotatable.min.js"></script>
        <script>
        $(function(){
          $('#target').rotatable();
        });
        </script>
      </head>
      <body>
      <div id="target">Rotate me!</div>
      </body>
    </html>

