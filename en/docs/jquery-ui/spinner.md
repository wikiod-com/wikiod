---
title: "Spinner"
slug: "spinner"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
 - $( "#id" ).spinner();
 - $( "#id" ).spinner({min:0,max:100,step:5,spin:function( event, ui ) {}});

## Parameters
| Parameters | Detail |
| ---------- | ------ |
| min | Minimum value |
| max | Maximum value |
| step | How much the value increases by on spinner click, can be decimal |
| spin | Can be used to check the spinner value, `ui.value` and do something |

Official [Example][1]


Official [Documentation][2]


  [1]: https://jqueryui.com/spinner/
  [2]: https://api.jqueryui.com/spinner/

## Basic Example
Makes entering numbers a bit handier by showing a set of arrows on the right side of the `input`.

HTML

    <link rel="stylesheet" href="//code.jquery.com/ui/1.12.0/themes/base/jquery-ui.css">
    <script src="https://code.jquery.com/jquery-1.12.4.js"></script>
    <script src="https://code.jquery.com/ui/1.12.0/jquery-ui.js"></script>
    <script src="/resources/demos/external/jquery-mousewheel/jquery.mousewheel.js"></script>
    <script>
      $( function() {
        var spinner = $( "#spinner" ).spinner();
      } );
    </script>
     
    <input id="spinner" name="value">


