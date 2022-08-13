---
title: "jquery ui sortable"
slug: "jquery-ui-sortable"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## jQuery UI Sortable - Drop Placeholder
This example of the Sortable using a Placeholder is common usage. Sortable is applied to a group of DOM elements, allowing the user to move items around in the list via Drag'n Drop style actions.

    <!doctype html>
    <html lang="en">
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <title>jQuery UI Sortable - Drop Placeholder</title>
      <link rel="stylesheet" href="//code.jquery.com/ui/1.12.0/themes/base/jquery-ui.css">
      <style>
      #sortable {
        list-style-type: none;
        margin: 0;
        padding: 0;
        width: 60%;
      }
      #sortable li {
        margin: 0 5px 5px 5px;
        padding: 5px;
        font-size: 1.2em;
        height: 1.5em;
      }
      html>body #sortable li {
        height: 1.5em; line-height: 1.2em;
      }
      .ui-state-highlight {
        height: 1.5em;
        line-height: 1.2em;
      }
      </style>
      <script src="https://code.jquery.com/jquery-3.1.0.js"></script>
      <script src="https://code.jquery.com/ui/1.12.0/jquery-ui.js"></script>
      <script>
      $( function() {
        $( "#sortable" ).sortable({
          placeholder: "ui-state-highlight"
        }).disableSelection();
      });
      </script>
    </head>
    <body>
     
    <ul id="sortable">
      <li class="ui-state-default">Item 1</li>
      <li class="ui-state-default">Item 2</li>
      <li class="ui-state-default">Item 3</li>
      <li class="ui-state-default">Item 4</li>
      <li class="ui-state-default">Item 5</li>
      <li class="ui-state-default">Item 6</li>
      <li class="ui-state-default">Item 7</li>
    </ul>
     
    </body>
    </html>

