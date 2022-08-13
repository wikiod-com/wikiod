---
title: "Getting started with jQuery UI Library"
slug: "getting-started-with-jquery-ui-library"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Adding the jQuery UI script & basic usage
To get started with the jQuery UI library, you'll need to add the jQuery script, the jQuery UI script, and the jQuery UI stylesheet to your HTML.

First, [download](http://jqueryui.com/download/) jQuery UI; choose the features you need on the download page. Unzip your download, and put `jquery-ui.css` and `jquery-ui.js` (and `jquery.js`) in a folder where you can use them from your HTML (e.g. with your other scripts and stylesheets.)

jQuery UI depends on jQuery, so remember to include `jquery.js` before `jquery-ui.js`.

    <link rel="stylesheet" href="stylesheets/jquery-ui.css">
    <script src="scripts/jquery.js"></script>
    <script src="scripts/jquery-ui.js"></script>

That's it! You can now use jQuery UI. For example, use the datepicker with the following HTML:

    <input type="text" name="date" id="date">

Then use the following JavaScript:

    $("#date").datepicker();

Which will get you a nice datepicker popup:

[![screenshot][1]][1]

For more, see the **[official "Getting started" gude](http://learn.jquery.com/jquery-ui/getting-started/)**.


  [1]: http://i.stack.imgur.com/FgfrY.png

## Setting up jQuery UI for the First Time Example
The jQuery UI framework helps to extend and increase the User Interface controls for jQuery JavaScript library.

When you wish to use jQuery UI, you will need to add these libraries to your HTML. A quick way to start is using the Content Delivery Network available code sources:

**jQuery Libraries**

    https://code.jquery.com/jquery-3.1.0.js
    https://code.jquery.com/ui/1.12.0/jquery-ui.js

You can choose many different themes for jQuery UI and even Roll your Own Theme. For this example, we will use 'Smoothness'. You add the theme via CSS.

**jQuery UI CSS**

    https://code.jquery.com/ui/1.12.0/themes/smoothness/jquery-ui.css

**Putting it all Together**

When you have downloaded or selected your CDN, you will now want to add these libraries and style sheets to your HTML so that your web page can now make use of the jQuery and jQuery UI. The order in which you load the libraries is important. Call the jQuery library first, and then your jQuery UI library. Since jQuery UI extends jQuery, it must be called after. Your HTML may look something like the following.

    <html>
    <head>
      <title>My First UI</title>
      <link rel="stylesheet" href="https://code.jquery.com/ui/1.12.0/themes/smoothness/jquery-ui.css">
      <script src="https://code.jquery.com/jquery-3.1.0.js"></script>
      <script src="https://code.jquery.com/ui/1.12.0/jquery-ui.js"></script>
      <script>
      $( function() {
        $( "#sortable" ).sortable();
        $( "#sortable" ).disableSelection();
      } );
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



