---
title: "Dialog"
slug: "dialog"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Syntax
 - $( ".selector" ).dialog( "option", "disabled" ); // Option Getter, specific
 - $( ".selector" ).dialog( "option" );  // Option Getter all
 - $( ".selector" ).dialog( "option", "disabled", true ); // Option Setter, specific
 - $( ".selector" ).dialog( "option", { disabled: true } ); // Option Setter, multiple
 - $( ".selector" ).dialog( "close" ); // Triggers close
 - $( ".selector" ).dialog({ close: function() {} }); // Close overloading
 - $( ".selector" ).on( "dialogclose", function( event, ui ) {} ); // Close overloading

## Parameters
| Parameter | Description |
| ----- | ----- |
| **Options** | &nbsp; |
| appendTo | (Selector) [Default: `"body"`] Which element the dialog (and overlay, if modal) should be appended to. |
| autoOpen | (Boolean) [Default: `true`] If set to true, the dialog will automatically open upon initialization. If false, the dialog will stay hidden until the open() method is called. |
| buttons | (Object/Array) Specifies which buttons should be displayed on the dialog. The context of the callback is the dialog element; if you need access to the button, it is available as the target of the event object. |
| closeOnEscape | (Boolean) [Default: `true`] Specifies whether the dialog should close when it has focus and the user presses the escape (ESC) key. |
| closeText | (String) [Default: `"close"`] Specifies the text for the close button. Note that the close text is visibly hidden when using a standard theme. |
| dialogClass | (String) The specified class name(s) will be added to the dialog, for additional theming. |
| draggable | (Boolean) [Default: `true`] If set to `true`, the dialog will be draggable by the title bar. Requires the jQuery UI Draggable widget to be included. |
| height | (Number/String) [Default: `"auto"`] The height of the dialog. |
| hide | (Bool/Num/Str/Obj) If and how to animate the hiding of the dialog. |
| maxHeight | (Number) [Default: `false`] The maximum height to which the dialog can be resized, in pixels. |
| maxWidth | (Number) [Default: `false`] The maximum width to which the dialog can be resized, in pixels. |
| minHeight | (Number) [Default: `150`] The minimum height to which the dialog can be resized, in pixels. |
| minWidth | (Number) [Default: `150`] The minimum width to which the dialog can be resized, in pixels. |
| modal | (Boolean) [Default: `false`] If set to true, the dialog will have modal behavior; other items on the page will be disabled, i.e., cannot be interacted with. Modal dialogs create an overlay below the dialog but above other page elements. |
| position | (Object) [Default: `{ my: "center", at: "center", of: window }`] Specifies where the dialog should be displayed when opened. The dialog will handle collisions such that as much of the dialog is visible as possible.
| resizable | (Boolean) [Default: `true`] If set to true, the dialog will be resizable. Requires the jQuery UI Resizable widget to be included. |
| show | (Bool/Num/Str/Obj) If and how to animate the showing of the dialog. |
| title | (String) Specifies the title of the dialog. If the value is null, the title attribute on the dialog source element will be used. |
| width | (Number) [Default: `300`] The width of the dialog, in pixels. |
| **Methods** | &nbsp; |
| close | Closes the dialog. |
| destroy | Removes the dialog functionality completely. This will return the element back to its pre-init state. |
| instance | Retrieves the dialog's instance object. If the element does not have an associated instance, undefined is returned. |
| isOpen | Whether the dialog is currently open. |
| moveToTop | Moves the dialog to the top of the dialog stack. |
| open | Opens the dialog. |
| option | Gets the value currently associated with the specified `optionName`. |
| option | Gets an object containing key/value pairs representing the current dialog options hash. |
| option | Sets one or more options for the dialog. |
| widget | Returns a jQuery object containing the generated wrapper. |
| **Extension Points** | &nbsp; |
| _allowInteraction | (event) Modal dialogs do not allow users to interact with elements behind the dialog. This can be problematic for elements that are not children of the dialog, but are absolutely positioned to appear as though they are. The `_allowInteraction()` method determines whether the user should be allowed to interact with a given target element; therefore, it can be used to whitelist elements that are not children of the dialog but you want users to be able to use. |
| **Events** | &nbsp; |
| beforeClose | (event, ui) Triggered when a dialog is about to close. If canceled, the dialog will not close. |
| close | (event, ui) Triggered when the dialog is closed. |
| create | (event, ui) Triggered when the dialog is created. |
| drag | (event, ui { position, offset }) Triggered while the dialog is being dragged. |
| dragStart | (event, ui { position, offset }) Triggered when the user starts dragging the dialog. |
| dragStop | (event, ui { position, offset }) Triggered after the dialog has been dragged. |
| focus | (event, ui) Triggered when the dialog gains focus. |
| open | (event, ui) Triggered when the dialog is opened. |
| resize | (event, ui { originalPosition, position, originalSize, size }) Triggered while the dialog is being resized. |
| resizeStart | (event, ui { originalPosition, position, originalSize, size }) Triggered while the dialog is being resized. |
| resizeStop | (event, ui { originalPosition, position, originalSize, size }) Triggered while the dialog is being resized. |

Parameter Source: http://api.jqueryui.com/dialog/

## Creating a Dialog with Tabbed Titlebar
Occasionally, we may want to display dialogs with more than one pane of content.  jQuery UI offers tabs that can be used in tandem with a dialog to make this possible.  While it may be more common to have tabs within a dialog's content container, this example will demonstrate how to make a list of tabs the titlebar of the dialog.

**HTML**

    <button id="openButton">
      Open Dialog
    </button>
    <div id="dialog" style="display:none">
      <div class="ui-tabs">
        <ul>
          <li><a href="#tab_1">Tab 1</a></li>
          <li><a href="#tab_2">Tab 2</a></li>
        </ul>
        <div id="tab_1">
          <p>Tab 1 content...</p>
        </div>
        <div id="tab_2">
          <p>Tab 2 content...</p>
        </div>
      </div>
    </div>

**jQuery**

    $(document).ready(function() {
      // Options to pass to the jQuery UI Dialog
      var options = {
        position: {
          my: "left top",
          at: "left top",
          of: window
        },
        autoOpen: false
      };
    
      /* Initialization */
      // Initialize the dialog
      var dialog = $("#dialog").dialog(options);
    
      // Initialize the tabs
      var tabs = $(".ui-tabs").tabs();
    
      /* Gather Elements Before Rearrangement */
      var closeButton = dialog.siblings(".ui-dialog-titlebar").find(".ui-dialog-titlebar-close");
      var initialTitlebar = dialog.siblings(".ui-dialog-titlebar");
      
      // Find the list of tabs to make the titlebar, add the ui-dialog-titlebar class, and append the close button
      var tabbedTitlebar = dialog.find(".ui-tabs ul:first").addClass("ui-dialog-titlebar").append(closeButton);
    
      /* Arranging */
      // Remove the initialTitlebar
      $(initialTitlebar).remove();
    
      // Create a new .ui-tabs container for the tabbedTitlebar
      var tabbedTitlebarContainer = $("<div>", {
        class: "ui-tabs"
      }).append(tabbedTitlebar);
    
      // Prepend the tabbedTitlebarContainer to the dialog container
      dialog.parents(".ui-dialog").prepend(tabbedTitlebarContainer);
    
      /* Show the Dialog */
      dialog.dialog("open");
    
      var openButton = $("#openButton").button().click(function() {
        dialog.dialog("open");
      });
    });

Working example for reference: https://jsfiddle.net/5x4zz681/

## Simple Example
Dialog is a window which is overlay positioned within the viewport.

    <script>
      $(function() {
        $( "#dialog" ).dialog();
      });
    </script>
    <div id="dialog" title="Basic dialog">
      <p>This is the default dialog which is useful for displaying information. The dialog window can be moved, resized and closed with the 'x' icon.</p>
    </div>

## Open dialog when event occurs
Usually we want to separate the creation of the dialog from its appearance. Then three steps are needed.

 1. Create base element
 

    <div id="dialog" title="Basic dialog">
      <p>This is the default dialog which is useful for displaying information. The dialog window can be moved, resized and closed with the 'x' icon.</p>
    </div>

 

 2. Make it a dialog, note the `autoOpen: false` option that ensures that it will be closed at first


    $( "#dialog" ).dialog({
      autoOpen: false
    });

3. Open it when needed, like on a button click


    $( "#dialog" ).dialog( "open" );

## Complex Example - jQuery UI Dynamicly Create Dialog
Generally, dialog relies on a `div` within the HTML. Sometimes you may want to create a dialog from scratch, programmatically. Here is an example of a complex modal dialog created dynamically with interactive functions.

**HTML**

    <div id="users-contain" class="ui-widget">
      <h1>Existing Users:</h1>
      <table id="users" class="ui-widget ui-widget-content">
        <thead>
          <tr class="ui-widget-header ">
            <th>Name</th>
            <th>Email</th>
            <th>Password</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>John Doe</td>
            <td>john.doe@example.com</td>
            <td>johndoe1</td>
          </tr>
        </tbody>
      </table>
    </div>
    <button id="create-user">Create new user</button>

**CSS**

    label,
    input {
      display: block;
    }
    
    input.text {
      margin-bottom: 12px;
      width: 95%;
      padding: .4em;
    }
    
    fieldset {
      padding: 0;
      border: 0;
      margin-top: 25px;
    }
    
    h1 {
      font-size: 1.2em;
      margin: .6em 0;
    }
    
    div#users-contain {
      width: 350px;
      margin: 20px 0;
    }
    
    div#users-contain table {
      margin: 1em 0;
      border-collapse: collapse;
      width: 100%;
    }
    
    div#users-contain table td,
    div#users-contain table th {
      border: 1px solid #eee;
      padding: .6em 10px;
      text-align: left;
    }
    
    .ui-dialog .ui-state-error {
      padding: .3em;
    }
    
    .validateTips {
      border: 1px solid transparent;
      padding: 0.3em;
    }

**jQuery**

    $(function() {
      // Define variables for the dialog, form and a regular expression used to verify email addresses in the form
      var dialog, form,
        emailRegex = /^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;
    
      // Function to update tips when an issue in the form is detected
      // t = text to enter as the tip
      function updateTips(t) {
        tips
          .text(t)
          .addClass("ui-state-highlight");
        setTimeout(function() {
          tips.removeClass("ui-state-highlight", 1500);
        }, 500);
      }
    
      // Function to check the length of text entered into a field
      // o = object reference (object), n = name of field (string), min = minimum number of characters (int), max = maximum number of characters (int)
      function checkLength(o, n, min, max) {
        if (o.val().length > max || o.val().length < min) {
          o.addClass("ui-state-error");
          updateTips("Length of " + n + " must be between " +
            min + " and " + max + ".");
          return false;
        } else {
          return true;
        }
      }
    
      // Function to perform regular expression check of text entered in field
      // o = object reference (object), regexp = regular expression reference (RegExp Object), n = name of field
      function checkRegexp(o, regexp, n) {
        if (!(regexp.test(o.val()))) {
          o.addClass("ui-state-error");
          updateTips(n);
          return false;
        } else {
          return true;
        }
      }
    
      //Function called when form is submitted that will check all the form fields. If all fields have text and all the text meets the requirements, the data is collected and added back to the table.
      function addUser() {
        var valid = true;
        allFields.removeClass("ui-state-error");
    
        valid = valid && checkLength(name, "username", 3, 16);
        valid = valid && checkLength(email, "email", 6, 80);
        valid = valid && checkLength(password, "password", 5, 16);
    
        valid = valid && checkRegexp(name, /^[a-z]([0-9a-z_\s])+$/i, "Username may consist of a-z, 0-9, underscores, spaces and must begin with a letter.");
        valid = valid && checkRegexp(email, emailRegex, "eg. ui@jquery.com");
        valid = valid && checkRegexp(password, /^([0-9a-zA-Z])+$/, "Password field only allow : a-z 0-9");
    
        if (valid) {
          $("#users tbody").append("<tr>" +
            "<td>" + name.val() + "</td>" +
            "<td>" + email.val() + "</td>" +
            "<td>" + password.val() + "</td>" +
            "</tr>");
          dialog.dialog("close");
        }
        return valid;
      }
    
      // Creation of the dialog object
      dialog = $("<div>", {
        id: "dialog-form",
        title: "Create New User"
      }).dialog({
        autoOpen: false,
        height: 400,
        width: 350,
        modal: true,
        buttons: {
          "Create an account": addUser,
          Cancel: function() {
            dialog.dialog("close");
          }
        },
        close: function() {
          form[0].reset();
          allFields.removeClass("ui-state-error");
        }
      });

      // Adding elements to the dialog to be shown
      dialog.html("<p class='validateTips'>All form fields are required.</p>")
    
      // Creation of the form object to be shown inside the dialog
      form = $("<form>").submit(function(e) {
        e.preventDefault();
        addUser();
      }).appendTo(dialog);

      // Adding elements to the form, fieldset and fields    
      form.append($("<fieldset>"));
      var markup = "";
      markup += "<label for='name'>Name</label>\r\n";
      markup += "<input type='text' name='name' id='name' value='Jane Smith' class='text ui-widget-content ui-corner-all'>";
      markup += "<label for='email'>Email</label><input type='text' name='email' id='email' value='jane@smith.com' class='text ui-widget-content ui-corner-all'>\r\n";
      markup += "<label for='password'>Password</label><input type='password' name='password' id='password' value='xxxxxxx' class='text ui-widget-content ui-corner-all'>\r\n";
      markup += "<input type='submit' tabindex='-1' style='position:absolute; top:-1000px'>\r\n";

      // Assigning our fields HTML markup to the fieldset
      form.find("fieldset").html(markup);
    
      // Assigning variables to be used for easy reference, post creation and amendment of dynamic objects
      var name = $("#name"),
        email = $("#email"),
        password = $("#password"),
        allFields = $([]).add(name).add(email).add(password),
        tips = $(".validateTips");
    
      // Override the click event of the button to launch our dynamic dialog
      $("#create-user").button().on("click", function() {
        dialog.dialog("open");
      });
    });

Working example for reference: https://jsfiddle.net/Twisty/LqjuxLu1/

## Dialog with no close button
If you like to show the dialog without the close button (i.e. the x button in the upper-right corner of the dialog), perhaps because you want to force the user to select one of options or buttons in the dialog itself:

1- Give your dialog a CSS class:

    $("#selector").dialog({
        closeOnEscape: false,
        dialogClass: "dialog-no-close",
    });

2- Hide the close button using this CSS:

    .dialog-no-close .ui-dialog-titlebar-close {display: none; }

Note: If you want to hide the entire title bar, use this CSS instead:

    .dialog-no-close .ui-dialog-titlebar {display: none; }

Alternatively, you can hide the close button in the dialog's initialization code:

    $("#selector").dialog({
        closeOnEscape: false,
        open: function(event, ui) {
            $(".ui-dialog-titlebar-close", $(this).parent()).hide();
        }
    });



