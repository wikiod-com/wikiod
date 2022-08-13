---
title: "execCommand and contenteditable"
slug: "execcommand-and-contenteditable"
draft: false
images: []
weight: 9741
type: docs
toc: true
---

## Syntax
 - bool supported = document.execCommand(commandName, showDefaultUI, valueArgument)

## Parameters
| commandId | value |
| --------- | ----- | 
| **&vellip; Inline formatting commands** | &nbsp; |
| backColor | Color value String |
| bold | &nbsp; |
| createLink |  URL String |
| fontName | Font family name |
| fontSize | "1", "2", "3", "4", "5", "6", "7" |
| foreColor | Color value String |
| strikeThrough | &nbsp; |
| superscript | &nbsp; |
| unlink | &nbsp; |
| **&vellip; Block formatting commands** | &nbsp; |
| delete | &nbsp; |
| formatBlock | "address", "dd", "div", "dt", "h1", "h2", "h3", "h4", "h5", "h6", "p",  "pre" |
| forwardDelete | &nbsp; |
| insertHorizontalRule | &nbsp; |
| insertHTML  | HTML String |
| insertImage | URL String |
| insertLineBreak | &nbsp; |
| insertOrderedList | &nbsp; |
| insertParagraph | &nbsp; |
| insertText | Text string |
| insertUnorderedList | &nbsp; |
| justifyCenter | &nbsp; |
| justifyFull | &nbsp; |
| justifyLeft | &nbsp; |
| justifyRight | &nbsp; |
| outdent | &nbsp; |
| **&vellip; Clipboard commands** | &nbsp; |
| copy | Currently Selected String |
| cut | Currently Selected String |
| paste | &nbsp; |
| **&vellip; Miscellaneous commands** | &nbsp; |
| defaultParagraphSeparator | &nbsp; |
| redo | &nbsp; |
| selectAll | &nbsp; |
| styleWithCSS | &nbsp; |
| undo | &nbsp; |
| useCSS | &nbsp; |

## Listening to Changes of contenteditable
Events that work with most form elements (e.g., `change`, `keydown`, `keyup`, `keypress`) do not work with  `contenteditable`.

Instead, you can listen to changes of `contenteditable` contents with the `input` event. Assuming `contenteditableHtmlElement` is a JS DOM object that is `contenteditable`:

    contenteditableHtmlElement.addEventListener("input", function() {
        console.log("contenteditable element changed");
    });

## Getting started
The HTML attribute `contenteditable` provides a simple way to turn a HTML element into a user-editable area

    <div contenteditable>You can <b>edit</b> me!</div>

 **Native Rich-Text editing**

Using **JavaScript** and [`execCommand`<sup>W3C</sup>](https://w3c.github.io/editing/execCommand.html) you can additionally pass more editing features to the currently focused `contenteditable` element (specifically at the caret position or selection).

The `execCommand` function method accepts 3 arguments

    document.execCommand(commandId, showUI, value)

 - `commandId` String.  from the list of
   available ***commandId***s   
<sup>(see: ***Parameters**&rarr;commandId*)</sup>
 -  `showUI` Boolean (not implemented. Use
   `false`)   
 - `value` String If a command
   expects a command-related String ***value***, otherwise `""`. <sup>  
(see: ***Parameters**&rarr;value*)</sup>

Example using the `"bold"` **command** and `"formatBlock"` (where a **value** is expected):

    document.execCommand("bold", false, "");          // Make selected text bold
    document.execCommand("formatBlock", false, "H2"); // Make selected text Block-level <h2>

**Quick Start Example:**

    <button data-edit="bold"><b>B</b></button>
    <button data-edit="italic"><i>I</i></button>
    <button data-edit="formatBlock:p">P</button>
    <button data-edit="formatBlock:H1">H1</button>
    <button data-edit="insertUnorderedList">UL</button>
    <button data-edit="justifyLeft">&#8676;</button>
    <button data-edit="justifyRight">&#8677;</button>
    <button data-edit="removeFormat">&times;</button>

    <div contenteditable><p>Edit me!</p></div>

    <script>
    [].forEach.call(document.querySelectorAll("[data-edit]"), function(btn) {
      btn.addEventListener("click", edit, false);
    });
    
    function edit(event) {
      event.preventDefault();
      var cmd_val = this.dataset.edit.split(":");
      document.execCommand(cmd_val[0], false, cmd_val[1]);
    }
    <script>

[jsFiddle demo](https://jsfiddle.net/RokoCB/az7f38w7/)  
[Basic Rich-Text editor example (Modern browsers)](https://jsfiddle.net/RokoCB/yvshdr4q/)

**Final thoughts**  
Even being present for a long time (IE6), implementations and behaviors of `execCommand` vary from browser to browser making "building a Fully-featured and cross-browser compatible WYSIWYG editor" a hard task to any experienced JavaScript developer.  
Even if not yet fully standardized you can expect pretty decent results on the newer browsers like **Chrome, Firefox, Edge**. If you need *better* support for other browsers and more features like HTMLTable editing etc. a rule of thumbs is to look for an **already existent** and robust **Rich-Text** editor.



  [1]: https://jsfiddle.net/RokoCB/yvshdr4q/

## Formatting
Users can add formatting to `contenteditable` documents or elements using their browser's features, such as common keyboard shortcuts for formatting (<kbd>Ctrl-B</kbd> for **bold**, <kbd>Ctrl-I</kbd> for *italic*, etc.) or by dragging and dropping images, links, or markup from the clipboard.

Additionally, developers can use JavaScript to apply formatting to the current selection (highlighted text).

    document.execCommand('bold', false, null); // toggles bold formatting
    document.execCommand('italic', false, null); // toggles italic formatting
    document.execCommand('underline', false, null); // toggles underline

## Copy to clipboard from textarea using execCommand("copy")
Example:

    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <title></title>
    </head>
    <body>
        <textarea id="content"></textarea>
        <input type="button" id="copyID" value="Copy" />
        <script type="text/javascript">
            var button = document.getElementById("copyID"),
                input = document.getElementById("content");
    
            button.addEventListener("click", function(event) {
                event.preventDefault();
                input.select();
                document.execCommand("copy");
            });
        </script>
    </body>
    </html>

`document.execCommand("copy")` copies the current selection to the clipboard

