---
title: "Getting started with ckeditor"
slug: "getting-started-with-ckeditor"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting Started
Create a file `ckeditor.html` with the following content:

    <!DOCTYPE html>
    <html>
    <head>
        <title>CKEditor Demo!</title>
    </head>
    <body>
        <script src="//cdn.ckeditor.com/4.6.1/basic/ckeditor.js"></script>
        <div id="editor1">
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. In est ipsum, elementum id ipsum vel, aliquam lobortis ligula. Nam vel purus eget nulla bibendum interdum at non orci. Nulla facilisi. Vivamus aliquet sapien risus. Mauris molestie efficitur pharetra. Aliquam erat volutpat. Fusce ac leo pretium, ornare libero et, tincidunt erat. Nunc tempus tortor eget ex ultricies, a cursus nibh fringilla.<br /><br />
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. In est ipsum, elementum id ipsum vel, aliquam lobortis ligula. Nam vel purus eget nulla bibendum interdum at non orci. Nulla facilisi. Vivamus aliquet sapien risus. Mauris molestie efficitur pharetra. Aliquam erat volutpat. Fusce ac leo pretium, ornare libero et, tincidunt erat. Nunc tempus tortor eget ex ultricies, a cursus nibh fringilla.<br /><br />
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. In est ipsum, elementum id ipsum vel, aliquam lobortis ligula. Nam vel purus eget nulla bibendum interdum at non orci. Nulla facilisi. Vivamus aliquet sapien risus. Mauris molestie efficitur pharetra. Aliquam erat volutpat. Fusce ac leo pretium, ornare libero et, tincidunt erat. Nunc tempus tortor eget ex ultricies, a cursus nibh fringilla.<br /><br />
        </div>
        
        <script>
        CKEDITOR.replace( 'editor1' );
        </script>
    </body>
    </html>

[Live Demo on JSFiddle](https://jsfiddle.net/dekelb/m8muLxmo/)


Explanation of code
-------------------

1. Loads the CKEditor library from the CKEditor [CDN][1]:
      
           <script src="https://cdnjs.cloudflare.com/ajax/libs/ckeditor/4.6.1/ckeditor.js"></script>


2. Create a new DIV element with all of the content that we want inside the Editor

       <div id="editor1">
       ALL CONTENT HERE
        </div>


3. Tell the `CKEDITOR` object to replace the element with the id `editor1` with a WYSIWYG editor (the CKEditor).

        <script>
        CKEDITOR.replace( 'editor1' );
        </script>
  
For more refer to the [CKEditor Documentation][2] page.

  [1]: https://en.wikipedia.org/wiki/Content_delivery_network
  [2]: http://docs.ckeditor.com/

## CKEditor - Inline Editor Example
Create a file `ckeditor-inline.html` with the following content:

    <!DOCTYPE html>
    <html>
    <head>
        <title>CKEditor Inline Demo!</title>
    </head>
    <body>
        <script src="//cdn.ckeditor.com/4.6.1/basic/ckeditor.js"></script>
        <div id="editor1" contenteditable="true">
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. In est ipsum, elementum id ipsum vel, aliquam lobortis ligula. Nam vel purus eget nulla bibendum interdum at non orci. Nulla facilisi. Vivamus aliquet sapien risus. Mauris molestie efficitur pharetra. Aliquam erat volutpat. Fusce ac leo pretium, ornare libero et, tincidunt erat. Nunc tempus tortor eget ex ultricies, a cursus nibh fringilla.<br /><br />
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. In est ipsum, elementum id ipsum vel, aliquam lobortis ligula. Nam vel purus eget nulla bibendum interdum at non orci. Nulla facilisi. Vivamus aliquet sapien risus. Mauris molestie efficitur pharetra. Aliquam erat volutpat. Fusce ac leo pretium, ornare libero et, tincidunt erat. Nunc tempus tortor eget ex ultricies, a cursus nibh fringilla.<br /><br />
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. In est ipsum, elementum id ipsum vel, aliquam lobortis ligula. Nam vel purus eget nulla bibendum interdum at non orci. Nulla facilisi. Vivamus aliquet sapien risus. Mauris molestie efficitur pharetra. Aliquam erat volutpat. Fusce ac leo pretium, ornare libero et, tincidunt erat. Nunc tempus tortor eget ex ultricies, a cursus nibh fringilla.<br /><br />
        </div>
    </body>
    </html>

[Live Demo on JSFiddle](https://jsfiddle.net/dekelb/1f325zwm/1/)


Explanation of code
-------------------

1. Loads the CKEditor library from the CKEditor CDN:
      
           <script src="https://cdnjs.cloudflare.com/ajax/libs/ckeditor/4.6.1/ckeditor.js"></script>


2. Create a new DIV element with all of the content that we want inside the Editor

       <div id="editor1" contenteditable="true">
       ALL CONTENT HERE
        </div>

> The important thing to note here is the `contenteditable="true"` inside the `DIV` element. The CKEditor will automatically replace every element that has `contenteditable` attribute with an **inline** editor.
  
For more refer to the [Documentation][1].

  [1]: http://docs.ckeditor.com/#!/guide/dev_savedata

## Get the HTML content of CKEditor
Create a file `ckeditor-content.html` with the following content:

    <!DOCTYPE html>
    <html>
    <head>
        <title>CKEditor Get Content Demo!</title>
    </head>
    <body>
        <script src="//cdn.ckeditor.com/4.6.1/basic/ckeditor.js"></script>
        <div id="editor1" contenteditable="true">
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. In est ipsum, elementum id ipsum vel, aliquam lobortis ligula. Nam vel purus eget nulla bibendum interdum at non orci. Nulla facilisi. Vivamus aliquet sapien risus. Mauris molestie efficitur pharetra. Aliquam erat volutpat. Fusce ac leo pretium, ornare libero et, tincidunt erat. Nunc tempus tortor eget ex ultricies, a cursus nibh fringilla.<br /><br />
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. In est ipsum, elementum id ipsum vel, aliquam lobortis ligula. Nam vel purus eget nulla bibendum interdum at non orci. Nulla facilisi. Vivamus aliquet sapien risus. Mauris molestie efficitur pharetra. Aliquam erat volutpat. Fusce ac leo pretium, ornare libero et, tincidunt erat. Nunc tempus tortor eget ex ultricies, a cursus nibh fringilla.<br /><br />
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. In est ipsum, elementum id ipsum vel, aliquam lobortis ligula. Nam vel purus eget nulla bibendum interdum at non orci. Nulla facilisi. Vivamus aliquet sapien risus. Mauris molestie efficitur pharetra. Aliquam erat volutpat. Fusce ac leo pretium, ornare libero et, tincidunt erat. Nunc tempus tortor eget ex ultricies, a cursus nibh fringilla.<br /><br />
        </div>
        <button id="btn1">Click to get the content</button>
        <script>
            document.getElementById('btn1').addEventListener('click', function() {
                content = CKEDITOR.instances.editor1.getData()
                console.log(content);
            }, false);
        </script>
    </body>
    </html>

[Live Demo on JSFiddle](https://jsfiddle.net/dekelb/nhxctow9/)


Explanation of code
-------------------

1. Loads the CKEditor library from the CKEditor CDN:
      
           <script src="https://cdnjs.cloudflare.com/ajax/libs/ckeditor/4.6.1/ckeditor.js"></script>


2. Create a new DIV element with all of the content that we want inside the Editor

       <div id="editor1" contenteditable="true">
       ALL CONTENT HERE
        </div>

3. Add a `click` listener to the button we have, and once clicked - get the HTML content of the ckeditor.

        <script>
            document.getElementById('btn1').addEventListener('click', function() {
                content = CKEDITOR.instances.editor1.getData()
                console.log(content);
            }, false);
        </script>

> 1. The name of the editor `editor1` in `CKEDITOR.instances.editor1` is the value of the `id` attribute of the element we used (`<div id="editor1" ... >`)
> 2. Note the usage of `console.log` - the HTML content of the editor will be available in the console (you can open the console by clicking the <kbd>F12</kbd> key)
  
For more refer to the [Inline Documentation][1] page.

  [1]: http://docs.ckeditor.com/#!/guide/dev_inline

