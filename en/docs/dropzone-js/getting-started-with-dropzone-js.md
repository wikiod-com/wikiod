---
title: "Getting started with dropzone.js"
slug: "getting-started-with-dropzonejs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, World!
**Important:** Be sure to set up the server-side implementation as per the instructions [here][1], otherwise uploads will not work.

To get started, create a new HTML document. Download the script as per the "Installation" example, and include it in your head tag like so (remebering to replace the example path with the real path!):

`<script src="/path/to/dropzone.js"></script>`

Include a `<form>` in your `<body>`:

`<form action="/file-upload" class="dropzone" id="my-awesome-dropzone"></form>`

That's it! Here is a complete example:


    <html>
        <head>
            <title>My Awesome Website!</title>
            <script src="/path/to/dropzone.js"></script>
        </head>
        <body>
            <form action="/file-upload" class="dropzone" id="my-awesome-dropzone"></form>
        </body>
    </html>


For more complicated and specific setup, refer to the [ usage docs][2] on the official website.


  [1]: http://www.dropzonejs.com/#server-side-implementation
  [2]: http://www.dropzonejs.com/#usage

