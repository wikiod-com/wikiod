---
title: "Handling static files"
slug: "handling-static-files"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
 1. To serve static files (Images, CSS, JS files, etc.) use the **express.static** middleware function.
 2. Pass the name of the directory that contains the assets to **express.static** to serve the files directly. (Look to the *Basic Example*)
 3. You can use multiple directories, simply call the **express.static** multiple times. Remember, Express looks up files in the order you set the directories with **express.static**. (Look to the *Multiple Directories Example*)
 4. You can create a virtual path prefix (i.e. one where the path does not actually exist in the file system) with **express.static**, just specify a mount path. (Look to the *Virtual Path Prefix Example*)
 5. All of the preceding paths have been relative to the directory from where you launch the *node* process. So, it is generally safer to use the absolute path of the directory you want to serve. (Look to the *Absolute Path to Static Files Directory Example*)
 6. You can mix and match the options of this method, as seen in the *Absolute Path to Directory & Virtual Path Prefix Example*

All of the examples can be run in node. Simply copy and paste into a node project with Express installed and run them with **node filename**. For an example of how to install express click [here][1] and ensure you have npm installed then follow the instructions on installing packages to install "express."


  [1]: https://www.wikiod.com/npm/getting-started-with-npm

## Absolute Path to Directory & Virtual Path Prefix Example
    // Set up Express
    var express = require('express');
    var app = express();
    
    /*  Serve from the absolute path of the directory that you want to serve with a
     */ virtual path prefix
    app.use('/static', express.static(__dirname + '/public'));

    // Start Express server
    app.listen(3030);

## Basic Example
    // Basic code for Express Instance
    var express = require('express');
    var app = express();

    // Serve static files from directory 'public'
    app.use(express.static('public'));

    // Start Express server
    app.listen(3030);

## Multiple Directories Example
    // Set up Express
    var express = require('express');
    var app = express();

    // Serve static assets from both 'public' and 'files' directory
    app.use(express.static('public');
    app.use(express.static('files');

    // Start Express server
    app.listen(3030);

## Virtual Path Prefix Example
    // Set up Express
    var express = require('express');
    var app = express();

    // Specify mount path, '/static', for the static directory
    app.use('/static', express.static('public'));

    // Start Express server
    app.listen(3030);

## Absolute Path to Static Files Directory Example
    // Set up Express
    var express = require('express');
    var app = express();    

    // Serve files from the absolute path of the directory
    app.use(express.static(__dirname + '/public'));

    // Start Express server
    app.listen(3030);

## Basic static files and favicon serve example
    var express = require('express');
    var path = require('path');
    var favicon = require('serve-favicon');
    
    var app = express();
    
    app.use(favicon(__dirname + '/public/img/favicon.ico'));
    app.use(express.static(path.join(__dirname, 'public')));
    
    app.listen(3000, function() {
      console.log("Express App listening on port 3000");
    })

