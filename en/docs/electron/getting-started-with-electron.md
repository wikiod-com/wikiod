---
title: "Getting started with electron"
slug: "getting-started-with-electron"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of Electron
# Dependencies

To install electron you must first install [Node.js](http://nodejs.org), which comes with [npm](http://npmjs.org).

# How to install it?

Use [npm](https://www.wikiod.com/npm):

    # Install the `electron` command globally in your $PATH
    npm install electron -g

    # OR

    # Install as a development dependency
    npm install electron --save-dev

## Hello World!
# Setup

An Electron project structure usually looks like this:

    hello-world-app/
    ├── package.json
    ├── index.js
    └── index.html

Now let's create the files and initialize our `package.json`.


    $ mkdir hello-world-app && cd hello-world-app
    $ touch index.js
    $ touch index.html
    $ npm init

__Note:__ If the `main` parameter is not specified in `package.json`, Electron will use `index.js` as the default entry point.

# The Main Process

In Electron, the process that runs `package.json`’s main script is called the __main process__. Here we can display a GUI by creating `BrowserWindow` instances.

Add the following to `index.js`:

<!-- language: lang-javascript -->

    const { app, BrowserWindow } = require('electron')

    // Global reference to the window object
    let win

    // This method will be called when Electron has finished
    // initialization and is ready to create browser windows
    app.on('ready', function(){
        // Create the window
        win = new BrowserWindow({width: 800, height: 600})

        // Open and load index.html to the window
        win.loadURL('file://' + __dirname + '/index.html')

        // Emitted when the window is closed.
        win.on('closed', () => {
            // Dereference the window object
            win = null
        });
    })

    // Quit the app if all windows are closed
    app.on('window-all-closed', () => {
        app.quit()
    })

# HTML Template & Renderer Process

Next we create the GUI for the app. Electron uses web pages as its GUI, each running in their own process called the __renderer process__.

Add the following code to `index.html`:

<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
    <head>
        <title>Hello World</title>
    </head>
    <body>
        <h1>Hello World!</h1>
    </body>
    </html>

# Running the App

There are multiple ways to run an Electron App.

## With `electron-prebuilt` installed Globally ##

First, make sure you have [`electron-prebuilt` installed](https://www.wikiod.com/electron/getting-started-with-electron#Installation of Electron).

Now we can test the app using this command:

    $ electron .

## Method 2 - Without `electron-prebuilt` installed Globally ##

First, we'll have to enter your app's folder (the folder where package.json is).

There, open up a Terminal/Command Prompt window and type `npm install` to install the necessary into that app's folder.

Afterwards, key in `npm start` to run the app. Keep in mind that your `package.json` still has to specify a 'start' script.

If everything worked correctly, you should see something like this:

[![enter image description here][1]][1]

Congratulations! You've successfully created your first Electron app.


  [1]: http://i.stack.imgur.com/5Pl2v.png

