---
title: "Getting started with polymer"
slug: "getting-started-with-polymer"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic Element Structure
We got the following very basic element `my-element` saved as `src/my-element.html`

    <link rel="import" href="bower_components/polymer/polymer.html">
    
    <dom-module id="my-element">

      <template>
        <style>
          /* local styles go here */
          :host {
            display: block;
          }
        </style>
        <!-- local DOM goes here -->
        <content></content>
      </template>
    
      <script>
        Polymer({
          /* this is the element's prototype */
          is: 'my-element'
        });
      </script>

    </dom-module>

 - The `<link>` includes the Polymer library using an HTML import.
 - The `<dom-module>` is the local DOM wrapper for the element (in this case, `my-element`).
 - The `<template>` is the actual local DOM definition.
 - The `<style>` inside the `<template>` lets you define styles that are scoped to this element and its local DOM and will not affect anything else in the document.
 - The `<content>` will hold anything you place inside your element.
 - The `:host` pseudo class matches the custom element (`my-element`).
 - The `Polymer` call registers the element.
 - The `is` Property is the element's name (it **has** to match the `<dom-module>`'s `id`)

You can import it in your app using:

    <link rel="import" href="src/my-element.html">

And use it as a tag:

    <my-element>Content</my-element>




## Setting up your first polymer app from a Template
Let's set yourself up to build your own awesome Progressive Web App with Polymer!

Before you can start installing Polymer you require the following:
- Node.js - check out the [StackOverflow Installing Node.js Documentation][1]
- Bower - you can install Bower using the Node Package Manager installed with Node.js:

      npm install -g bower

# Installing the Polymer Command Line Interface
The Polymer CLI provides you with all tools needed for Polymer Projects:

    npm install -g polymer-cli

# Initialize your app from an app template
Use `polymer init` to initialize your app from an [app template][2]. 

A cool template is the `--app-drawer-template`. Let's use that:

    polymer init app-drawer-template

# Serve your app
There is no building needed to serve your first awesome Polymer app. Just `serve` it:

    polymer serve --open

This will open the app in your default browser on `http://localhost:8080`.

  [1]: https://www.wikiod.com/node-js/installing-nodejs
  [2]: https://www.polymer-project.org/1.0/toolbox/templates

## Hello World
This example creates a Polymer element named `x-foo`, whose template binds to a string property, named "message". The element's HTML is imported into the main document, which allows usage of `<x-foo>` tags in `<body>`.

*x-foo.html*

    <dom-module id="x-foo">
      <template>
        <span>{{message}}</span>
      </template>

      <script>
        Polymer({
          is: 'x-foo',
          properties : {
            message: {
              type: String,
              value: "Hello world!"
            }
          }
        });
      </script>
    </dom-module>

*index.html*

    <head>
      <!-- PolyGit used here as CDN for demo purposes only. In production,
           it's recommended to import Polymer and Web Components locally
           from Bower. -->
      <base href="https://polygit.org/polymer+1.6.0/components/">

      <script src="webcomponentsjs/webcomponents-lite.min.js"></script>
      <link rel="import" href="polymer/polymer.html">

      <link rel="import" href="x-foo.html">
    </head>
    <body>
      <x-foo></x-foo>
    </body>

See demo in [CodePen](http://codepen.io/tony19/pen/rLddOo)

## Using Elements from the Polymer Catalog
Polymer prodives a lot of well built elements for you to use in your app.

Browse them in their [Element Catalog][1].

Let's go through the workflow of using an element by including `paper-input` ([Documentation][2])

# Download the Element
To Download an element there are two ways:

## Bower
The convinient way is to use the command line using the bower `install` command:

`bower install --save PolymerElements/paper-input`

**Note:** `--save` adds the element as a dependency to the `bower.json` of your app.

## ZIP file
The other way is to add the selected element (`paper-input` in this case) to your collection (in the Polymer Catalog) using "Add to Collection" in the navigation and download your collection using the star icon in the upper right corner.

This will generate a .zip file that contains the element and all of its dependencies. You can then copy the `bower_components` folder inside the .zip/components to the root directory of your app.

# Import the Element in your app
To import the element you've just installed, import the corresponding `.html` file:

    <link rel="import" href="bower_components/paper-input/paper-input.html">

# Use the Element
Now you can use `paper-input` inside the document you imported it to:

    <paper-input></paper-input>

  [1]: https://elements.polymer-project.org
  [2]: https://elements.polymer-project.org/elements/paper-input

